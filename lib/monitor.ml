open! Core
open! Async

module Reply = struct
  type t =
    | Decided of Decision.t
    | Already_decided of Decision.t
    | Overloaded
  [@@deriving sexp_of]

  let decision = function
    | Decided d | Already_decided d -> Some d
    | Overloaded -> None
  ;;
end

module Revoke_reply = struct
  type t =
    | Revoked of
        { customer_events : int
        ; counterparty_events : int
        }
    | Already_revoked of Store.Revocation.t
    | Unknown_event
  [@@deriving sexp_of]
end

(* A small LRU over Hash_queue: lookups move the key to the back, inserts evict from the
   front. *)
module Lru (Q : Hash_queue.S) = struct
  type 'v t =
    { queue : 'v Q.t
    ; capacity : int
    }

  let create capacity = { queue = Q.create (); capacity }
  let find t k = Q.lookup_and_move_to_back t.queue k

  let set t k v =
    ignore (Q.remove t.queue k : [ `Ok | `No_such_key ]);
    Q.enqueue_back_exn t.queue k v;
    while Q.length t.queue > t.capacity do
      ignore (Q.dequeue_front t.queue : _ option)
    done
  ;;

  let remove t k = ignore (Q.remove t.queue k : [ `Ok | `No_such_key ])
end

module Customers = Lru (Ids.Customer_id.Hash_queue)
module Counterparties = Lru (Ids.Counterparty_id.Hash_queue)

type t =
  { evaluator : Evaluator.t
  ; store : Store.t
  ; sequencer : unit Throttle.Sequencer.t
  ; max_pending : int
  ; mutable pending : int
  ; on_decision : Decision.t -> unit Deferred.t
  ; customers : Profile.Customer.t Customers.t
  ; counterparties : Profile.Counterparty.t Counterparties.t
  }

let store t = t.store
let evaluator t = t.evaluator
let pending t = t.pending

let create
  ?(cache_capacity = 100_000)
  ?(max_pending = 10_000)
  ?(on_decision = fun _ -> return ())
  ~now
  ~store
  evaluator
  =
  let open Deferred.Or_error.Let_syntax in
  let (Store.T ((module S), s)) = store in
  let%map () =
    match Evaluator.model evaluator with
    | None -> return ()
    | Some m ->
      S.record_model
        s
        { name = Model.name m
        ; version = Model.version m
        ; policy = (Evaluator.policy evaluator).name
        ; activated_at = now
        }
  in
  { evaluator
  ; store
  ; sequencer = Throttle.Sequencer.create ~continue_on_error:true ()
  ; max_pending
  ; pending = 0
  ; on_decision
  ; customers = Customers.create cache_capacity
  ; counterparties = Counterparties.create cache_capacity
  }
;;

let customer t id =
  match Customers.find t.customers id with
  | Some p -> return (Ok p)
  | None ->
    let (Store.T ((module S), s)) = t.store in
    S.load_customer s id
    >>|? fun snapshot ->
    let p =
      match snapshot with
      | Some snap
        when Profile.Params.equal
               (Profile.Customer.params (Profile.Customer.of_snapshot snap))
               (Evaluator.config t.evaluator).profile -> Profile.Customer.of_snapshot snap
      | Some _ | None ->
        (* A snapshot built with other parameters counts different things; start over
           rather than mix them. *)
        Profile.Customer.create (Evaluator.config t.evaluator).profile id
    in
    Customers.set t.customers id p;
    p
;;

let counterparty t id =
  match Counterparties.find t.counterparties id with
  | Some p -> return (Ok p)
  | None ->
    let (Store.T ((module S), s)) = t.store in
    S.load_counterparty s id
    >>|? fun snapshot ->
    let p =
      match snapshot with
      | Some snap -> Profile.Counterparty.of_snapshot snap
      | None -> Profile.Counterparty.create id
    in
    Counterparties.set t.counterparties id p;
    p
;;

let evict t (e : Event.t) =
  Customers.remove t.customers e.customer;
  Counterparties.remove t.counterparties e.counterparty
;;

let process t (e : Event.t) =
  let open Deferred.Or_error.Let_syntax in
  let (Store.T ((module S), s)) = t.store in
  match%bind S.find_decision s e.id with
  | Some d -> return (Reply.Already_decided d)
  | None ->
    let%bind customer = customer t e.customer in
    let%bind counterparty = counterparty t e.counterparty in
    let decision =
      Evaluator.decide t.evaluator (Evaluator.context e ~customer ~counterparty)
    in
    Evaluator.apply e ~customer ~counterparty;
    let commit =
      { Store.Commit.event = e
      ; decision
      ; alert = Alert.open_ decision
      ; customer = Profile.Customer.snapshot customer
      ; counterparty = Profile.Counterparty.snapshot counterparty
      }
    in
    (match%bind.Deferred S.commit s commit with
     | Ok Committed ->
       let%bind.Deferred () = t.on_decision decision in
       return (Reply.Decided decision)
     | Ok (Duplicate d) ->
       (* another instance decided this event first; its profiles are in the store *)
       evict t e;
       return (Reply.Already_decided d)
     | Error _ as error ->
       evict t e;
       Deferred.return error)
;;

let on_event t (e : Event.t) =
  if t.pending >= t.max_pending
  then return (Ok Reply.Overloaded)
  else (
    t.pending <- t.pending + 1;
    (* A raising job would close the sequencer for good: turn exceptions into errors
       inside the job. *)
    Throttle.enqueue t.sequencer (fun () ->
      Monitor.try_with_or_error ~rest:`Log (fun () -> process t e) >>| Or_error.join)
    >>| fun result ->
    t.pending <- t.pending - 1;
    result)
;;

(* Profiles cannot subtract an event (quantile sketches and bounded tables only move
   forward), so revoking rebuilds both profiles from the stored history without it. That
   is exact only if the store holds every event the profile saw: a profile warmed outside
   the monitor (or by a snapshot from elsewhere) is refused rather than rebuilt wrong. *)
let rebuild ~what ~current ~history ~(revoked : Event.t) ~create ~update =
  if current <> List.length history
  then
    Or_error.error_s
      [%message
        "cannot revoke: the store does not hold the whole history of the profile"
          what
          ~profile_events:(current : int)
          ~stored_events:(List.length history : int)]
  else (
    let p = create () in
    List.iter history ~f:(fun (e : Event.t) ->
      if not (Ids.Event_id.equal e.id revoked.id) then update p e);
    Ok p)
;;

let process_revoke t id ~reason ~at =
  let open Deferred.Or_error.Let_syntax in
  let (Store.T ((module S), s)) = t.store in
  match%bind S.find_event s id with
  | None -> return Revoke_reply.Unknown_event
  | Some (_, Some stored) -> return (Revoke_reply.Already_revoked stored)
  | Some (e, None) ->
    let%bind customer = customer t e.customer in
    let%bind counterparty = counterparty t e.counterparty in
    let%bind customer_history = S.customer_history s e.customer in
    let%bind counterparty_history = S.counterparty_history s e.counterparty in
    let%bind rebuilt_customer =
      Deferred.return
        (rebuild
           ~what:(Ids.Customer_id.to_string e.customer)
           ~current:(Profile.Customer.events customer)
           ~history:customer_history
           ~revoked:e
           ~create:(fun () ->
             Profile.Customer.create (Evaluator.config t.evaluator).profile e.customer)
           ~update:Profile.Customer.update)
    in
    let%bind rebuilt_counterparty =
      Deferred.return
        (rebuild
           ~what:(Ids.Counterparty_id.to_string e.counterparty)
           ~current:(Profile.Counterparty.events counterparty)
           ~history:counterparty_history
           ~revoked:e
           ~create:(fun () -> Profile.Counterparty.create e.counterparty)
           ~update:Profile.Counterparty.update)
    in
    (match%bind.Deferred
       S.revoke
         s
         { event_id = id; reason; revoked_at = at }
         ~customer:(Profile.Customer.snapshot rebuilt_customer)
         ~counterparty:(Profile.Counterparty.snapshot rebuilt_counterparty)
     with
     | Ok Revoked ->
       Customers.set t.customers e.customer rebuilt_customer;
       Counterparties.set t.counterparties e.counterparty rebuilt_counterparty;
       return
         (Revoke_reply.Revoked
            { customer_events = Profile.Customer.events rebuilt_customer
            ; counterparty_events = Profile.Counterparty.events rebuilt_counterparty
            })
     | Ok (Already_revoked stored) ->
       evict t e;
       return (Revoke_reply.Already_revoked stored)
     | Ok Unknown_event ->
       evict t e;
       return Revoke_reply.Unknown_event
     | Error _ as error ->
       evict t e;
       Deferred.return error)
;;

let revoke t id ~reason ~at =
  (* Through the same sequencer as [on_event], so no event of these profiles is half
     processed while they are rebuilt. *)
  Throttle.enqueue t.sequencer (fun () ->
    Monitor.try_with_or_error ~rest:`Log (fun () -> process_revoke t id ~reason ~at)
    >>| Or_error.join)
;;

let apply_alert t id command =
  let open Deferred.Or_error.Let_syntax in
  let (Store.T ((module S), s)) = t.store in
  let rec attempt n =
    match%bind S.find_alert s id with
    | None -> Deferred.Or_error.error_s [%message "no such alert" (id : Ids.Alert_id.t)]
    | Some alert ->
      let%bind updated, events = Deferred.return (Alert.apply alert command) in
      if List.is_empty events
      then return (updated, events)
      else (
        match%bind S.update_alert s updated events with
        | Updated -> return (updated, events)
        | Conflict _ when n > 0 -> attempt (n - 1)
        | Conflict _ ->
          Deferred.Or_error.error_s
            [%message "alert changed concurrently, try again" (id : Ids.Alert_id.t)])
  in
  attempt 1
;;

let review_queue ?status t =
  let (Store.T ((module S), s)) = t.store in
  match status with
  | Some status -> S.alerts ~status s
  | None ->
    let open Deferred.Or_error.Let_syntax in
    let%map all = S.alerts s in
    List.filter all ~f:(fun a ->
      match a.status with
      | Open | Assigned -> true
      | Closed -> false)
;;
