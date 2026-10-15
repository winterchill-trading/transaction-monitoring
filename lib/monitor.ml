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

(* A small LRU over Hash_queue: lookups move the key to the back, inserts evict from
   the front. *)
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
