open! Core
open! Async

type t =
  { events : Event.t Ids.Event_id.Table.t
  ; decisions : Decision.t Ids.Event_id.Table.t
  ; alerts : Alert.t Ids.Alert_id.Table.t
  ; alert_events : Alert.Event.t list Ids.Alert_id.Table.t (** newest first *)
  ; customers : Profile.Customer.snapshot Ids.Customer_id.Table.t
  ; counterparties : Profile.Counterparty.snapshot Ids.Counterparty_id.Table.t
  ; mutable models : Store.Model_version.t list
  ; order : int Ids.Event_id.Table.t (** commit order, for histories *)
  ; revocations : Store.Revocation.t Ids.Event_id.Table.t
  }

let create () =
  { events = Ids.Event_id.Table.create ()
  ; decisions = Ids.Event_id.Table.create ()
  ; alerts = Ids.Alert_id.Table.create ()
  ; alert_events = Ids.Alert_id.Table.create ()
  ; customers = Ids.Customer_id.Table.create ()
  ; counterparties = Ids.Counterparty_id.Table.create ()
  ; models = []
  ; order = Ids.Event_id.Table.create ()
  ; revocations = Ids.Event_id.Table.create ()
  }
;;

let kind _ = "memory"
let find_decision t id = return (Ok (Hashtbl.find t.decisions id))

(* Snapshots are copied in and out, like a round trip through the database. *)
let copy_customer s = Profile.Customer.(snapshot (of_snapshot s))
let copy_counterparty s = Profile.Counterparty.(snapshot (of_snapshot s))

let commit t (c : Store.Commit.t) =
  match Hashtbl.find t.decisions c.event.id with
  | Some d -> return (Ok (Store.Commit_result.Duplicate d))
  | None ->
    Hashtbl.set t.events ~key:c.event.id ~data:c.event;
    Hashtbl.set t.order ~key:c.event.id ~data:(Hashtbl.length t.order);
    Hashtbl.set t.decisions ~key:c.event.id ~data:c.decision;
    Option.iter c.alert ~f:(fun (alert, events) ->
      if not (Hashtbl.mem t.alerts alert.id)
      then (
        Hashtbl.set t.alerts ~key:alert.id ~data:alert;
        Hashtbl.set t.alert_events ~key:alert.id ~data:(List.rev events)));
    let newer stored_events incoming_events = incoming_events > stored_events in
    let customer = Profile.Customer.(of_snapshot c.customer) in
    let id = Profile.Customer.customer customer in
    (match Hashtbl.find t.customers id with
     | Some old
       when not
              (newer
                 (Profile.Customer.events (Profile.Customer.of_snapshot old))
                 (Profile.Customer.events customer)) -> ()
     | Some _ | None -> Hashtbl.set t.customers ~key:id ~data:(copy_customer c.customer));
    let cp = Profile.Counterparty.of_snapshot c.counterparty in
    let cp_id = Profile.Counterparty.counterparty cp in
    (match Hashtbl.find t.counterparties cp_id with
     | Some old
       when not
              (newer
                 (Profile.Counterparty.events (Profile.Counterparty.of_snapshot old))
                 (Profile.Counterparty.events cp)) -> ()
     | Some _ | None ->
       Hashtbl.set t.counterparties ~key:cp_id ~data:(copy_counterparty c.counterparty));
    return (Ok Store.Commit_result.Committed)
;;

let load_customer t id =
  return (Ok (Option.map (Hashtbl.find t.customers id) ~f:copy_customer))
;;

let load_counterparty t id =
  return (Ok (Option.map (Hashtbl.find t.counterparties id) ~f:copy_counterparty))
;;

let find_alert t id = return (Ok (Hashtbl.find t.alerts id))

let update_alert t (alert : Alert.t) events =
  let expected = alert.version - List.length events in
  match Hashtbl.find t.alerts alert.id with
  | Some stored when stored.version = expected ->
    Hashtbl.set t.alerts ~key:alert.id ~data:alert;
    Hashtbl.update t.alert_events alert.id ~f:(fun old ->
      List.rev_append events (Option.value old ~default:[]));
    return (Ok Store.Alert_update.Updated)
  | stored -> return (Ok (Store.Alert_update.Conflict stored))
;;

let alerts ?status ?customer t =
  Hashtbl.data t.alerts
  |> List.filter ~f:(fun (a : Alert.t) ->
    Option.for_all status ~f:(Alert.Status.equal a.status)
    && Option.for_all customer ~f:(Ids.Customer_id.equal a.customer))
  |> List.sort ~compare:(fun (a : Alert.t) b ->
    match Time_ns.compare a.opened_at b.opened_at with
    | 0 -> String.compare (Ids.Alert_id.to_string a.id) (Ids.Alert_id.to_string b.id)
    | c -> c)
  |> Or_error.return
  |> return
;;

let alert_events t id =
  return (Ok (Hashtbl.find t.alert_events id |> Option.value ~default:[] |> List.rev))
;;

let decisions ?customer t =
  Hashtbl.data t.decisions
  |> List.filter ~f:(fun (d : Decision.t) ->
    Option.for_all customer ~f:(Ids.Customer_id.equal d.customer))
  |> List.sort ~compare:(fun (a : Decision.t) b ->
    match Time_ns.compare a.decided_at b.decided_at with
    | 0 ->
      String.compare
        (Ids.Event_id.to_string a.event_id)
        (Ids.Event_id.to_string b.event_id)
    | c -> c)
  |> Or_error.return
  |> return
;;

let record_model t (m : Store.Model_version.t) =
  if not (List.exists t.models ~f:(fun x -> String.equal x.version m.version))
  then t.models <- m :: t.models;
  return (Ok ())
;;

let current_model t =
  List.max_elt t.models ~compare:(fun (a : Store.Model_version.t) b ->
    match Time_ns.compare a.activated_at b.activated_at with
    | 0 -> String.compare a.version b.version
    | c -> c)
  |> Or_error.return
  |> return
;;

let find_event t id =
  return
    (Ok
       (Option.map (Hashtbl.find t.events id) ~f:(fun e ->
          e, Hashtbl.find t.revocations id)))
;;

let history t ~f =
  Hashtbl.data t.events
  |> List.filter ~f:(fun (e : Event.t) -> f e && not (Hashtbl.mem t.revocations e.id))
  |> List.sort
       ~compare:
         (Comparable.lift Int.compare ~f:(fun (e : Event.t) ->
            Hashtbl.find_exn t.order e.id))
  |> Or_error.return
  |> return
;;

let customer_history t id = history t ~f:(fun e -> Ids.Customer_id.equal e.customer id)

let counterparty_history t id =
  history t ~f:(fun e -> Ids.Counterparty_id.equal e.counterparty id)
;;

let revoke t (r : Store.Revocation.t) ~customer ~counterparty =
  match Hashtbl.find t.events r.event_id, Hashtbl.find t.revocations r.event_id with
  | None, _ -> return (Ok Store.Revoke_result.Unknown_event)
  | Some _, Some stored -> return (Ok (Store.Revoke_result.Already_revoked stored))
  | Some _, None ->
    Hashtbl.set t.revocations ~key:r.event_id ~data:r;
    let c = Profile.Customer.of_snapshot customer in
    Hashtbl.set
      t.customers
      ~key:(Profile.Customer.customer c)
      ~data:(copy_customer customer);
    let cp = Profile.Counterparty.of_snapshot counterparty in
    Hashtbl.set
      t.counterparties
      ~key:(Profile.Counterparty.counterparty cp)
      ~data:(copy_counterparty counterparty);
    return (Ok Store.Revoke_result.Revoked)
;;

let close _ = return ()

let store t =
  Store.T
    ( (module struct
        type nonrec t = t

        let kind = kind
        let find_decision = find_decision
        let commit = commit
        let load_customer = load_customer
        let load_counterparty = load_counterparty
        let find_alert = find_alert
        let update_alert = update_alert
        let alerts = alerts
        let alert_events = alert_events
        let decisions = decisions
        let record_model = record_model
        let current_model = current_model
        let find_event = find_event
        let customer_history = customer_history
        let counterparty_history = counterparty_history
        let revoke = revoke
        let close = close
      end)
    , t )
;;
