open! Core
open! Async
open Transaction_monitoring
open Fixtures

let ok = Or_error.ok_exn

let run (Store.T ((module S), store)) =
  let config = Rules.Config.default in
  let evaluator = Evaluator.create config |> ok in
  let profiles = Evaluator.Profiles.create config.profile in
  (* Build a commit the way the monitor does: decide, apply, snapshot. *)
  let commit_of (e : Event.t) : Store.Commit.t =
    let customer = Evaluator.Profiles.customer profiles e.customer in
    let counterparty = Evaluator.Profiles.counterparty profiles e.counterparty in
    let decision = Evaluator.decide evaluator (Evaluator.context e ~customer ~counterparty) in
    Evaluator.apply e ~customer ~counterparty;
    { event = e
    ; decision
    ; alert = Alert.open_ decision
    ; customer = Profile.Customer.snapshot customer
    ; counterparty = Profile.Counterparty.snapshot counterparty
    }
  in
  let e1 = event ~id:"e1" ~direction:Inbound ~counterparty:"employer" ~at:(at_min 0.) 300_000 in
  let e2 = event ~id:"e2" ~counterparty:"landlord" ~at:(at_min 10.) 90_000 in
  let e3 = event ~id:"e3" ~counterparty:"far" ~counterparty_country:"IR" ~at:(at_min 20.) 50_000 in
  let e4 = event ~id:"e4" ~customer:"cust-2" ~counterparty:"landlord" ~at:(at_min 30.) 70_000 in
  let commits = List.map [ e1; e2; e3; e4 ] ~f:commit_of in
  let%bind () =
    Deferred.List.iter ~how:`Sequential commits ~f:(fun c ->
      let%map r = S.commit store c >>| ok in
      print_s [%message "commit" ~event:(c.event.id : Ids.Event_id.t) (r : Store.Commit_result.t)])
  in
  (* a redelivery of e3 with a different body changes nothing *)
  let again = List.nth_exn commits 2 in
  let%bind r =
    S.commit store { again with decision = { again.decision with outcome = Allow } } >>| ok
  in
  print_s [%message "recommit e3" ~outcome:(match r with Duplicate d -> Decision.Outcome.to_string d.outcome | Committed -> "committed")];
  let%bind d = S.find_decision store (Ids.Event_id.of_string "e2") >>| ok in
  print_s [%message "find e2" ~outcome:(Option.map d ~f:(fun d -> d.outcome) : Decision.Outcome.t option)];
  let%bind d = S.find_decision store (Ids.Event_id.of_string "nope") >>| ok in
  print_s [%message "find nope" ~found:(Option.is_some d : bool)];
  let show_decisions label ds =
    print_s
      [%message
        label
          ~_:(List.map ds ~f:(fun (d : Decision.t) -> Ids.Event_id.to_string d.event_id, Decision.Outcome.to_string d.outcome) : (string * string) list)]
  in
  let%bind ds = S.decisions store >>| ok in
  show_decisions "decisions" ds;
  let%bind ds = S.decisions ~customer:(Ids.Customer_id.of_string "cust-2") store >>| ok in
  show_decisions "decisions of cust-2" ds;
  (* profiles: the latest snapshot; an older one does not replace it *)
  let cust1 = Ids.Customer_id.of_string "cust-1" in
  let%bind p = S.load_customer store cust1 >>| ok in
  let events_of p = Option.map p ~f:(fun s -> Profile.Customer.events (Profile.Customer.of_snapshot s)) in
  print_s [%message "cust-1 profile events" ~_:(events_of p : int option)];
  let stale = { (List.hd_exn commits) with event = event ~id:"e5" ~at:(at_min 40.) 1_000 } in
  let%bind (_ : Store.Commit_result.t) =
    S.commit store { stale with decision = { stale.decision with event_id = Ids.Event_id.of_string "e5"; outcome = Allow }; alert = None }
    >>| ok
  in
  let%bind p = S.load_customer store cust1 >>| ok in
  print_s [%message "after a stale snapshot" ~_:(events_of p : int option)];
  let%bind cp = S.load_counterparty store (Ids.Counterparty_id.of_string "landlord") >>| ok in
  print_s
    [%message
      "landlord profile events"
        ~_:(Option.map cp ~f:(fun s -> Profile.Counterparty.events (Profile.Counterparty.of_snapshot s)) : int option)];
  let%bind missing = S.load_customer store (Ids.Customer_id.of_string "nobody") >>| ok in
  print_s [%message "unknown customer" ~found:(Option.is_some missing : bool)];
  (* alerts *)
  let show_alerts label alerts =
    print_s
      [%message
        label
          ~_:(List.map alerts ~f:(fun (a : Alert.t) -> Ids.Alert_id.to_string a.id, Alert.Status.to_string a.status, a.version) : (string * string * int) list)]
  in
  let%bind alerts = S.alerts store >>| ok in
  show_alerts "alerts" alerts;
  let alert = List.hd_exn alerts in
  let assigned, events =
    Alert.apply alert (Assign { analyst = Ids.Analyst_id.of_string "ana"; at = at_min 50. }) |> ok
  in
  let%bind r = S.update_alert store assigned events >>| ok in
  print_s [%message "assign" (r : Store.Alert_update.t)];
  let%bind r = S.update_alert store assigned events >>| ok in
  print_s [%message "assign again from the old version" ~conflict:(match r with Conflict (Some a) -> Some a.version | Conflict None -> None | Updated -> Some (-1) : int option)];
  let%bind r =
    S.update_alert store { assigned with id = Ids.Alert_id.of_string "alert-missing" } events >>| ok
  in
  print_s [%message "unknown alert" (r : Store.Alert_update.t)];
  let%bind history = S.alert_events store alert.id >>| ok in
  print_s [%message (history : Alert.Event.t list)];
  let%bind open_ = S.alerts ~status:Open store >>| ok in
  show_alerts "open" open_;
  let%bind mine = S.alerts ~status:Assigned ~customer:cust1 store >>| ok in
  show_alerts "assigned to cust-1" mine;
  let%bind found = S.find_alert store alert.id >>| ok in
  print_s [%message "stored" ~equal:(Option.equal Alert.equal found (Some assigned) : bool)];
  (* models *)
  let m name version minute =
    { Store.Model_version.name; version; policy = "shadow"; activated_at = at_min minute }
  in
  let%bind none = S.current_model store >>| ok in
  print_s [%message "no model" ~_:(none : Store.Model_version.t option)];
  let%bind () = S.record_model store (m "tm" "v1" 0.) >>| ok in
  let%bind () = S.record_model store (m "tm" "v2" 60.) >>| ok in
  let%bind () = S.record_model store (m "tm" "v1" 120.) >>| ok in
  let%map current = S.current_model store >>| ok in
  print_s [%message "current" ~_:(current : Store.Model_version.t option)]
;;
