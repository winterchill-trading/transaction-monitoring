open! Core
open! Async
open Transaction_monitoring
open Fixtures

let ok = Or_error.ok_exn
let evaluator () = Evaluator.create Rules.Config.default |> ok

let new_monitor ?max_pending ?on_decision ?cache_capacity store =
  Monitor.create ?max_pending ?on_decision ?cache_capacity ~now:t0 ~store (evaluator ())
  >>| ok
;;

let show_reply r =
  match (r : Monitor.Reply.t) with
  | Decided d -> sprintf "decided %s" (Decision.Outcome.to_string d.outcome)
  | Already_decided d -> sprintf "already decided %s" (Decision.Outcome.to_string d.outcome)
  | Overloaded -> "overloaded"
;;

let%expect_test "idempotent per event id; alerts reach the review queue" =
  let store = Store_memory.store (Store_memory.create ()) in
  let seen = ref [] in
  let%bind m = new_monitor ~on_decision:(fun d -> seen := d.event_id :: !seen; return ()) store in
  let e = event ~id:"pay-1" ~counterparty_country:"IR" ~at:t0 50_000 in
  let%bind r1 = Monitor.on_event m e >>| ok in
  let%bind r2 = Monitor.on_event m e >>| ok in
  print_endline (show_reply r1);
  print_endline (show_reply r2);
  print_s [%message "emitted" ~_:(!seen : Ids.Event_id.t list)];
  let%bind queue = Monitor.review_queue m >>| ok in
  print_s [%message "queue" ~_:(List.map queue ~f:(fun a -> a.id) : Ids.Alert_id.t list)];
  let id = (List.hd_exn queue).id in
  let%bind _ =
    Monitor.apply_alert m id (Assign { analyst = Ids.Analyst_id.of_string "ana"; at = at_min 5. }) >>| ok
  in
  let%bind closed =
    Monitor.apply_alert
      m
      id
      (Close { by = Ids.Analyst_id.of_string "ana"; verdict = False_positive; note = "family transfer, documented"; at = at_min 9. })
    >>| ok
  in
  print_endline (Alert.Status.to_string (fst closed).Alert.status);
  let%map queue = Monitor.review_queue m >>| ok in
  printf "open after closing: %d\n" (List.length queue);
  [%expect {|
    decided hold
    already decided hold
    (emitted (pay-1))
    (queue (alert-pay-1))
    closed
    open after closing: 0
    |}]
;;

let%expect_test "overload is reported, not buffered" =
  let store = Store_memory.store (Store_memory.create ()) in
  let gate = Ivar.create () in
  let%bind m = new_monitor ~max_pending:2 ~on_decision:(fun _ -> Ivar.read gate) store in
  let replies =
    (* Base's List.init calls f from the last index down; map keeps arrival order. *)
    List.map [ 0; 1; 2; 3; 4 ] ~f:(fun i ->
      Monitor.on_event m (event ~id:(sprintf "p%d" i) ~at:(at_min (Float.of_int i)) 1_000))
  in
  printf "pending %d\n" (Monitor.pending m);
  Ivar.fill_exn gate ();
  let%map replies = Deferred.all replies in
  List.iter replies ~f:(fun r -> print_endline (show_reply (ok r)));
  [%expect {|
    pending 2
    decided allow
    decided allow
    overloaded
    overloaded
    overloaded
    |}]
;;

(* Decisions through the monitor, restarted halfway with an empty cache, equal one
   uninterrupted in-memory replay: profiles survive through the store. *)
let restart_equals_replay store_of =
  let events =
    Synthetic.generate { Synthetic.Config.default with customers = 60; days = 20; bad_share = 0.3 }
    |> List.map ~f:(fun l -> l.event)
  in
  let expected =
    let ev = evaluator () in
    let profiles = Evaluator.Profiles.create Rules.Config.default.profile in
    List.map events ~f:(Evaluator.process ev profiles)
  in
  let%bind store = store_of () in
  let half = List.length events / 2 in
  let run monitor events =
    Deferred.List.map ~how:`Sequential events ~f:(fun e ->
      Monitor.on_event monitor e >>| ok >>| fun r -> Option.value_exn (Monitor.Reply.decision r))
  in
  let%bind first = new_monitor ~cache_capacity:10 store in
  let%bind a = run first (List.take events half) in
  let%bind second = new_monitor store in
  let%map b = run second (List.drop events half) in
  let got = a @ b in
  printf
    "%d events, %d alerts, equal to an uninterrupted replay: %b\n"
    (List.length got)
    (List.count got ~f:(fun d -> not (Decision.Outcome.equal d.outcome Allow)))
    (List.equal Decision.equal got expected)
;;

let%expect_test "restart on the memory store" =
  let%map () = restart_equals_replay (fun () -> return (Store_memory.store (Store_memory.create ()))) in
  [%expect {| 2798 events, 74 alerts, equal to an uninterrupted replay: true |}]
;;

let%expect_test "restart on PostgreSQL" =
  let%map () =
    restart_equals_replay (fun () ->
      let%bind url = Test_db.fresh_url () in
      Store_pg.connect ~database_url:url () >>| ok >>| Store_pg.store)
  in
  [%expect {| 2798 events, 74 alerts, equal to an uninterrupted replay: true |}]
;;
