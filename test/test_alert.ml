open! Core
open Transaction_monitoring
open Fixtures

let held_decision () =
  let e = event ~id:"pay-1" ~counterparty_country:"IR" ~at:t0 5_000 in
  let customer = Profile.Customer.create Profile.Params.default e.customer in
  let counterparty = Profile.Counterparty.create e.counterparty in
  let ctx =
    Context.create
      e
      ~customer:(Profile.Customer.view customer e)
      ~counterparty:(Profile.Counterparty.view counterparty e)
  in
  Decision.create
    Policy.default
    ~event:e
    ~rules:(Rules.evaluate Rules.Config.default ctx)
    ~model:None
  |> Or_error.ok_exn
;;

let ana = Ids.Analyst_id.of_string
let at m = at_min m

let show = function
  | Ok ((t : Alert.t), events) ->
    print_s
      [%message
        (Alert.Status.to_string t.status)
          ~assignee:(t.assignee : Ids.Analyst_id.t option)
          ~sar:(t.sar_candidate : bool)
          ~version:(t.version : int)
          (events : Alert.Event.t list)]
  | Error e -> print_s [%message "refused" ~_:(e : Error.t)]
;;

let%expect_test "lifecycle" =
  let alert, events = Alert.open_ (held_decision ()) |> Option.value_exn in
  print_s [%message (alert.id : Ids.Alert_id.t) (events : Alert.Event.t list)];
  let run alert command =
    let r = Alert.apply alert command in
    show r;
    Result.map r ~f:fst |> Result.ok |> Option.value ~default:alert
  in
  let alert =
    run alert (Close { by = ana "a1"; verdict = True_positive; note = "x"; at = at 1. })
  in
  let alert = run alert (Assign { analyst = ana "a1"; at = at 2. }) in
  let alert = run alert (Assign { analyst = ana "a1"; at = at 3. }) in
  let alert = run alert (Flag_sar { by = ana "a2"; reason = "pattern"; at = at 4. }) in
  let alert =
    run alert (Flag_sar { by = ana "a1"; reason = "layering via IR"; at = at 5. })
  in
  let alert =
    run alert (Close { by = ana "a1"; verdict = False_positive; note = "ok"; at = at 6. })
  in
  let alert =
    run
      alert
      (Close { by = ana "a1"; verdict = True_positive; note = "SAR drafted"; at = at 7. })
  in
  let alert = run alert (Reopen { by = ana "a1"; reason = "second look"; at = at 8. }) in
  let alert = run alert (Reopen { by = ana "a2"; reason = "QA sample"; at = at 9. }) in
  ignore (alert : Alert.t);
  [%expect
    {|
    ((alert.id alert-pay-1) (events ((Opened (at "2026-03-02 09:00:00Z")))))
    (refused
     ("assign the alert before closing it" (alert alert-pay-1) (status open)))
    (assigned (assignee (a1)) (sar false) (version 2)
     (events ((Assigned (analyst a1) (previous ()) (at "2026-03-02 09:02:00Z")))))
    (assigned (assignee (a1)) (sar false) (version 2) (events ()))
    (refused
     ("only the assignee can flag a SAR candidate" (alert alert-pay-1)
      (status assigned)))
    (assigned (assignee (a1)) (sar true) (version 3)
     (events
      ((Sar_flagged (by a1) (reason "layering via IR")
        (at "2026-03-02 09:05:00Z")))))
    (refused
     ("a SAR candidate cannot be closed as a false positive" (alert alert-pay-1)
      (status assigned)))
    (closed (assignee (a1)) (sar true) (version 4)
     (events
      ((Closed
        ((verdict True_positive) (note "SAR drafted") (by a1)
         (at "2026-03-02 09:07:00Z"))))))
    (refused
     ("the analyst who closed the alert cannot reopen it" (alert alert-pay-1)
      (status closed)))
    (assigned (assignee (a2)) (sar true) (version 5)
     (events
      ((Reopened (by a2) (reason "QA sample") (at "2026-03-02 09:09:00Z")))))
    |}]
;;

let%test "an allowed decision opens no alert" =
  let e = event ~at:t0 5_000 in
  let d =
    Decision.create Policy.default ~event:e ~rules:Risk_rules.Decision.allow ~model:None
    |> Or_error.ok_exn
  in
  Option.is_none (Alert.open_ d)
;;

let command_generator : Alert.Command.t Quickcheck.Generator.t =
  let open Quickcheck.Generator.Let_syntax in
  let%bind by = Quickcheck.Generator.of_list [ ana "a1"; ana "a2"; ana "a3" ]
  and reason = Quickcheck.Generator.of_list [ ""; "because" ]
  and minute = Float.gen_incl 0. 1000. in
  let at = at minute in
  Quickcheck.Generator.of_list
    [ Alert.Command.Assign { analyst = by; at }
    ; Flag_sar { by; reason; at }
    ; Unflag_sar { by; reason; at }
    ; Close { by; verdict = True_positive; note = reason; at }
    ; Close { by; verdict = False_positive; note = reason; at }
    ; Reopen { by; reason; at }
    ]
;;

let%test_unit "invariants hold under any command sequence" =
  let start, _ = Alert.open_ (held_decision ()) |> Option.value_exn in
  Quickcheck.test
    (List.gen_with_length 30 command_generator)
    ~sexp_of:[%sexp_of: Alert.Command.t list]
    ~f:(fun commands ->
      let final, applied =
        List.fold commands ~init:(start, 0) ~f:(fun (t, applied) c ->
          match Alert.apply t c with
          | Error _ -> t, applied
          | Ok (t', events) ->
            (match t'.status, t'.assignee, t'.resolution with
             | Closed, Some _, Some r ->
               assert (
                 not (Alert.Verdict.equal r.verdict False_positive && t'.sar_candidate))
             | Assigned, Some _, None -> ()
             | Open, None, None -> ()
             | _ -> raise_s [%message "inconsistent alert" (t' : Alert.t)]);
            t', applied + List.length events)
      in
      [%test_result: int] final.version ~expect:(1 + applied))
;;
