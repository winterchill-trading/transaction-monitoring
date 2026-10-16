open! Core
open Transaction_monitoring

(* An active model only adds alerts to the rules (the worst outcome wins), so nothing is
   cleared; on a population this small and young most of its extra alerts come from
   profiles without history. *)
let%expect_test "a new model replayed next to the rules-only baseline" =
  let model = Model.load "../models/tm-aml-gbdt/model.json" |> Or_error.ok_exn in
  let config = Rules.Config.default in
  let baseline = Evaluator.create config |> Or_error.ok_exn in
  let candidate =
    Evaluator.create ~model ~policy:{ Policy.default with model_mode = Active } config
    |> Or_error.ok_exn
  in
  let replay = Replay.create ~baseline ~max_changes:3 candidate in
  Synthetic.generate
    { Synthetic.Config.default with customers = 150; days = 40; bad_share = 0.2 }
  |> List.iter ~f:(fun (l : Synthetic.labelled) ->
    ignore (Replay.step ?label:l.label replay l.event : Decision.t));
  let r = Replay.report replay in
  let side (s : Replay.Side.t) =
    s.decisions, s.hold + s.block, s.labelled, s.labelled_alerted
  in
  print_s
    [%message
      ""
        ~candidate:(side r.candidate : int * int * int * int)
        ~baseline:(side (Option.value_exn r.baseline) : int * int * int * int)
        ~changed:(r.changed : int)
        ~new_alerts:(r.new_alerts : int)
        ~cleared:(r.cleared_alerts : int)
        ~sample:(List.length r.sample_changes : int)];
  [%expect
    {|
    ((candidate (14756 911 103 103)) (baseline (14756 265 103 62)) (changed 688)
     (new_alerts 646) (cleared 0) (sample 3))
    |}]
;;

(* The accuracy-coverage table on labelled synthetic events, and the same history with an
   abstaining policy: abstentions routed to a hold show up as their own alert kind. *)
let%expect_test "accuracy-coverage report and an abstaining policy" =
  let model = Model.load "../models/tm-aml-gbdt/model.json" |> Or_error.ok_exn in
  let config = Rules.Config.default in
  let events =
    Synthetic.generate
      { Synthetic.Config.default with customers = 150; days = 40; bad_share = 0.2 }
  in
  let replay policy =
    let candidate = Evaluator.create ~model ~policy config |> Or_error.ok_exn in
    let r = Replay.create ~max_changes:1 candidate in
    List.iter events ~f:(fun (l : Synthetic.labelled) ->
      ignore (Replay.step ?label:l.label r l.event : Decision.t));
    Replay.report r
  in
  let shadow = replay Policy.default in
  let curve = Option.value_exn shadow.accuracy_coverage in
  List.iter [ 1.0; 0.95; 0.8 ] ~f:(fun c ->
    let p = Option.value_exn (Risk_models.Abstain.Curve.at_coverage curve c) in
    printf
      "coverage >= %.2f: covered %d of %d, accuracy %.3f, recall %.3f\n"
      c
      p.covered
      curve.rows
      p.accuracy
      p.recall);
  let abstaining =
    replay
      { Policy.default with
        model_mode = Active
      ; abstain = Some { min_confidence = 0.5; confidence = Logit_margin; route = Hold }
      }
  in
  printf
    "abstained %d; alerts by kind: %s\n"
    abstaining.candidate.abstained
    (List.map abstaining.candidate.alerts_by_typology ~f:(fun (k, v) ->
       sprintf "%s %d" k v)
     |> String.concat ~sep:", ");
  [%expect
    {|
    coverage >= 1.00: covered 14756 of 14756, accuracy 0.958, recall 1.000
    coverage >= 0.95: covered 14019 of 14756, accuracy 0.975, recall 1.000
    coverage >= 0.80: covered 11805 of 14756, accuracy 0.979, recall 1.000
    abstained 131; alerts by kind: atm_cash 19, card_testing 23, dormant_reactivation 2, high_risk_corridor 1, model 671, model_uncertain 131, mule_fan_in 122, rapid_movement 20, sanctions_adjacent 1, smurfing 67, structuring 10, unusual_amount 13
    |}]
;;
