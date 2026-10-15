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
  Synthetic.generate { Synthetic.Config.default with customers = 150; days = 40; bad_share = 0.2 }
  |> List.iter ~f:(fun (l : Synthetic.labelled) ->
    ignore (Replay.step ?label:l.label replay l.event : Decision.t));
  let r = Replay.report replay in
  let side (s : Replay.Side.t) = s.decisions, s.hold + s.block, s.labelled, s.labelled_alerted in
  print_s
    [%message
      ""
        ~candidate:(side r.candidate : int * int * int * int)
        ~baseline:(side (Option.value_exn r.baseline) : int * int * int * int)
        ~changed:(r.changed : int)
        ~new_alerts:(r.new_alerts : int)
        ~cleared:(r.cleared_alerts : int)
        ~sample:(List.length r.sample_changes : int)];
  [%expect {|
    ((candidate (14756 911 103 103)) (baseline (14756 265 103 62)) (changed 688)
     (new_alerts 646) (cleared 0) (sample 3))
    |}]
;;
