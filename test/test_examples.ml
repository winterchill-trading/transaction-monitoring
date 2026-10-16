open! Core
open Transaction_monitoring

let%expect_test "example files load" =
  let config = Rules.Config.load "../examples/config.sexp" |> Or_error.ok_exn in
  printf
    "config equals the default: %b\n"
    (Rules.Config.equal config Rules.Config.default);
  let countermeasures =
    Rules.Config.load "../examples/config-countermeasures.sexp" |> Or_error.ok_exn
  in
  print_s [%sexp (countermeasures.overrides : Rules.Override.t list)];
  List.iter [ "policy-active"; "policy-shadow"; "policy-abstain" ] ~f:(fun name ->
    let p = Policy.load (sprintf "../examples/%s.sexp" name) |> Or_error.ok_exn in
    print_s [%sexp (p : Policy.t)]);
  [%expect
    {|
    config equals the default: true
    (((typologies ()) (countries (KP IR MM)) (severity Block))
     ((typologies (Structuring Smurfing)) (countries (RU BY)) (severity Block)))
    ((name active-2026-10) (model_mode Active) (hold_at ()) (block_at ()))
    ((name shadow-2026-10) (model_mode Shadow) (hold_at ()) (block_at ()))
    ((name active-abstain-2026-10) (model_mode Active) (hold_at ()) (block_at ())
     (abstain ((min_confidence 0.5) (confidence Logit_margin) (route Hold))))
    |}]
;;
