open! Core
open Transaction_monitoring

let%expect_test "example files load" =
  let config = Rules.Config.load "../examples/config.sexp" |> Or_error.ok_exn in
  printf
    "config equals the default: %b\n"
    (Rules.Config.equal config Rules.Config.default);
  List.iter [ "policy-active"; "policy-shadow" ] ~f:(fun name ->
    let p = Policy.load (sprintf "../examples/%s.sexp" name) |> Or_error.ok_exn in
    print_s [%sexp (p : Policy.t)]);
  [%expect
    {|
    config equals the default: true
    ((name active-2026-10) (model_mode Active) (hold_at ()) (block_at ())
     (abstain ()))
    ((name shadow-2026-10) (model_mode Shadow) (hold_at ()) (block_at ())
     (abstain ()))
    |}]
;;
