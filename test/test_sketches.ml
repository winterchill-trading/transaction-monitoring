open! Core
open Transaction_monitoring

let%expect_test "window: bucket-aligned sums, skipped buckets cleared, late values ignored" =
  let w = Window.create ~width:(Time_ns.Span.of_min 5.) ~buckets:3 ~fields:1 in
  let at m = Fixtures.at_min m in
  let show now = printf "%d " (Window.total w ~now:(at now) ~field:0) in
  Window.add w ~at:(at 0.) ~field:0 1;
  Window.add w ~at:(at 6.) ~field:0 10;
  Window.add w ~at:(at 11.) ~field:0 100;
  show 11.;
  show 15.;
  show 20.;
  show 100.;
  (* a late value inside the window still counts *)
  Window.add w ~at:(at 1.) ~field:0 1000;
  show 11.;
  (* moving to minute 100 clears everything; minute 1 is then older than the window *)
  Window.add w ~at:(at 100.) ~field:0 5;
  Window.add w ~at:(at 1.) ~field:0 1000;
  show 100.;
  show 11.;
  [%expect {| 111 110 100 0 1111 5 0 |}]
;;

let%test_unit "P-square stays close to the exact quantile on skewed data" =
  let rng = Random.State.make [| 7 |] in
  List.iter [ 0.5; 0.95 ] ~f:(fun p ->
    let sketch = P2_quantile.create ~p in
    let xs =
      Array.init 5_000 ~f:(fun _ ->
        (* log-normal amounts, like payment amounts *)
        let u1 = Random.State.float rng 1. +. 1e-12
        and u2 = Random.State.float rng 1. in
        Float.exp (4. +. (1.2 *. Float.sqrt (-2. *. Float.log u1) *. Float.cos (2. *. Float.pi *. u2))))
    in
    Array.iter xs ~f:(P2_quantile.add sketch);
    Array.sort xs ~compare:Float.compare;
    let exact = xs.(Float.iround_down_exn (p *. 5_000.)) in
    let estimate = Option.value_exn (P2_quantile.estimate sketch) in
    (* P-square is an approximation; within 5% of the exact value on 5 000 samples *)
    if Float.(abs (estimate - exact) > 0.05 * exact)
    then raise_s [%message "estimate too far" (p : float) (estimate : float) (exact : float)])
;;

let%test_unit "P-square estimate lies within the range seen" =
  Quickcheck.test
    (List.gen_non_empty (Float.gen_incl 0. 1e6))
    ~sexp_of:[%sexp_of: float list]
    ~f:(fun xs ->
      let sketch = P2_quantile.create ~p:0.9 in
      List.iter xs ~f:(P2_quantile.add sketch);
      let e = Option.value_exn (P2_quantile.estimate sketch) in
      let lo = List.reduce_exn xs ~f:Float.min
      and hi = List.reduce_exn xs ~f:Float.max in
      assert (Float.(lo <= e && e <= hi)))
;;

let%expect_test "recent: evicts the least recently seen key when full" =
  let r = Recent.create ~capacity:3 in
  let at m = Fixtures.at_min m in
  Recent.touch r "a" ~at:(at 0.) ~inbound:true;
  Recent.touch r "b" ~at:(at 1.) ~inbound:false;
  Recent.touch r "c" ~at:(at 2.) ~inbound:true;
  Recent.touch r "a" ~at:(at 3.) ~inbound:false;
  Recent.touch r "d" ~at:(at 4.) ~inbound:true;
  print_s [%sexp (Recent.keys r : string list)];
  printf
    "inbound since 1: %d, outbound since 0: %d\n"
    (Recent.count_since r ~since:(at 1.) ~inbound:true)
    (Recent.count_since r ~since:(at 0.) ~inbound:false);
  print_s [%sexp (Recent.first_seen r "a" : Time_ns.Alternate_sexp.t option)];
  [%expect {|
    (d a c)
    inbound since 1: 2, outbound since 0: 1
    ("2026-03-02 09:00:00Z")
    |}]
;;

let%expect_test "country set" =
  let s = Country_set.create () in
  List.iter [ "AD"; "ZW"; "DE"; "DE"; "ZZ" ] ~f:(fun c ->
    Country_set.add s (Risk_rules.Country.of_string c));
  printf
    "%d %b %b\n"
    (Country_set.cardinal s)
    (Country_set.mem s (Risk_rules.Country.of_string "ZW"))
    (Country_set.mem s (Risk_rules.Country.of_string "FR"));
  [%expect {| 4 true false |}]
;;
