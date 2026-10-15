open! Core
open Transaction_monitoring

let small = { Synthetic.Config.default with customers = 300; days = 40; bad_share = 0.2 }

let%expect_test "a small population: volumes, labels, and determinism" =
  let a = Synthetic.generate small in
  let b = Synthetic.generate small in
  printf
    "deterministic: %b\n"
    (List.equal
       (fun (x : Synthetic.labelled) y ->
         Event.equal x.event y.event && Option.equal Typology.equal x.label y.label)
       a
       b);
  printf "events: %d\n" (List.length a);
  let by_label =
    List.filter_map a ~f:(fun l -> l.label)
    |> List.sort_and_group ~compare:Typology.compare
    |> List.map ~f:(fun g -> Typology.name (List.hd_exn g), List.length g)
  in
  print_s [%sexp (by_label : (string * int) list)];
  let sorted =
    List.is_sorted a ~compare:(fun (x : Synthetic.labelled) y ->
      Time_ns.compare x.event.occurred_at y.event.occurred_at)
  in
  printf "time-ordered: %b\n" sorted;
  [%expect
    {|
    deterministic: true
    events: 28303
    ((structuring 25) (smurfing 50) (rapid_movement 24) (mule_fan_in 54)
     (dormant_reactivation 18) (high_risk_corridor 12) (card_testing 40)
     (atm_cash 76) (sanctions_adjacent 13))
    time-ordered: true
    |}]
;;

let%test_unit "JSON lines round trip with labels" =
  List.take (Synthetic.generate { small with customers = 30 }) 500
  |> List.iter ~f:(fun (l : Synthetic.labelled) ->
    let back = Synthetic.of_json_line (Synthetic.to_json_line l) |> Or_error.ok_exn in
    [%test_result: Event.t] back.event ~expect:l.event;
    assert (Option.equal Typology.equal back.label l.label))
;;
