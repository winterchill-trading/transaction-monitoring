open! Core
open Transaction_monitoring
module View = Profile.Customer.View

let params = Profile.Params.default

(* The incremental Welford mean and variance differ from the two-pass reference in the
   last bits; compare those within a tolerance and everything else exactly. *)
let close a b =
  match a, b with
  | None, None -> true
  | Some a, Some b -> Float.(abs (a - b) <= 1e-9 * (1. + abs b))
  | _ -> false
;;

let same (incremental : View.t) (reference : View.t) =
  close incremental.out_log_mean reference.out_log_mean
  && close incremental.out_log_sd reference.out_log_sd
  && View.equal
       { incremental with out_log_mean = None; out_log_sd = None }
       { reference with out_log_mean = None; out_log_sd = None }
;;

let%test_unit "incremental views equal a recomputation from the full history" =
  Quickcheck.test
    Fixtures.stream_generator
    ~trials:300
    ~sexp_of:[%sexp_of: Event.t list]
    ~f:(fun events ->
      let profile = Profile.Customer.create params (Ids.Customer_id.of_string "cust-1") in
      List.fold events ~init:[] ~f:(fun history e ->
        let incremental = Profile.Customer.view profile e in
        let reference = Reference.customer_view params ~history:(List.rev history) e in
        if not (same incremental reference)
        then
          raise_s
            [%message
              "views differ" (incremental : View.t) (reference : View.t) (e : Event.t)];
        Profile.Customer.update profile e;
        e :: history)
      |> ignore)
;;

let%test_unit "a snapshot taken mid-stream resumes to the same profile" =
  Quickcheck.test
    (Quickcheck.Generator.both Fixtures.stream_generator Int.quickcheck_generator)
    ~trials:200
    ~sexp_of:[%sexp_of: Event.t list * int]
    ~f:(fun (events, cut) ->
      let cut = Int.abs cut % (List.length events + 1) in
      let #(first, rest) = List.split_n events cut in
      let straight = Profile.Customer.create params (Ids.Customer_id.of_string "cust-1") in
      List.iter events ~f:(Profile.Customer.update straight);
      let partial = Profile.Customer.create params (Ids.Customer_id.of_string "cust-1") in
      List.iter first ~f:(Profile.Customer.update partial);
      let bytes =
        Bin_prot.Writer.to_string
          Profile.Customer.Stable.Snapshot.V1.bin_writer_t
          (Profile.Customer.snapshot partial)
      in
      let resumed =
        Profile.Customer.of_snapshot
          (Bin_prot.Reader.of_string Profile.Customer.Stable.Snapshot.V1.bin_reader_t bytes)
      in
      List.iter rest ~f:(Profile.Customer.update resumed);
      [%test_result: Profile.Customer.snapshot]
        (Profile.Customer.snapshot resumed)
        ~expect:(Profile.Customer.snapshot straight))
;;

let%test_unit "a snapshot is a copy: later updates do not change it" =
  let profile = Profile.Customer.create params (Ids.Customer_id.of_string "cust-1") in
  Profile.Customer.update profile (Fixtures.event ~at:Fixtures.t0 1_000);
  let snap = Profile.Customer.snapshot profile in
  let before = Profile.Customer.snapshot profile in
  Profile.Customer.update profile (Fixtures.event ~at:(Fixtures.at_min 1.) 2_000);
  [%test_result: Profile.Customer.snapshot] snap ~expect:before
;;

let%expect_test "a view of a small history" =
  let profile = Profile.Customer.create params (Ids.Customer_id.of_string "cust-1") in
  let open Fixtures in
  List.iter
    ~f:(Profile.Customer.update profile)
    [ event ~direction:Inbound ~counterparty:"employer" ~at:(at_min 0.) 950_000
    ; event ~counterparty:"landlord" ~counterparty_country:"DE" ~at:(at_min 10.) 120_000
    ; event ~counterparty:"shop" ~rail:Card ~card_channel:Online ~at:(at_min 20.) 150
    ; event ~counterparty:"shop" ~rail:Card ~card_channel:Online ~declined:true ~at:(at_min 21.) 150
    ; event ~counterparty:"atm-1" ~rail:Card ~card_channel:Atm ~at:(at_min 30.) 20_000
    ];
  let next = event ~counterparty:"new-friend" ~counterparty_country:"AE" ~at:(at_min 31.) 900_000 in
  print_s [%sexp (Profile.Customer.view profile next : View.t)];
  [%expect {|
    ((now "2026-03-02 09:31:00Z") (events 5)
     (first_seen ("2026-03-02 09:00:00Z")) (last_seen ("2026-03-02 09:30:00Z"))
     (last_inbound ("2026-03-02 09:00:00Z")) (out_n_1h 3) (out_sum_1h 140150)
     (in_n_1h 1) (in_sum_1h 950000) (out_n_24h 3) (out_sum_24h 140150)
     (in_n_24h 1) (in_sum_24h 950000) (out_n_7d 3) (out_sum_7d 140150)
     (in_n_7d 1) (in_sum_7d 950000) (out_n_30d 3) (out_sum_30d 140150)
     (in_n_30d 1) (in_sum_30d 950000) (round_n_30d 3) (below_n_24h 1)
     (below_sum_24h 950000) (below_n_7d 1) (below_sum_7d 950000) (cash_n_24h 1)
     (cash_sum_24h 20000) (card_n_10m 3) (card_small_n_10m 2) (declined_n_10m 1)
     (declined_n_1h 1) (out_p50 (200)) (out_p95 (1200))
     (out_log_mean (4.4368351540044051)) (out_log_sd (3.1771933531502667))
     (counterparty_first_seen ()) (senders_24h 1) (senders_7d 1)
     (beneficiaries_24h 3) (country_seen (false)) (countries_seen 1)
     (device_seen ()))
    |}]
;;
