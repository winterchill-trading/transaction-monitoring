open! Core
open Transaction_monitoring
open Fixtures

let config = Rules.Config.default

let context history last =
  let customer = Profile.Customer.create config.profile last.Event.customer in
  let counterparty = Profile.Counterparty.create last.Event.counterparty in
  List.iter history ~f:(fun (e : Event.t) ->
    Profile.Customer.update customer e;
    if Ids.Counterparty_id.equal e.counterparty last.counterparty
    then Profile.Counterparty.update counterparty e);
  Context.create
    last
    ~customer:(Profile.Customer.view customer last)
    ~counterparty:(Profile.Counterparty.view counterparty last)
;;

let sample =
  context
    [ event ~direction:Inbound ~counterparty:"employer" ~at:(at_min 0.) 320_000
    ; event ~counterparty:"landlord" ~counterparty_country:"DE" ~at:(at_min 60.) 95_000
    ; event
        ~rail:Card
        ~card_channel:Online
        ~counterparty:"shop"
        ~device_id:"d1"
        ~at:(at_min 120.)
        4_500
    ]
    (event
       ~counterparty:"friend"
       ~counterparty_country:"IR"
       ~ip_country:"DE"
       ~device_id:"d2"
       ~at:(at_min 180.)
       50_000)
;;

let%test_unit "extra names match the extras" =
  [%test_result: string list]
    (List.map (Features.extras config sample) ~f:fst)
    ~expect:Features.extra_names
;;

let%expect_test "schema" =
  let s = Features.schema in
  printf
    "%s v%d, %d features, digest %s\n"
    s.name
    s.version
    (Risk_models.Schema.length s)
    s.digest;
  [%expect
    {| transaction-monitoring-features v1, 76 features, digest c843b98a225cad232323fe62b44bc795 |}]
;;

let%expect_test "a feature vector (present values only)" =
  List.iter (Features.to_named config sample) ~f:(fun (name, v) ->
    if (not (Float.is_nan v)) && Float.(v <> 0.) then printf "%s %g\n" name v);
  [%expect
    {|
    amount 500
    amount_is_round 1
    out_count_24h 2
    out_sum_24h 995
    out_sum_30d 995
    out_distinct_beneficiaries_24h 2
    in_count_24h 1
    in_sum_24h 3200
    pass_through_ratio_24h 0.467187
    new_device 1
    ip_country_is_residence 1
    account_age_days 0.125
    hour_of_day_utc 12
    log1p_amount 6.21661
    out_count_7d 2
    out_sum_7d 995
    amount_over_mean_30d 1.00503
    in_distinct_senders_24h 1
    minutes_since_last_inbound 180
    events_seen 3
    dormancy_days 0.0416667
    round_share_30d 0.333333
    new_counterparty 1
    new_country 1
    countries_seen 1
    country_high_risk 1
    amount_over_p95 0.526316
    amount_log_z 0.40786
    in_n_7d 1
    in_sum_7d 3200
    in_sum_30d 3200
    out_n_30d 2
    senders_7d 1
    out_in_ratio_7d 0.467187
    |}]
;;

let%expect_test "every schema feature has a reviewer-facing label" =
  let unlabelled =
    Array.filter Features.schema.features ~f:(fun name -> not (Features.has_label name))
  in
  print_s [%sexp (unlabelled : string array)];
  print_endline (Features.label "beneficiary_age_hours");
  [%expect {|
    ()
    hours since the payee was added
    |}]
;;
