open! Core
open Transaction_monitoring
open Fixtures

let config = Rules.Config.default

(* Apply [history] to fresh profiles, then evaluate [last] and print every hit. *)
let run ?(config = config) history last =
  let customers = Hashtbl.create (module String) in
  let counterparties = Hashtbl.create (module String) in
  let customer (e : Event.t) =
    Hashtbl.find_or_add
      customers
      (Ids.Customer_id.to_string e.customer)
      ~default:(fun () -> Profile.Customer.create config.profile e.customer)
  in
  let counterparty (e : Event.t) =
    Hashtbl.find_or_add
      counterparties
      (Ids.Counterparty_id.to_string e.counterparty)
      ~default:(fun () -> Profile.Counterparty.create e.counterparty)
  in
  List.iter history ~f:(fun e ->
    Profile.Customer.update (customer e) e;
    Profile.Counterparty.update (counterparty e) e);
  let ctx =
    Context.create
      last
      ~customer:(Profile.Customer.view (customer last) last)
      ~counterparty:(Profile.Counterparty.view (counterparty last) last)
  in
  let decision = Rules.evaluate config ctx in
  printf "severity %s\n" (Risk_rules.Severity.to_string decision.severity);
  List.iter decision.hits ~f:(fun hit ->
    let e = Explanation.of_hit hit in
    print_endline (Explanation.to_string_hum e);
    print_s [%sexp (e.facts : (string * string) list)];
    printf "family %s; %s\n" (Risk_rules.Typology.to_string hit.typology) e.reference)
;;

let%expect_test "structuring: the third amount just under 10 000 in a week" =
  run
    [ event ~direction:Inbound ~at:(at_min 0.) 950_000
    ; event ~direction:Inbound ~at:(at_min (60. *. 24.)) 920_000
    ]
    (event ~direction:Inbound ~at:(at_min (60. *. 48.)) 990_000);
  [%expect
    {|
    severity review
    [review] structuring: 3 amounts in [8500.00, 10000.00) within 7d, this one 9900.00; together 28600.00
    ((count_7d 3) (sum_7d 28600.00) (threshold 10000.00) (amount 9900.00))
    family structuring; FATF R.20; 31 CFR 1010.314 (structuring to evade reporting)
    |}]
;;

let%expect_test "structuring: two in a week is not enough, 10 000 itself is outside the \
                 band"
  =
  run [ event ~at:(at_min 0.) 950_000 ] (event ~at:(at_min 10.) 950_000);
  run
    [ event ~at:(at_min 0.) 950_000; event ~at:(at_min 1.) 950_000 ]
    (event ~at:(at_min 10.) 1_000_000);
  [%expect {|
    severity allow
    severity allow
    |}]
;;

let%expect_test "smurfing: four payers under the threshold adding up above it" =
  run
    (List.init 3 ~f:(fun i ->
       event
         ~direction:Inbound
         ~counterparty:(sprintf "payer-%d" i)
         ~at:(at_min (Float.of_int i *. 30.))
         300_000))
    (event ~direction:Inbound ~counterparty:"payer-9" ~at:(at_min 120.) 250_000);
  [%expect
    {|
    severity review
    [review] smurfing: 4 payers in 24h sent 11500.00 in amounts under 10000.00
    ((payers_24h 4) (inbound_24h 11500.00) (threshold 10000.00))
    family structuring; FATF R.20; EBA/GL/2021/02 4.46 (unusual patterns); 31 CFR 1010.314
    |}]
;;

let%expect_test "rapid movement and mule fan-in on a young account" =
  let opened = at_min (-60. *. 24. *. 10.) in
  run
    (List.init 5 ~f:(fun i ->
       event
         ~account_opened_at:opened
         ~direction:Inbound
         ~counterparty:(sprintf "payer-%d" i)
         ~at:(at_min (Float.of_int i *. 20.))
         60_000))
    (event
       ~account_opened_at:opened
       ~counterparty:"exit"
       ~counterparty_country:"LT"
       ~at:(at_min 110.)
       280_000);
  [%expect
    {|
    severity review
    [review] rapid_movement: outbound 2800.00 in 24h against inbound 3000.00 (93%), last inbound 30min ago
    ((inbound_24h 3000.00) (outbound_24h 2800.00) (share_bps 9333))
    family money_mule; FATF Professional Money Laundering (2018), layering; EBA/GL/2021/02 4.72-4.75 (transaction monitoring)
    [review] mule_fan_in: 5 payers in 7d, paying out to 1 payee(s) in 24h, young account
    ((payers_7d 5) (payees_24h 1) (inbound_7d 3000.00) (account_age 10.1d))
    family money_mule; FATF Illicit Financial Flows from Cyber-Enabled Fraud (2023), money mules; FATF Professional Money Laundering (2018)
    |}]
;;

let%expect_test "mule collector: one payee paid by five customers" =
  run
    (List.init 4 ~f:(fun i ->
       event
         ~customer:(sprintf "cust-%d" (i + 10))
         ~counterparty:"collector"
         ~at:(at_min (Float.of_int i))
         40_000))
    (event ~customer:"cust-99" ~counterparty:"collector" ~at:(at_min 10.) 40_000);
  [%expect
    {|
    severity review
    [review] mule_fan_in: payee collects from 5 of the bank's customers in 7d
    ((payee_customers_7d 5) (payee_received_7d 2000.00))
    family money_mule; FATF Illicit Financial Flows from Cyber-Enabled Fraud (2023), money mules; FATF Professional Money Laundering (2018)
    |}]
;;

let%expect_test "dormant account reactivated" =
  run
    [ event ~at:(at_min 0.) 5_000 ]
    (event ~counterparty:"far" ~at:(at_min (60. *. 24. *. 200.)) 700_000);
  [%expect
    {|
    severity review
    [review] dormant_reactivation: 7000.00 after 200.0d without activity
    ((idle 200.0d) (amount 7000.00))
    family velocity; EBA/GL/2021/02 4.46 (unusual-pattern transactions); AMLR (EU) 2024/1624 Art. 26 (ongoing monitoring)
    |}]
;;

let%expect_test "corridor and sanctions-adjacent countries" =
  run [] (event ~counterparty_country:"IR" ~at:(at_min 0.) 5_000);
  run [] (event ~counterparty_country:"DE" ~ip_country:"BY" ~at:(at_min 0.) 5_000);
  [%expect
    {|
    severity review
    [review] high_risk_corridor: 50.00 to IR, first time
    ((country IR) (amount 50.00) (first_time true))
    family sanctions_geography; FATF R.19 (higher-risk countries); AMLR (EU) 2024/1624 Arts. 29-31 (high-risk third countries)
    severity review
    [review] sanctions_adjacent: 50.00 touches BY
    ((ip_country BY))
    family sanctions_geography; FATF R.6 and R.7 (targeted financial sanctions); AMLR (EU) 2024/1624 Art. 20(1)(d)
    |}]
;;

let%expect_test "card testing blocks" =
  run
    (List.init 4 ~f:(fun i ->
       event
         ~rail:Card
         ~card_channel:Online
         ~counterparty:(sprintf "m-%d" i)
         ~at:(at_min (Float.of_int i))
         99))
    (event ~rail:Card ~card_channel:Online ~counterparty:"m-9" ~at:(at_min 5.) 50);
  run
    [ event ~rail:Card ~card_channel:Online ~declined:true ~at:(at_min 0.) 4_000
    ; event ~rail:Card ~card_channel:Online ~declined:true ~at:(at_min 1.) 4_000
    ]
    (event ~rail:Card ~card_channel:Online ~declined:true ~at:(at_min 2.) 4_000);
  [%expect
    {|
    severity block
    [block] card_testing: 5 small and 0 declined card attempts in 10-15min
    ((small_10m 5) (declined_10m 0))
    family card_controls; RTS (EU) 2018/389 Art. 2(2) (known fraud scenarios)
    severity block
    [block] card_testing: 0 small and 3 declined card attempts in 10-15min
    ((small_10m 0) (declined_10m 3))
    family card_controls; RTS (EU) 2018/389 Art. 2(2) (known fraud scenarios)
    |}]
;;

let%expect_test "ATM cash: limit exceeded, and a threshold report" =
  let atm i =
    event
      ~rail:Card
      ~card_channel:Atm
      ~counterparty:(sprintf "atm-%d" i)
      ~counterparty_country:"NL"
  in
  run
    (List.init 6 ~f:(fun i -> atm i ~at:(at_min (Float.of_int i *. 30.)) 25_000))
    (atm 7 ~at:(at_min 200.) 25_000);
  run
    [ atm 1 ~direction:Inbound ~at:(at_min 0.) 600_000 ]
    (atm 2 ~direction:Inbound ~at:(at_min 30.) 400_000);
  [%expect
    {|
    severity review
    [review] atm_cash: 7 ATM operations totalling 1750.00 in 24h
    ((count_24h 7) (sum_24h 1750.00) (atm_country NL))
    family money_mule; FATF Professional Money Laundering (2018), mule cash withdrawals; EBA/GL/2021/02 Guideline 2 (cash-intensive activity)
    severity review
    [review] atm_cash: 2 ATM operations totalling 10000.00 in 24h
    ((count_24h 2) (sum_24h 10000.00) (atm_country NL))
    family money_mule; FATF Professional Money Laundering (2018), mule cash withdrawals; EBA/GL/2021/02 Guideline 2 (cash-intensive activity)
    [allow] threshold_report: cash 10000.00 in 24h reaches the reporting threshold 10000.00
    ((cash_24h 10000.00) (threshold 10000.00))
    family limits; 31 CFR 1010.311 (currency transaction report above USD 10 000); AMLR (EU) 2024/1624 Art. 80 (EUR 10 000 cash limit)
    |}]
;;

let%expect_test "unusual amount against the customer's own history" =
  run
    (List.init 12 ~f:(fun i ->
       event ~at:(at_min (Float.of_int i *. 600.)) (2_000 + (i * 100))))
    (event ~at:(at_min 8000.) 400_000);
  [%expect
    {|
    severity review
    [review] unusual_amount: 4000.00 is 142.9x the customer's p95 of 28.00
    ((amount 4000.00) (p95 28.00) (multiple 142.9))
    family velocity; EBA/GL/2021/02 4.46 (unusually large transactions); AMLR (EU) 2024/1624 Art. 34
    |}]
;;

let%expect_test "shadow and disabled typologies" =
  let config =
    { config with shadow = [ High_risk_corridor ]; disabled = [ Sanctions_adjacent ] }
  in
  run ~config [] (event ~counterparty_country:"IR" ~ip_country:"RU" ~at:(at_min 0.) 5_000);
  [%expect
    {|
    severity allow
    [allow] high_risk_corridor: 50.00 to IR, first time
    ((country IR) (amount 50.00) (first_time true))
    family sanctions_geography; FATF R.19 (higher-risk countries); AMLR (EU) 2024/1624 Arts. 29-31 (high-risk third countries)
    |}]
;;

let%expect_test "an ordinary payment triggers nothing" =
  run
    [ event ~direction:Inbound ~counterparty:"employer" ~at:(at_min 0.) 320_000 ]
    (event ~counterparty:"landlord" ~at:(at_min 60.) 95_000);
  [%expect {| severity allow |}]
;;

let%expect_test "config: defaults load from an empty file and validation refuses nonsense"
  =
  print_s [%sexp (Rules.Config.validate Rules.Config.default |> Or_error.is_ok : bool)];
  print_s
    [%sexp
      (Rules.Config.validate
         { config with structuring_min_count = 0; unusual_multiple = 0.5 }
       : Rules.Config.t Or_error.t)];
  [%expect
    {|
    true
    (Error
     ("invalid rules config"
      (("must be positive" structuring_min_count (v 0))
       "unusual_multiple must exceed 1")))
    |}]
;;
