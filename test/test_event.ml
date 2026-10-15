open! Core
open Transaction_monitoring

let sample =
  Fixtures.event
    ~id:"pay-42"
    ~counterparty:"iban-hash-9f1c"
    ~counterparty_country:"NL"
    ~rail:Card
    ~card_channel:Atm
    ~mcc:6011
    ~device_id:"dev-7"
    ~ip_country:"DE"
    ~account_opened_at:(Fixtures.at_min (-60. *. 24. *. 400.))
    ~at:Fixtures.t0
    25_000
;;

let%expect_test "JSON line" =
  print_endline (Event.to_json_line sample);
  [%expect
    {| {"id":"pay-42","customer":"cust-1","account":"customer:cust-1/eur","counterparty":"iban-hash-9f1c","counterparty_country":"NL","rail":"card","direction":"out","amount":"250.00 EUR","base_amount":"250.00 EUR","occurred_at":"2026-03-02T09:00:00.000000000Z","received_at":"2026-03-02T09:00:00.000000000Z","mcc":6011,"card_channel":"atm","declined":false,"device_id":"dev-7","ip_country":"DE","residence":"DE","account_opened_at":"2025-01-26T09:00:00.000000000Z","risk_band":null} |}]
;;

let%test_unit "JSON round trip" =
  [%test_result: Event.t]
    (Event.of_json_line (Event.to_json_line sample) |> Or_error.ok_exn)
    ~expect:sample
;;

let%expect_test "minimal JSON, offsets and unknown fields" =
  let line =
    {|{"id":"e1","customer":"c1","account":"customer:c1/eur","counterparty":"cp","rail":"sepa_instant","direction":"in","amount":"10.00 USD","base_amount":"9.20 EUR","occurred_at":"2026-03-02T10:00:00+01:00","label":"structuring"}|}
  in
  print_s [%sexp (Event.of_json_line line : Event.t Or_error.t)];
  [%expect
    {|
    (Ok
     ((id e1) (customer c1) (account customer:c1/eur) (counterparty cp)
      (counterparty_country ()) (rail Sepa_instant) (direction Inbound)
      (amount "10.00 USD") (base_amount "9.20 EUR")
      (occurred_at "2026-03-02 09:00:00Z") (received_at "2026-03-02 09:00:00Z")
      (mcc ()) (card_channel ()) (declined false) (device_id ()) (ip_country ())
      (residence ()) (account_opened_at ()) (risk_band ())))
    |}]
;;

let%expect_test "refusals" =
  let base = Event.to_json sample in
  let with_field ?(base = base) name value =
    match base with
    | `Assoc fields ->
      `Assoc ((name, value) :: List.Assoc.remove fields ~equal:String.equal name)
    | _ -> assert false
  in
  List.iter
    [ with_field "rail" (`String "telex")
    ; with_field "base_amount" (`String "-1.00 EUR")
    ; with_field "occurred_at" (`String "yesterday")
    ; with_field ~base:(with_field "rail" (`String "sepa")) "card_channel" (`String "atm")
    ; with_field "counterparty" `Null
    ]
    ~f:(fun json ->
      match Event.of_json json with
      | Ok _ -> print_endline "accepted"
      | Error e -> print_s [%sexp (e : Error.t)]);
  [%expect
    {|
    ((event pay-42) (("in field" rail) ("unknown value" (kind rail) telex)))
    ((event pay-42) ("negative base amount" (event pay-42)))
    ((event pay-42)
     (("in field" occurred_at) ("bad time, expected RFC 3339" yesterday)))
    ((event pay-42) ("card channel on a non-card event" (event pay-42)))
    ((event pay-42) ("missing field" counterparty))
    |}]
;;

let%expect_test "stable digests" =
  print_endline [%bin_digest: Event.Stable.V1.t];
  print_endline [%bin_digest: Profile.Customer.Stable.Snapshot.V1.t];
  print_endline [%bin_digest: Profile.Counterparty.Stable.Snapshot.V1.t];
  [%expect
    {|
    0c95d0f134f6375588f2ecc6201bef52
    dbcac0d438177b0cbdd404e81076a707
    46c4fde5fa46d50754e68a89d3113c7b
    |}]
;;
