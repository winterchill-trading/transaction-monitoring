open! Core
open Transaction_monitoring

let t0 = Time_ns.of_string_with_utc_offset "2026-03-02T09:00:00Z"
let at_min m = Time_ns.add t0 (Time_ns.Span.of_min m)
let counter = ref 0

let event
      ?id
      ?(customer = "cust-1")
      ?(counterparty = "cp-1")
      ?counterparty_country
      ?(rail = Event.Rail.Sepa)
      ?(direction = Event.Direction.Outbound)
      ?card_channel
      ?mcc
      ?(declined = false)
      ?device_id
      ?ip_country
      ?(residence = "DE")
      ?account_opened_at
      ~at
      amount
  : Event.t
  =
  let id =
    match id with
    | Some id -> id
    | None ->
      incr counter;
      sprintf "ev-%d" !counter
  in
  let amount = Money.Amount.of_minor_units_int Money.Currency.eur amount in
  { id = Ids.Event_id.of_string id
  ; customer = Ids.Customer_id.of_string customer
  ; account = Ledger.Account_id.of_string ("customer:" ^ customer ^ "/eur")
  ; counterparty = Ids.Counterparty_id.of_string counterparty
  ; counterparty_country = Option.map counterparty_country ~f:Risk_rules.Country.of_string
  ; rail
  ; direction
  ; amount
  ; base_amount = amount
  ; occurred_at = at
  ; received_at = at
  ; mcc = Option.map mcc ~f:Risk_rules.Mcc.of_int_exn
  ; card_channel
  ; declined
  ; device_id
  ; ip_country = Option.map ip_country ~f:Risk_rules.Country.of_string
  ; residence = Some (Risk_rules.Country.of_string residence)
  ; account_opened_at
  ; risk_band = None
  }
;;

let stream_generator =
  let open Quickcheck.Generator.Let_syntax in
  let one =
    (* gap in minutes: mostly small, sometimes days, sometimes slightly backwards *)
    let%bind gap =
      Quickcheck.Generator.weighted_union
        [ 6., Float.gen_incl 0. 30.
        ; 2., Float.gen_incl 30. 600.
        ; 1., Float.gen_incl 600. 5000.
        ; 0.5, Float.gen_incl (-10.) 0.
        ]
    and amount =
      Quickcheck.Generator.weighted_union
        [ 4., Int.gen_incl 1 50_000
        ; 1., (Int.gen_incl 1 50 >>| fun k -> k * 10_000)
        ; 1., Int.gen_incl 850_000 999_999
        ; 1., Int.gen_incl 1 199
        ; 0.3, Int.gen_incl 1_000_000 5_000_000
        ]
    and cp = Int.gen_incl 1 20
    and country =
      Option.quickcheck_generator (Quickcheck.Generator.of_list [ "DE"; "FR"; "NL"; "AE"; "TR"; "PA" ])
    and ip_country = Option.quickcheck_generator (Quickcheck.Generator.of_list [ "DE"; "RU" ])
    and device = Option.quickcheck_generator (Int.gen_incl 1 5)
    and rail = Event.Rail.quickcheck_generator
    and direction = Event.Direction.quickcheck_generator
    and channel = Event.Card_channel.quickcheck_generator
    and declined = Quickcheck.Generator.weighted_union [ 9., return false; 1., return true ] in
    return (gap, fun ~at ->
      event
        ~counterparty:(sprintf "cp-%d" cp)
        ?counterparty_country:country
        ?ip_country
        ?device_id:(Option.map device ~f:(sprintf "dev-%d"))
        ~rail
        ~direction
        ?card_channel:(match rail with Card -> Some channel | _ -> None)
        ~declined
        ~at
        amount)
  in
  let%bind n = Int.gen_incl 1 120 in
  let%map steps = List.gen_with_length n one in
  let _, events =
    List.fold_map steps ~init:0. ~f:(fun minutes (gap, make) ->
      let minutes = minutes +. gap in
      minutes, make ~at:(at_min minutes))
  in
  events
;;
