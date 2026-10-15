open! Core
module F = Risk_rules.Features
module Country = Risk_rules.Country

let major minor = Float.of_int minor /. 100.
let opt_float f = Option.value_map ~default:Float.nan ~f
let flag b = if b then 1. else 0.
let opt_flag = opt_float flag
let hours span = Time_ns.Span.to_hr span
let days span = Time_ns.Span.to_day span

let rail_name : Event.Rail.t -> string = function
  | Crypto -> "onchain:unknown" (* payments-core's rail text, which Encoding one-hots *)
  | r -> Event.Rail.to_string r
;;

let card_channel_name : Event.Card_channel.t -> string = function
  | In_person -> "In_person"
  | Online -> "Online"
  | Atm -> "Atm"
;;

let risk_rules (ctx : Context.t) : F.t =
  let e = ctx.event
  and c = ctx.customer in
  let amount = major ctx.amount in
  let outbound = Context.is_outbound ctx in
  let now = c.now in
  let mean_out_30d =
    if c.out_n_30d = 0
    then None
    else Some (major c.out_sum_30d /. Float.of_int c.out_n_30d)
  in
  let ofday = Time_ns.to_ofday now ~zone:Time_float.Zone.utc in
  let weekday = Date.day_of_week (Time_ns.to_date now ~zone:Time_float.Zone.utc) in
  { F.Stable.V1.schema_version = F.schema_version
  ; is_card = Event.Rail.equal e.rail Card
  ; rail = rail_name e.rail
  ; card_channel = Option.map e.card_channel ~f:card_channel_name
  ; currency = Money.Currency.code (Money.Amount.currency e.base_amount)
  ; amount
  ; amount_is_round = ctx.amount > 0 && ctx.amount % 10_000 = 0
  ; balance = None
  ; amount_to_balance = None
  ; beneficiary_age_hours =
      Option.map c.counterparty_first_seen ~f:(fun t -> hours (Time_ns.diff now t))
  ; beneficiary_trusted = None
  ; counterparty_country_is_residence =
      (match e.counterparty_country, e.residence with
       | Some a, Some b -> Some (Country.equal a b)
       | _ -> None)
  ; self_hosted_unverified = false
  ; mcc = Option.map e.mcc ~f:Risk_rules.Mcc.to_int
  ; out_count_1h = c.out_n_1h
  ; out_count_24h = c.out_n_24h
  ; out_sum_24h = major c.out_sum_24h
  ; out_sum_30d = major c.out_sum_30d
  ; out_distinct_beneficiaries_24h = c.beneficiaries_24h
  ; in_count_24h = c.in_n_24h
  ; in_sum_24h = major c.in_sum_24h
  ; pass_through_ratio_24h =
      (if c.in_sum_24h > 0
       then
         Some
           (major (c.out_sum_24h + if outbound then ctx.amount else 0)
            /. major c.in_sum_24h)
       else None)
  ; new_device = Option.equal Bool.equal c.device_seen (Some false)
  ; ip_country_is_residence =
      (match e.ip_country, e.residence with
       | Some a, Some b -> Some (Country.equal a b)
       | _ -> None)
  ; hours_since_credentials_change = None
  ; account_age_days = Option.map (Context.account_age ctx) ~f:days
  ; hour_of_day_utc =
      Time_ns.Ofday.to_span_since_start_of_day ofday
      |> Time_ns.Span.to_hr
      |> Float.iround_down_exn
  ; log1p_amount = Float.log1p amount
  ; is_weekend = Day_of_week.is_sun_or_sat weekday
  ; out_count_7d = c.out_n_7d
  ; out_sum_1h = major c.out_sum_1h
  ; out_sum_7d = major c.out_sum_7d
  ; amount_over_mean_30d =
      Option.bind mean_out_30d ~f:(fun m ->
        if Float.(m > 0.) then Some (amount /. m) else None)
  ; in_distinct_senders_24h = c.senders_24h
  ; minutes_since_last_inbound =
      Option.map c.last_inbound ~f:(fun t -> Time_ns.Span.to_min (Time_ns.diff now t))
  ; failed_sca_24h = 0
  ; payee_check = None
  ; card_auths_10m = c.card_n_10m
  ; card_declines_10m = c.declined_n_10m
  }
;;

(* The AML columns: name and value, in order. *)
let extras (config : Rules.Config.t) (ctx : Context.t) =
  let e = ctx.event
  and c = ctx.customer
  and cp = ctx.counterparty in
  let amount = major ctx.amount in
  let outbound = Context.is_outbound ctx in
  let listed l = Option.value_map ~default:false ~f:(List.mem l ~equal:Country.equal) in
  let ratio a b = if b = 0 then Float.nan else Float.of_int a /. Float.of_int b in
  [ "direction_inbound", flag (not outbound)
  ; "is_cash", flag (Event.is_cash e)
  ; "declined", flag e.declined
  ; "events_seen", Float.of_int c.events
  ; "dormancy_days", opt_float (fun t -> days (Time_ns.diff c.now t)) c.last_seen
  ; "round_share_30d", ratio c.round_n_30d (c.out_n_30d + c.in_n_30d)
  ; "below_threshold_n_24h", Float.of_int c.below_n_24h
  ; "below_threshold_n_7d", Float.of_int c.below_n_7d
  ; "below_threshold_sum_7d", major c.below_sum_7d
  ; "cash_n_24h", Float.of_int c.cash_n_24h
  ; "cash_sum_24h", major c.cash_sum_24h
  ; "card_small_n_10m", Float.of_int c.card_small_n_10m
  ; "declined_n_1h", Float.of_int c.declined_n_1h
  ; "new_counterparty", flag (Option.is_none c.counterparty_first_seen)
  ; "new_country", opt_flag (Option.map c.country_seen ~f:not)
  ; "countries_seen", Float.of_int c.countries_seen
  ; "country_high_risk", flag (listed config.high_risk_countries e.counterparty_country)
  ; ( "country_sanctions_adjacent"
    , flag
        (listed config.sanctions_adjacent_countries e.counterparty_country
         || listed config.sanctions_adjacent_countries e.ip_country) )
  ; ( "amount_over_p95"
    , opt_float (fun p -> if Float.(p > 0.) then amount /. p else Float.nan) c.out_p95 )
  ; ( "amount_log_z"
    , match c.out_log_mean, c.out_log_sd with
      | Some m, Some sd when Float.(sd > 0.) -> (Float.log1p amount -. m) /. sd
      | _ -> Float.nan )
  ; "in_n_7d", Float.of_int c.in_n_7d
  ; "in_sum_7d", major c.in_sum_7d
  ; "in_sum_30d", major c.in_sum_30d
  ; "out_n_30d", Float.of_int c.out_n_30d
  ; "senders_7d", Float.of_int c.senders_7d
  ; ( "out_in_ratio_7d"
    , ratio (c.out_sum_7d + if outbound then ctx.amount else 0) c.in_sum_7d )
  ; "cp_events", Float.of_int cp.events
  ; "cp_payers_7d", Float.of_int cp.payers_7d
  ; "cp_payees_7d", Float.of_int cp.payees_7d
  ; "cp_paid_sum_7d", major cp.paid_sum_7d
  ; "cp_sent_sum_7d", major cp.sent_sum_7d
  ; ( "risk_band"
    , opt_float
        (function
          | Event.Risk_band.Low -> 0.
          | Medium -> 1.
          | High -> 2.)
        e.risk_band )
  ]
;;

let extra_names =
  (* The names do not depend on the values; any context gives them. *)
  [ "direction_inbound"
  ; "is_cash"
  ; "declined"
  ; "events_seen"
  ; "dormancy_days"
  ; "round_share_30d"
  ; "below_threshold_n_24h"
  ; "below_threshold_n_7d"
  ; "below_threshold_sum_7d"
  ; "cash_n_24h"
  ; "cash_sum_24h"
  ; "card_small_n_10m"
  ; "declined_n_1h"
  ; "new_counterparty"
  ; "new_country"
  ; "countries_seen"
  ; "country_high_risk"
  ; "country_sanctions_adjacent"
  ; "amount_over_p95"
  ; "amount_log_z"
  ; "in_n_7d"
  ; "in_sum_7d"
  ; "in_sum_30d"
  ; "out_n_30d"
  ; "senders_7d"
  ; "out_in_ratio_7d"
  ; "cp_events"
  ; "cp_payers_7d"
  ; "cp_payees_7d"
  ; "cp_paid_sum_7d"
  ; "cp_sent_sum_7d"
  ; "risk_band"
  ]
;;

let base = Risk_models.Encoding.schema
let n_base = Risk_models.Schema.length base

let schema =
  Risk_models.Schema.create_exn
    ~name:"transaction-monitoring-features"
    ~version:1
    (Array.to_list base.features @ extra_names)
;;

let encode_into config ctx into =
  if Array.length into <> Risk_models.Schema.length schema
  then invalid_arg "Features.encode_into: wrong vector length";
  let head = Risk_models.Encoding.encode (risk_rules ctx) in
  Array.blit ~src:head ~src_pos:0 ~dst:into ~dst_pos:0 ~len:n_base;
  List.iteri (extras config ctx) ~f:(fun i (_, v) -> into.(n_base + i) <- v)
;;

let encode config ctx =
  let into = Array.create ~len:(Risk_models.Schema.length schema) 0. in
  encode_into config ctx into;
  into
;;

let to_named config ctx =
  List.zip_exn (Array.to_list schema.features) (Array.to_list (encode config ctx))
;;
