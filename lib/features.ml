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

let labels =
  String.Map.of_alist_exn
    [ "is_card", "card payment"
    ; "amount", "amount"
    ; "amount_is_round", "round amount"
    ; "balance", "balance"
    ; "amount_to_balance", "amount over balance"
    ; "beneficiary_age_hours", "hours since the payee was added"
    ; "beneficiary_trusted", "trusted payee"
    ; "counterparty_country_is_residence", "counterparty in the country of residence"
    ; "self_hosted_unverified", "unverified self-hosted wallet"
    ; "mcc", "merchant category"
    ; "out_count_1h", "payments out, 1 h"
    ; "out_count_24h", "payments out, 24 h"
    ; "out_sum_24h", "paid out, 24 h"
    ; "out_sum_30d", "paid out, 30 days"
    ; "out_distinct_beneficiaries_24h", "distinct payees, 24 h"
    ; "in_count_24h", "payments in, 24 h"
    ; "in_sum_24h", "received, 24 h"
    ; "pass_through_ratio_24h", "paid out over received, 24 h"
    ; "new_device", "new device"
    ; "ip_country_is_residence", "IP in the country of residence"
    ; "hours_since_credentials_change", "hours since a credentials change"
    ; "account_age_days", "account age in days"
    ; "hour_of_day_utc", "hour of day (UTC)"
    ; "log1p_amount", "amount (log scale)"
    ; "is_weekend", "weekend"
    ; "out_count_7d", "payments out, 7 days"
    ; "out_sum_1h", "paid out, 1 h"
    ; "out_sum_7d", "paid out, 7 days"
    ; "amount_over_mean_30d", "amount over the 30-day mean"
    ; "in_distinct_senders_24h", "distinct payers, 24 h"
    ; "minutes_since_last_inbound", "minutes since the last credit"
    ; "failed_sca_24h", "failed strong authentications, 24 h"
    ; "card_auths_10m", "card authorisations, 10 min"
    ; "card_declines_10m", "card declines, 10 min"
    ; "rail_sepa_instant", "rail: instant credit transfer"
    ; "rail_onchain", "rail: on-chain"
    ; "rail_lightning", "rail: Lightning"
    ; "rail_intraledger", "rail: internal transfer"
    ; "card_online", "card not present"
    ; "card_atm", "ATM"
    ; "payee_match", "payee name matched"
    ; "payee_close_match", "payee name close match"
    ; "payee_no_match", "payee name did not match"
    ; "payee_not_possible", "payee check not possible"
    ; "direction_inbound", "inbound"
    ; "is_cash", "cash"
    ; "declined", "declined"
    ; "events_seen", "events seen for the customer"
    ; "dormancy_days", "days since the last activity"
    ; "round_share_30d", "share of round amounts, 30 days"
    ; "below_threshold_n_24h", "amounts just under the reporting threshold, 24 h"
    ; "below_threshold_n_7d", "amounts just under the reporting threshold, 7 days"
    ; "below_threshold_sum_7d", "sum just under the reporting threshold, 7 days"
    ; "cash_n_24h", "cash operations, 24 h"
    ; "cash_sum_24h", "cash, 24 h"
    ; "card_small_n_10m", "small card attempts, 10 min"
    ; "declined_n_1h", "declines, 1 h"
    ; "new_counterparty", "new counterparty"
    ; "new_country", "new country"
    ; "countries_seen", "countries seen"
    ; "country_high_risk", "higher-risk country"
    ; "country_sanctions_adjacent", "country under sectoral measures"
    ; "amount_over_p95", "amount over the customer's 95th percentile"
    ; "amount_log_z", "amount against the customer's usual (z-score)"
    ; "in_n_7d", "payments in, 7 days"
    ; "in_sum_7d", "received, 7 days"
    ; "in_sum_30d", "received, 30 days"
    ; "out_n_30d", "payments out, 30 days"
    ; "senders_7d", "distinct payers, 7 days"
    ; "out_in_ratio_7d", "paid out over received, 7 days"
    ; "cp_events", "events of the counterparty"
    ; "cp_payers_7d", "customers paying the counterparty, 7 days"
    ; "cp_payees_7d", "customers paid by the counterparty, 7 days"
    ; "cp_paid_sum_7d", "paid to the counterparty by all customers, 7 days"
    ; "cp_sent_sum_7d", "sent by the counterparty to all customers, 7 days"
    ; "risk_band", "customer risk band"
    ]
;;

(* Unknown names (a schema change without a label) fall back to the name with spaces,
   never an exception on the decision path. *)
let label name =
  match Map.find labels name with
  | Some l -> l
  | None -> String.tr name ~target:'_' ~replacement:' '
;;

let has_label name = Map.mem labels name
