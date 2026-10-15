open! Core

module Stable = struct
  module V1 = struct
    type t =
      | Structuring
      | Smurfing
      | Rapid_movement
      | Mule_fan_in
      | Dormant_reactivation
      | High_risk_corridor
      | Card_testing
      | Atm_cash
      | Sanctions_adjacent
      | Threshold_report
      | Unusual_amount
    [@@deriving bin_io, sexp, compare, equal, hash, enumerate, quickcheck]
  end
end

include Stable.V1

let name = function
  | Structuring -> "structuring"
  | Smurfing -> "smurfing"
  | Rapid_movement -> "rapid_movement"
  | Mule_fan_in -> "mule_fan_in"
  | Dormant_reactivation -> "dormant_reactivation"
  | High_risk_corridor -> "high_risk_corridor"
  | Card_testing -> "card_testing"
  | Atm_cash -> "atm_cash"
  | Sanctions_adjacent -> "sanctions_adjacent"
  | Threshold_report -> "threshold_report"
  | Unusual_amount -> "unusual_amount"
;;

let by_name = List.map all ~f:(fun t -> name t, t)
let of_name s = List.Assoc.find by_name ~equal:String.equal s

let code t : Risk_rules.Reason_code.t =
  match t with
  | Structuring -> Structuring
  | Rapid_movement -> Mule_pass_through
  | Mule_fan_in -> Mule_fan_in
  | Card_testing -> Card_testing
  | High_risk_corridor -> Geo_high_risk
  | Smurfing
  | Dormant_reactivation
  | Atm_cash
  | Sanctions_adjacent
  | Threshold_report
  | Unusual_amount -> Custom ("TM_" ^ String.uppercase (name t))
;;

let family t : Risk_rules.Typology.t =
  match t with
  | Structuring | Smurfing -> Structuring
  | Rapid_movement | Mule_fan_in | Atm_cash -> Money_mule
  | Dormant_reactivation | Unusual_amount -> Velocity
  | High_risk_corridor | Sanctions_adjacent -> Sanctions_geography
  | Card_testing -> Card_controls
  | Threshold_report -> Limits
;;

let description = function
  | Structuring ->
    "repeated amounts just under the reporting threshold from one customer within 7 days"
  | Smurfing ->
    "several different payers each sending amounts under the reporting threshold into \
     one account within 24 hours, together above it"
  | Rapid_movement ->
    "most of what came in during the last 24 hours leaves again (pass-through)"
  | Mule_fan_in ->
    "money from many different payers leaves to one or two payees, or one payee collects \
     from many of the bank's customers"
  | Dormant_reactivation -> "a large movement on an account silent for months"
  | High_risk_corridor ->
    "a payment to or from a jurisdiction on the higher-risk list, large or the first one"
  | Card_testing -> "a burst of tiny or declined card authorisations within minutes"
  | Atm_cash -> "more ATM operations or more cash in 24 hours than the limit"
  | Sanctions_adjacent ->
    "a payment touching a jurisdiction under sectoral restrictive measures"
  | Threshold_report ->
    "cash at or above the reporting threshold in 24 hours: a threshold report is due"
  | Unusual_amount -> "an outgoing amount far above the customer's own 95th percentile"
;;

let reference = function
  | Structuring -> "FATF R.20; 31 CFR 1010.314 (structuring to evade reporting)"
  | Smurfing -> "FATF R.20; EBA/GL/2021/02 4.46 (unusual patterns); 31 CFR 1010.314"
  | Rapid_movement ->
    "FATF Professional Money Laundering (2018), layering; EBA/GL/2021/02 4.72-4.75 \
     (transaction monitoring)"
  | Mule_fan_in ->
    "FATF Illicit Financial Flows from Cyber-Enabled Fraud (2023), money mules; FATF \
     Professional Money Laundering (2018)"
  | Dormant_reactivation ->
    "EBA/GL/2021/02 4.46 (unusual-pattern transactions); AMLR (EU) 2024/1624 Art. 26 \
     (ongoing monitoring)"
  | High_risk_corridor ->
    "FATF R.19 (higher-risk countries); AMLR (EU) 2024/1624 Arts. 29-31 (high-risk third \
     countries)"
  | Card_testing -> "RTS (EU) 2018/389 Art. 2(2) (known fraud scenarios)"
  | Atm_cash ->
    "FATF Professional Money Laundering (2018), mule cash withdrawals; EBA/GL/2021/02 \
     Guideline 2 (cash-intensive activity)"
  | Sanctions_adjacent ->
    "FATF R.6 and R.7 (targeted financial sanctions); AMLR (EU) 2024/1624 Art. 20(1)(d)"
  | Threshold_report ->
    "31 CFR 1010.311 (currency transaction report above USD 10 000); AMLR (EU) 2024/1624 \
     Art. 80 (EUR 10 000 cash limit)"
  | Unusual_amount ->
    "EBA/GL/2021/02 4.46 (unusually large transactions); AMLR (EU) 2024/1624 Art. 34"
;;
