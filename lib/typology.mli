(** The money-laundering and fraud typologies this library detects, each with the public
    source it implements (FATF Recommendations and reports, EBA guidelines, EU
    regulations, US regulations). Thresholds and windows are this library's defaults
    ({!Rules.Config}), not figures taken from those sources. *)

open! Core

module Stable : sig
  module V1 : sig
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
    [@@deriving bin_io, sexp, compare, equal, hash]
  end
end

type t = Stable.V1.t =
  | Structuring (** one customer splitting amounts to stay under a reporting line *)
  | Smurfing (** many people paying one account in amounts under the line *)
  | Rapid_movement (** money in and straight out again (pass-through) *)
  | Mule_fan_in (** many payers in, one or two payees out *)
  | Dormant_reactivation (** a long-silent account suddenly moving large amounts *)
  | High_risk_corridor (** payments to or from a higher-risk jurisdiction *)
  | Card_testing (** bursts of tiny or declined card authorisations *)
  | Atm_cash (** many or large ATM operations in a day *)
  | Sanctions_adjacent
  (** payments touching a jurisdiction under sectoral restrictive measures *)
  | Threshold_report
  (** cash at or above the reporting threshold: report, not suspicion *)
  | Unusual_amount (** far above the customer's own outgoing amounts *)
[@@deriving sexp, compare, equal, hash, enumerate, quickcheck]

(** The rule name, ["structuring"], ["mule_fan_in"], ...; also the [Hit.rule] of the hits
    it produces. *)
val name : t -> string

val of_name : string -> t option

(** The risk-rules reason code: the built-in one where risk-rules has it ([STRUCTURING],
    [MULE_PASS_THROUGH], [MULE_FAN_IN], [CARD_TESTING], [GEO_HIGH_RISK]), otherwise
    [CUSTOM_TM_<NAME>]. *)
val code : t -> Risk_rules.Reason_code.t

(** The closest risk-rules typology, for hits and audit grouping. *)
val family : t -> Risk_rules.Typology.t

(** One line for operators. *)
val description : t -> string

(** The public source the rule implements, e.g. ["FATF R.20; 31 CFR 1010.314"]. *)
val reference : t -> string
