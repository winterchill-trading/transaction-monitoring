(** The typology library, as risk-rules rules over {!Context.t}.

    Every rule is a [Risk_rules.Rule.t] named after its {!Typology}, with the typology's
    reason code; its reason names the facts it compared, and its evidence lists them.
    {!evaluate} runs them all with risk-rules' [Engine] (every rule runs, a raising rule
    fails closed as a [Block]) and files each hit under the typology's risk-rules family.

    | Typology | Triggers when (defaults) | Severity |
    |---|---|---|
    | structuring | 3 or more amounts in \[8 500, 10 000) within 7 days, this one included | Review |
    | smurfing | an inbound payment under 10 000 from the 4th or later distinct payer in 24 h, inbound total at or above 10 000 | Review |
    | rapid_movement | outbound, at least 1 000 came in within 24 h and outbound in 24 h reaches 80% of it | Review |
    | mule_fan_in | outbound to at most 2 payees in 24 h after 5 or more payers in 7 days, account younger than 90 days or paying out at least half of the inflow; or a payee collecting from 5 or more customers in 7 days | Review |
    | dormant_reactivation | 5 000 or more after 180 days without activity | Review |
    | high_risk_corridor | counterparty in a higher-risk country, 1 000 or more or the first time | Review |
    | card_testing | 5 or more small card attempts, or 3 or more declines, within 10-15 min | Block |
    | atm_cash | more than 6 ATM operations or 3 000 in 24 h | Review |
    | sanctions_adjacent | counterparty or IP country under sectoral restrictive measures | Review |
    | threshold_report | cash in 24 h, this one included, reaches 10 000 | Allow (report) |
    | unusual_amount | outbound 3 000 or more and 10 times the customer's p95, with 10 or more earlier events | Review |

    Amounts are in the reporting currency. *)

open! Core

module Config : sig
  type t =
    { profile : Profile.Params.t (** the threshold, band, round unit, small card *)
    ; structuring_min_count : int
    ; smurfing_min_payers : int
    ; rapid_min_inbound : int
    ; rapid_share_bps : int
    ; mule_min_payers : int
    ; mule_max_payees : int
    ; mule_max_account_age : Time_ns.Span.t
    ; mule_collector_min_customers : int
    ; dormant_after : Time_ns.Span.t
    ; dormant_min_amount : int
    ; high_risk_countries : Risk_rules.Country.t list
    ; corridor_min_amount : int
    ; sanctions_adjacent_countries : Risk_rules.Country.t list
    ; card_testing_min_small : int
    ; card_testing_min_declines : int
    ; atm_max_count_24h : int
    ; atm_max_sum_24h : int
    ; unusual_multiple : float
    ; unusual_min_amount : int
    ; unusual_min_history : int
    ; disabled : Typology.t list (** not run at all *)
    ; shadow : Typology.t list (** run with hits capped at Allow: logged, no effect *)
    }
  [@@deriving sexp, compare, equal]

  (** The defaults in the table above. Country lists: higher-risk = the FATF call for
      action (KP, IR, MM); sectoral restrictive measures = RU, BY. Both are examples to be
      replaced by lists the compliance function maintains. *)
  val default : t

  (** Positive counts and amounts, a band in (0, 10 000) bps, positive spans. *)
  val validate : t -> t Or_error.t

  (** A sexp file of [t]; missing fields take their default. *)
  val load : string -> t Or_error.t
end

(** One rule per typology. *)
val rule : Config.t -> Typology.t -> Context.t Risk_rules.Rule.t

(** The enabled rules, shadow ones capped at Allow. *)
val all : Config.t -> Context.t Risk_rules.Rule.t list

(** Runs {!all} through [Risk_rules.Engine.evaluate] and files each hit under its
    typology's family. *)
val evaluate : Config.t -> Context.t -> Risk_rules.Decision.t

(** Whether the decision carries a threshold report hit. *)
val requires_threshold_report : Risk_rules.Decision.t -> bool
