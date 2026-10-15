(** Model features for one event, computed from the event and the profile views in a
    {!Context.t}: the only feature code, used for scoring, for the training export
    ([transaction-monitoring features]) and in replays, so training rows and live scores
    can never disagree on what a feature means.

    The vector is risk-rules' feature record ({!risk_rules}, laid out by
    [Risk_models.Encoding]) followed by AML features risk-rules does not define
    ({!extra_names}): dormancy, round-amount share, amounts just under the threshold,
    cash, card bursts, counterparty and country novelty, list membership, the amount
    against the customer's own distribution, inflow and outflow balance, and the
    counterparty's own profile. Missing values are NaN. *)

open! Core

(** The risk-rules feature record for this event. Amounts are major units of the
    reporting currency; fields that need data this library does not have (balance, SCA
    failures, verification of payee, credential changes) are missing. *)
val risk_rules : Context.t -> Risk_rules.Features.t

(** Names of the AML features, in vector order after the risk-rules columns. *)
val extra_names : string list

(** The AML features with their names, in {!extra_names} order. *)
val extras : Rules.Config.t -> Context.t -> (string * float) list

(** ["transaction-monitoring-features"], version 1: [Risk_models.Encoding.schema]'s
    columns, then {!extra_names}. *)
val schema : Risk_models.Schema.t

(** Fills a caller-owned array of [Risk_models.Schema.length schema] floats. The country
    lists come from the rules config. *)
val encode_into : Rules.Config.t -> Context.t -> float array -> unit

val encode : Rules.Config.t -> Context.t -> float array

(** [schema] names with values, for debugging and tests. *)
val to_named : Rules.Config.t -> Context.t -> (string * float) list
