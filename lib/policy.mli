(** How rule hits and the model score combine into a {!Decision.Outcome.t}.

    Rules: [Allow] allows, [Challenge] and [Review] hold for review, [Block] blocks. The
    model, when [model_mode] is [Active], holds at [hold_at] and blocks at [block_at]; in
    [Shadow] its score and reasons are recorded but change nothing; [Off] skips scoring.
    The worst outcome wins. Thresholds left out come from the model file (risk-models'
    [review] and [block], chosen from the alert budget and a precision target).

    With [abstain], the model abstains on scores too close to [hold_at] to trust (a band
    around the hold threshold, see [Risk_models.Abstain]); [replay] prints the
    accuracy-coverage table to choose [min_confidence] from. An abstention either holds
    the payment with reason "model uncertain" ([Hold]) or leaves the decision to the rules
    ([Rules_only]). A score at or above [block_at] never abstains. In [Shadow] abstentions
    are recorded on the decision and change nothing. *)

open! Core

module Model_mode : sig
  type t =
    | Off
    | Shadow
    | Active
  [@@deriving sexp, compare, equal, enumerate]
end

module Abstain : sig
  module Route : sig
    type t =
      | Hold (** hold for review with reason "model uncertain" *)
      | Rules_only (** the model has no say; the rules decide *)
    [@@deriving sexp, compare, equal, enumerate]
  end

  type t =
    { min_confidence : float (** abstain below this distance from [hold_at] *)
    ; confidence : Risk_models.Abstain.Confidence.t (** default [Logit_margin] *)
    ; route : Route.t
    }
  [@@deriving sexp, compare, equal]
end

type t =
  { name : string (** recorded on every decision, e.g. ["default-2026-10"] *)
  ; model_mode : Model_mode.t
  ; hold_at : float option
  ; block_at : float option
  ; abstain : Abstain.t option
  }
[@@deriving sexp, compare, equal]

(** ["default"], model [Shadow], thresholds from the model. *)
val default : t

(** A sexp file of [t]; missing fields take their default. *)
val load : string -> t Or_error.t

(** The thresholds in force for [model]: the policy's, else the model file's. Fails when
    [hold_at > block_at], a value is outside \[0, 1\], or [abstain] is set without a hold
    threshold or with an invalid band. *)
val thresholds : t -> Model.t -> (float option * float option) Or_error.t

(** The abstention rule around [hold_at], when the policy has one. Raises on a policy that
    {!thresholds} rejected. *)
val abstainer : t -> hold_at:float -> Risk_models.Abstain.t option
