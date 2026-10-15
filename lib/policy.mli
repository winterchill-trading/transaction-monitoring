(** How rule hits and the model score combine into a {!Decision.Outcome.t}.

    Rules: [Allow] allows, [Challenge] and [Review] hold for review, [Block] blocks. The
    model, when [model_mode] is [Active], holds at [hold_at] and blocks at [block_at]; in
    [Shadow] its score and reasons are recorded but change nothing; [Off] skips scoring.
    The worst outcome wins. Thresholds left out come from the model file (risk-models'
    [review] and [block], chosen from the alert budget and a precision target). *)

open! Core

module Model_mode : sig
  type t =
    | Off
    | Shadow
    | Active
  [@@deriving sexp, compare, equal, enumerate]
end

type t =
  { name : string (** recorded on every decision, e.g. ["default-2026-10"] *)
  ; model_mode : Model_mode.t
  ; hold_at : float option
  ; block_at : float option
  }
[@@deriving sexp, compare, equal]

(** ["default"], model [Shadow], thresholds from the model. *)
val default : t

(** A sexp file of [t]; missing fields take their default. *)
val load : string -> t Or_error.t

(** The thresholds in force for [model]: the policy's, else the model file's. Fails when
    [hold_at > block_at] or a value is outside \[0, 1\]. *)
val thresholds : t -> Model.t -> (float option * float option) Or_error.t
