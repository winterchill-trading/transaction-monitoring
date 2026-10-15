(** A trained model scoring {!Features} vectors: a risk-models [Scorer.t] whose schema is
    {!Features.schema}. Loading refuses a model trained on another feature layout. *)

open! Core

type t [@@deriving sexp_of]

(** Reads a risk-models [model.json] (as written by [ml/train.py]). *)
val load : string -> t Or_error.t

val of_scorer : Risk_models.Scorer.t -> t Or_error.t
val scorer : t -> Risk_models.Scorer.t
val name : t -> string

(** First 12 hex digits of the MD5 of the model file. *)
val version : t -> string

val thresholds : t -> Risk_models.Scorer.Thresholds.t option

(** Calibrated score in \[0, 1\]; allocates nothing. *)
val score : t -> float array -> float

(** Calibrated score with the top three reasons (features and their contribution to
    the log-odds). *)
val explain : t -> float array -> float * (string * float) list
