(** A trained model scoring {!Features} vectors: a risk-models [Scorer.t] whose schema is
    {!Features.schema}. Loading refuses a model trained on another feature layout. *)

open! Core

type t [@@deriving sexp_of]

(** Reads a risk-models [model.json] (as written by [ml/train.py]). *)
val load : string -> t Or_error.t

(** A model from the text of a [model.json], without touching the file system. The version
    is the same as {!load} gives for that file. *)
val of_string : string -> t Or_error.t

(** A model from an already parsed [model.json]. *)
val of_json : Yojson.Safe.t -> t Or_error.t

(** The model shipped with the library ([models/tm-aml-gbdt], trained on synthetic data
    only; see the README), embedded at build time: no file, no path. Parsed once. *)
val bundled : unit -> t Or_error.t

(** The text of the bundled [model.json]. The package also installs it as
    [share/transaction-monitoring/models/tm-aml-gbdt/model.json]. *)
val bundled_json : string

(** The environment variable {!default} reads: ["TRANSACTION_MONITORING_MODEL"]. *)
val env_var : string

(** The model at the path in {!env_var} when it is set and not empty, else {!bundled}.
    Lets a deployment swap in a retrained model without code changes. *)
val default : unit -> t Or_error.t

val of_scorer : Risk_models.Scorer.t -> t Or_error.t
val scorer : t -> Risk_models.Scorer.t
val name : t -> string

(** First 12 hex digits of the MD5 of the model file. *)
val version : t -> string

val thresholds : t -> Risk_models.Scorer.Thresholds.t option

(** Calibrated score in \[0, 1\]; allocates nothing. *)
val score : t -> float array -> float

(** Calibrated score with the top three reasons (features and their contribution to the
    log-odds). *)
val explain : t -> float array -> float * (string * float) list

(** Exact per-feature contributions for this model (path-dependent tree attributions).
    Fails for an export without node covers. An explainer owns scratch space: one per
    evaluator. *)
val explainer : t -> Risk_models.Explainer.t Or_error.t
