(** Re-runs a history of events through a candidate evaluator (typically a new model
    version or policy), optionally next to a baseline, from empty profiles, and reports
    what would have been decided and what changed.

    Profiles do not depend on decisions, so both evaluators read the same profiles: each
    event is decided by the baseline and the candidate, then applied once. A replay is
    deterministic: the same events, config, policy and model give the same report. When
    events carry labels (synthetic data, or confirmed cases joined by the caller), the
    report includes how many labelled events each side held or blocked. *)

open! Core

module Side : sig
  type t =
    { decisions : int
    ; allow : int
    ; hold : int
    ; block : int
    ; threshold_reports : int
    ; alerts_by_typology : (string * int) list
    (** alerts per typology name, plus ["model"] for alerts raised by the score alone and
        ["model_uncertain"] for abstentions routed to a hold *)
    ; labelled : int (** events with a label *)
    ; labelled_alerted : int (** of those, held or blocked *)
    ; unlabelled_alerted : int (** alerts on events without a label *)
    ; abstained : int (** decisions where the model abstained *)
    }
  [@@deriving sexp_of]
end

module Change : sig
  type t =
    { event_id : Ids.Event_id.t
    ; baseline : Decision.Outcome.t
    ; candidate : Decision.Outcome.t
    ; baseline_score : float option
    ; candidate_score : float option
    ; label : Typology.t option
    ; why : string list (** the candidate's {!Decision.why} *)
    }
  [@@deriving sexp_of]
end

module Report : sig
  type t =
    { candidate : Side.t
    ; baseline : Side.t option
    ; changed : int (** events whose outcome differs *)
    ; new_alerts : int (** held or blocked by the candidate only *)
    ; cleared_alerts : int (** held or blocked by the baseline only *)
    ; sample_changes : Change.t list (** the first [max_changes] changes *)
    ; accuracy_coverage : Risk_models.Abstain.Curve.t option
    (** with labels and a model: accuracy, precision and recall of the model's decision at
        its hold threshold when it abstains on the scores closest to it, per coverage
        (logit-margin confidence); the table to choose [Policy.abstain]'s [min_confidence]
        from *)
    }
  [@@deriving sexp_of]

  val to_string_hum : t -> string
end

type t

val create : ?baseline:Evaluator.t -> ?max_changes:int -> Evaluator.t -> t

(** Decides one event with the candidate (and baseline), then applies it to the shared
    profiles. Returns the candidate's decision. *)
val step : ?label:Typology.t -> t -> Event.t -> Decision.t

val report : t -> Report.t
