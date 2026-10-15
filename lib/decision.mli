(** The monitoring verdict for one event: [Allow], [Hold] (for review) or [Block], with
    every reason, the model score, and whether a threshold report is due.

    A decision is a pure function of the event, the profiles before it, the rules config,
    the policy and the model, so replaying the same history gives the same decisions.
    [decided_at] is the event's [received_at], not a clock reading, for the same reason.
    Decisions are idempotent per event id: the {!Monitor} returns the stored decision for
    an event id it has seen. *)

open! Core

module Outcome : sig
  module Stable : sig
    module V1 : sig
      type t =
        | Allow
        | Hold
        | Block
      [@@deriving bin_io, sexp, compare, equal, hash]
    end
  end

  type t = Stable.V1.t =
    | Allow
    | Hold (** park the payment, open an alert, wait for an investigator *)
    | Block
  [@@deriving sexp, compare, equal, hash, enumerate]

  val to_string : t -> string
  val of_string : string -> t Or_error.t

  (** [Allow < Hold < Block]. *)
  val worst : t -> t -> t

  (** Allow for [Allow]; Hold for [Challenge] and [Review]; Block for [Block]. *)
  val of_severity : Risk_rules.Severity.t -> t
end

module Model_ref : sig
  module Stable : sig
    module V1 : sig
      type t =
        { name : string
        ; version : string
        ; shadow : bool (** the score was recorded but had no effect *)
        }
      [@@deriving bin_io, sexp, compare, equal, hash]
    end
  end

  type t = Stable.V1.t =
    { name : string
    ; version : string
    ; shadow : bool
    }
  [@@deriving sexp, compare, equal, hash]
end

module Stable : sig
  module V1 : sig
    type t =
      { event_id : Ids.Event_id.Stable.V1.t
      ; customer : Ids.Customer_id.Stable.V1.t
      ; outcome : Outcome.Stable.V1.t
      ; explanations : Explanation.Stable.V1.t list (** most severe first *)
      ; score : float option
      ; model : Model_ref.Stable.V1.t option
      ; threshold_report : bool
      ; policy : string
      ; decided_at : Time_ns.Stable.Alternate_sexp.V1.t
      }
    [@@deriving bin_io, sexp, compare, equal]
  end
end

type t = Stable.V1.t =
  { event_id : Ids.Event_id.t
  ; customer : Ids.Customer_id.t
  ; outcome : Outcome.t
  ; explanations : Explanation.t list
  ; score : float option
  ; model : Model_ref.t option
  ; threshold_report : bool
  ; policy : string
  ; decided_at : Time_ns.t
  }
[@@deriving sexp_of, compare, equal]

(** Combines a rules decision and an optional model score under [policy]. *)
val create
  :  Policy.t
  -> event:Event.t
  -> rules:Risk_rules.Decision.t
  -> model:(Model.t * float * (string * float) list) option
  -> t Or_error.t

(** The typologies among the explanations, without duplicates. *)
val typologies : t -> Typology.t list

(** What a customer-facing system may say: ["under review"] for a hold, a generic refusal
    for a block, nothing for allow. Never the reasons (tipping off). *)
val customer_status : t -> string option
