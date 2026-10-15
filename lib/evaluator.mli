(** The synchronous core: event in, decision out.

    [decide] reads the customer's and counterparty's profiles (as views before the event),
    runs the typology rules, scores the features with the model when the policy uses one,
    and combines everything under the policy. [apply] then folds the event into both
    profiles. Nothing here does I/O or reads a clock; {!Monitor} adds storage and Async
    around it, {!Replay} runs it over a history.

    An evaluator owns a scratch feature vector, so one evaluator must not be used from two
    Async jobs at once (the monitor serializes; replays are sequential). *)

open! Core

type t

(** Validates the config and, with a model, the policy's thresholds against it. *)
val create : ?model:Model.t -> ?policy:Policy.t -> Rules.Config.t -> t Or_error.t

val config : t -> Rules.Config.t
val policy : t -> Policy.t
val model : t -> Model.t option

val context
  :  Event.t
  -> customer:Profile.Customer.t
  -> counterparty:Profile.Counterparty.t
  -> Context.t

val decide : t -> Context.t -> Decision.t

(** Folds the event into both profiles. *)
val apply
  :  Event.t
  -> customer:Profile.Customer.t
  -> counterparty:Profile.Counterparty.t
  -> unit

module Profiles : sig
  (** Profiles kept in memory, created on first sight: for replays, benches and tests. *)

  type t

  val create : Profile.Params.t -> t
  val customer : t -> Ids.Customer_id.t -> Profile.Customer.t
  val counterparty : t -> Ids.Counterparty_id.t -> Profile.Counterparty.t
  val customers : t -> int
  val counterparties : t -> int
end

(** [decide] then [apply] against in-memory profiles. *)
val process : t -> Profiles.t -> Event.t -> Decision.t
