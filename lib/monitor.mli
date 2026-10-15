(** The live pipeline: [on_event] for every payment initiated or booked.

    For each event: return the stored decision if the event id was decided before
    (idempotent across redeliveries and restarts); otherwise load the customer's and the
    counterparty's profiles (cache, then store, then empty), decide with the
    {!Evaluator}, fold the event into the profiles, and commit event, decision, alert
    and both snapshots to the store in one write; then hand the decision to
    [on_decision] and return it.

    Backpressure: events are processed one at a time in arrival order (profiles must
    see events in order, and the work is tens of microseconds plus one store write).
    At most [max_pending] events wait; beyond that [on_event] answers [Overloaded] at
    once, and the caller applies its own fail-closed choice (hold the payment, retry
    the inbound booking later). [on_decision] is awaited before the next event starts,
    so a slow consumer slows intake instead of growing a buffer.

    Profiles stay in an LRU cache of [cache_capacity] customers and counterparties; a
    failed or duplicate commit evicts the two profiles it touched so they are reloaded
    from the store. *)

open! Core
open! Async

module Reply : sig
  type t =
    | Decided of Decision.t
    | Already_decided of Decision.t (** the event id was decided before *)
    | Overloaded (** too many events waiting; nothing was recorded *)
  [@@deriving sexp_of]

  (** The decision, if any. *)
  val decision : t -> Decision.t option
end

type t

(** Records the evaluator's model (name, version, policy) in the store as activated
    [now], if it has one. *)
val create
  :  ?cache_capacity:int (** default 100 000 *)
  -> ?max_pending:int (** default 10 000 *)
  -> ?on_decision:(Decision.t -> unit Deferred.t)
  -> now:Time_ns.t
  -> store:Store.t
  -> Evaluator.t
  -> t Or_error.t Deferred.t

val on_event : t -> Event.t -> Reply.t Or_error.t Deferred.t

(** Events accepted and not yet finished. *)
val pending : t -> int

(** Loads the alert, applies the command, stores the result over the version it read;
    on a concurrent update it retries once on the fresh alert. *)
val apply_alert
  :  t
  -> Ids.Alert_id.t
  -> Alert.Command.t
  -> (Alert.t * Alert.Event.t list) Or_error.t Deferred.t

(** Open, assigned (or the given status) alerts, oldest first: the review queue. *)
val review_queue : ?status:Alert.Status.t -> t -> Alert.t list Or_error.t Deferred.t

val store : t -> Store.t
val evaluator : t -> Evaluator.t
