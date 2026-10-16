(** Where monitoring state lives, in domain types: the events seen, one decision per
    event, alerts with their case history, the latest profile snapshots, and the model
    versions put in service.

    Two implementations: {!Store_memory} (tests, replays, single-process demos) and
    {!Store_pg} (PostgreSQL through pg-async, its own schema [transaction_monitoring]). No
    SQL or table name exists outside [store_pg.ml] and [migrations/]; the conformance
    suite in [test/] runs the same scenarios against both.

    Writes are keyed and idempotent. {!S.commit} stores an event's decision, its alert and
    both profile snapshots in one transaction, keyed by event id: a second commit of the
    same event id writes nothing and returns the decision already stored. A profile
    snapshot only replaces a stored one with fewer events, so a slow writer cannot roll a
    profile back. Alert updates are optimistic: they apply only over the version they were
    computed from. Reads never assume they see another instance's latest write. *)

open! Core
open! Async

module Commit : sig
  type t =
    { event : Event.t
    ; decision : Decision.t
    ; alert : (Alert.t * Alert.Event.t list) option
    ; customer : Profile.Customer.snapshot
    ; counterparty : Profile.Counterparty.snapshot
    }
end

module Commit_result : sig
  type t =
    | Committed
    | Duplicate of Decision.t (** the event id was committed before: this decision *)
  [@@deriving sexp_of]
end

module Alert_update : sig
  type t =
    | Updated
    | Conflict of Alert.t option
    (** the stored alert is not at the version the update started from (or is missing) *)
  [@@deriving sexp_of]
end

module Model_version : sig
  (** A model put in service, with the policy it runs under. *)
  type t =
    { name : string
    ; version : string
    ; policy : string
    ; activated_at : Time_ns.Alternate_sexp.t
    }
  [@@deriving sexp, compare, equal]
end

module Revocation : sig
  (** A decided event that did not happen after all (the customer cancelled, the scheme
      rejected the payment, the card authorisation was reversed before capture). *)
  type t =
    { event_id : Ids.Event_id.t
    ; reason : string (** free text for the audit trail, e.g. ["scheme rejected"] *)
    ; revoked_at : Time_ns.Alternate_sexp.t
    }
  [@@deriving sexp, compare, equal]
end

module Revoke_result : sig
  type t =
    | Revoked
    | Already_revoked of Revocation.t (** the stored revocation, unchanged *)
    | Unknown_event (** no event with this id was committed *)
  [@@deriving sexp_of]
end

module type S = sig
  type t

  (** ["memory"] or ["postgres ..."], for logs. *)
  val kind : t -> string

  val find_decision : t -> Ids.Event_id.t -> Decision.t option Or_error.t Deferred.t
  val commit : t -> Commit.t -> Commit_result.t Or_error.t Deferred.t

  val load_customer
    :  t
    -> Ids.Customer_id.t
    -> Profile.Customer.snapshot option Or_error.t Deferred.t

  val load_counterparty
    :  t
    -> Ids.Counterparty_id.t
    -> Profile.Counterparty.snapshot option Or_error.t Deferred.t

  val find_alert : t -> Ids.Alert_id.t -> Alert.t option Or_error.t Deferred.t

  (** Stores [alert] and appends [events] if the stored alert's version is
      [alert.version - List.length events]. *)
  val update_alert
    :  t
    -> Alert.t
    -> Alert.Event.t list
    -> Alert_update.t Or_error.t Deferred.t

  (** Oldest first (opened, then id). *)
  val alerts
    :  ?status:Alert.Status.t
    -> ?customer:Ids.Customer_id.t
    -> t
    -> Alert.t list Or_error.t Deferred.t

  (** In the order they happened. *)
  val alert_events : t -> Ids.Alert_id.t -> Alert.Event.t list Or_error.t Deferred.t

  (** Oldest first (decided, then event id). *)
  val decisions
    :  ?customer:Ids.Customer_id.t
    -> t
    -> Decision.t list Or_error.t Deferred.t

  (** Records a model version; repeating a version changes nothing. *)
  val record_model : t -> Model_version.t -> unit Or_error.t Deferred.t

  (** The most recently activated model (ties by version). *)
  val current_model : t -> Model_version.t option Or_error.t Deferred.t

  (** A committed event and its revocation, if it was revoked. *)
  val find_event
    :  t
    -> Ids.Event_id.t
    -> (Event.t * Revocation.t option) option Or_error.t Deferred.t

  (** The customer's committed events that are not revoked, in the order they were
      committed (the order the profile saw them). *)
  val customer_history : t -> Ids.Customer_id.t -> Event.t list Or_error.t Deferred.t

  (** The same for a counterparty, across every customer. *)
  val counterparty_history
    :  t
    -> Ids.Counterparty_id.t
    -> Event.t list Or_error.t Deferred.t

  (** Records the revocation and replaces both profile snapshots with the given ones (the
      profiles rebuilt without the event), in one transaction, whatever their event
      counts: the rebuilt profiles have one event fewer than the stored ones. Idempotent
      by event id. The event and its decision stay stored, so a redelivery still finds the
      decision and is not applied again. *)
  val revoke
    :  t
    -> Revocation.t
    -> customer:Profile.Customer.snapshot
    -> counterparty:Profile.Counterparty.snapshot
    -> Revoke_result.t Or_error.t Deferred.t

  val close : t -> unit Deferred.t
end

(** A store of either kind, for code that picks one at start-up. *)
type t = T : (module S with type t = 'a) * 'a -> t
