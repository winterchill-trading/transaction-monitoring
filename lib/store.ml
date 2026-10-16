open! Core
open! Async

module Commit = struct
  type t =
    { event : Event.t
    ; decision : Decision.t
    ; alert : (Alert.t * Alert.Event.t list) option
    ; customer : Profile.Customer.snapshot
    ; counterparty : Profile.Counterparty.snapshot
    }
end

module Commit_result = struct
  type t =
    | Committed
    | Duplicate of Decision.t
  [@@deriving sexp_of]
end

module Alert_update = struct
  type t =
    | Updated
    | Conflict of Alert.t option
  [@@deriving sexp_of]
end

module Model_version = struct
  type t =
    { name : string
    ; version : string
    ; policy : string
    ; activated_at : Time_ns.Alternate_sexp.t
    }
  [@@deriving sexp, compare, equal]
end

module Revocation = struct
  type t =
    { event_id : Ids.Event_id.t
    ; reason : string
    ; revoked_at : Time_ns.Alternate_sexp.t
    }
  [@@deriving sexp, compare, equal]
end

module Revoke_result = struct
  type t =
    | Revoked
    | Already_revoked of Revocation.t
    | Unknown_event
  [@@deriving sexp_of]
end

module type S = sig
  type t

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

  val update_alert
    :  t
    -> Alert.t
    -> Alert.Event.t list
    -> Alert_update.t Or_error.t Deferred.t

  val alerts
    :  ?status:Alert.Status.t
    -> ?customer:Ids.Customer_id.t
    -> t
    -> Alert.t list Or_error.t Deferred.t

  val alert_events : t -> Ids.Alert_id.t -> Alert.Event.t list Or_error.t Deferred.t

  val decisions
    :  ?customer:Ids.Customer_id.t
    -> t
    -> Decision.t list Or_error.t Deferred.t

  val record_model : t -> Model_version.t -> unit Or_error.t Deferred.t
  val current_model : t -> Model_version.t option Or_error.t Deferred.t

  val find_event
    :  t
    -> Ids.Event_id.t
    -> (Event.t * Revocation.t option) option Or_error.t Deferred.t

  val customer_history : t -> Ids.Customer_id.t -> Event.t list Or_error.t Deferred.t

  val counterparty_history
    :  t
    -> Ids.Counterparty_id.t
    -> Event.t list Or_error.t Deferred.t

  val revoke
    :  t
    -> Revocation.t
    -> customer:Profile.Customer.snapshot
    -> counterparty:Profile.Counterparty.snapshot
    -> Revoke_result.t Or_error.t Deferred.t

  val close : t -> unit Deferred.t
end

type t = T : (module S with type t = 'a) * 'a -> t
