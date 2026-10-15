(** A from-scratch recomputation of {!Transaction_monitoring.Profile.Customer.view} over
    the whole event history, written as plainly as possible (filters over a list), so the
    property tests can check the incremental profile against it. *)

open! Core
open Transaction_monitoring

(** [customer_view params ~history event] is the view of [event] after applying
    [history] (oldest first). Assumes fewer distinct counterparties and devices than the
    profile's tables hold. *)
val customer_view : Profile.Params.t -> history:Event.t list -> Event.t -> Profile.Customer.View.t
