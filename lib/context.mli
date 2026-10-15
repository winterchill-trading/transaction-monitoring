(** What the typology rules see for one event: the event, the customer's and the
    counterparty's profile views taken before the event is applied, and the event's
    amount in minor units of the reporting currency. Plain data, so a rule outcome can be
    reproduced from a journaled context. *)

open! Core

type t =
  { event : Event.t
  ; amount : int (** [Event.base_minor event] *)
  ; customer : Profile.Customer.View.t
  ; counterparty : Profile.Counterparty.View.t
  }
[@@deriving sexp_of]

val create
  :  Event.t
  -> customer:Profile.Customer.View.t
  -> counterparty:Profile.Counterparty.View.t
  -> t

(** [Some span] since the account was opened ([Event.account_opened_at], else the first
    event the profile saw). *)
val account_age : t -> Time_ns.Span.t option

val is_outbound : t -> bool
