open! Core

type t =
  { event : Event.t
  ; amount : int
  ; customer : Profile.Customer.View.t
  ; counterparty : Profile.Counterparty.View.t
  }
[@@deriving sexp_of]

let create (event : Event.t) ~customer ~counterparty =
  { event; amount = Event.base_minor event; customer; counterparty }
;;

let account_age t =
  let opened = Option.first_some t.event.account_opened_at t.customer.first_seen in
  Option.map opened ~f:(fun opened -> Time_ns.diff t.customer.now opened)
;;

let is_outbound t =
  match t.event.direction with
  | Outbound -> true
  | Inbound -> false
;;
