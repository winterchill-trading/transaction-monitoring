(** Event builders and generators shared by the tests. *)

open! Core
open Transaction_monitoring

(** 2026-03-02 09:00:00 UTC, a Monday. *)
val t0 : Time_ns.t

(** [at_min m] is [t0] plus [m] minutes. *)
val at_min : float -> Time_ns.t

(** An event in EUR (base currency EUR) with defaults for everything not given:
    customer ["cust-1"], counterparty ["cp-1"], SEPA, outbound, not declined. [amount] is
    in cents. *)
val event
  :  ?id:string
  -> ?customer:string
  -> ?counterparty:string
  -> ?counterparty_country:string
  -> ?rail:Event.Rail.t
  -> ?direction:Event.Direction.t
  -> ?card_channel:Event.Card_channel.t
  -> ?mcc:int
  -> ?declined:bool
  -> ?device_id:string
  -> ?ip_country:string
  -> ?residence:string
  -> ?account_opened_at:Time_ns.t
  -> at:Time_ns.t
  -> int
  -> Event.t

(** Streams of events for one customer: random gaps (sometimes slightly negative, to
    exercise clamping), at most 20 counterparties, 6 countries and 5 devices, amounts that
    hit round values and the band just under 10 000.00. *)
val stream_generator : Event.t list Quickcheck.Generator.t
