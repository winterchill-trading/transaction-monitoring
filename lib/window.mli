(** A sliding window of counters in fixed time buckets, with bounded memory.

    The window keeps [buckets] buckets of [width] each, and in every bucket [fields]
    integer counters (counts, sums in minor units). Sums over the last [k] buckets are
    bucket-aligned: "the last hour" with 5-minute buckets covers the current bucket and
    the 11 before it, so it reaches between 55 and 60 minutes back. That approximation is
    what keeps memory constant per profile; it is exact and reproducible, which is what
    replays and the property tests rely on.

    Adding at a time later than the newest bucket advances the window and clears the
    buckets it skips; adding at a time older than the oldest bucket is ignored. *)

open! Core

type t [@@deriving sexp_of]

val create : width:Time_ns.Span.t -> buckets:int -> fields:int -> t

(** A deep copy. *)
val copy : t -> t

val width : t -> Time_ns.Span.t
val buckets : t -> int
val fields : t -> int

(** [add t ~at ~field v] adds [v] to counter [field] of [at]'s bucket. *)
val add : t -> at:Time_ns.t -> field:int -> int -> unit

(** [add_many t ~at values] adds [values.(i)] to counter [i] (fewer values than fields is
    fine; zeros are skipped). *)
val add_many : t -> at:Time_ns.t -> int array -> unit

(** [sum t ~now ~field ~last] is the sum of counter [field] over the [last] buckets
    ending with [now]'s bucket ([last] is clamped to [buckets]). *)
val sum : t -> now:Time_ns.t -> field:int -> last:int -> int

(** [sum] over the whole window. *)
val total : t -> now:Time_ns.t -> field:int -> int

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal]
  end
end
