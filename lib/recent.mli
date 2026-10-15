(** A bounded table of recently seen keys (counterparties, devices, customers) with when
    each was first and last seen in each direction.

    At most [capacity] keys are kept; inserting into a full table evicts the key seen
    least recently (ties: the earliest slot). Below capacity the table is exact. Lookups
    scan the table comparing a precomputed hash first, which for the small capacities used
    here (8 to 64) is faster than a hash table and allocates nothing. *)

open! Core

type t [@@deriving sexp_of]

val create : capacity:int -> t
val copy : t -> t
val capacity : t -> int
val length : t -> int

(** Whether [key] is in the table. *)
val mem : t -> string -> bool

(** When [key] was first seen, if it is in the table. *)
val first_seen : t -> string -> Time_ns.t option

(** [touch t key ~at ~inbound] records a sighting. [inbound] is from the profile owner's
    point of view (money coming in). *)
val touch : t -> string -> at:Time_ns.t -> inbound:bool -> unit

(** Keys seen inbound (or outbound) at or after [since]. *)
val count_since : t -> since:Time_ns.t -> inbound:bool -> int

(** Keys in the table, most recently seen first; for tests and debugging. *)
val keys : t -> string list

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal]
  end
end
