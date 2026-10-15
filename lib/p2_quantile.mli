(** Streaming quantile estimate with the P-square algorithm (Jain and Chlamtac, 1985):
    five markers, constant memory, no stored observations after the fifth.

    Until five values have been seen the estimate is the exact nearest-rank quantile of
    those values. The estimate always lies between the smallest and largest value seen.
    It is deterministic: the same values in the same order give the same estimate, so a
    snapshot taken mid-stream and resumed equals an uninterrupted run. *)

open! Core

type t [@@deriving sexp_of]

(** [p] in (0, 1), e.g. 0.95. *)
val create : p:float -> t

val copy : t -> t
val p : t -> float
val count : t -> int
val add : t -> float -> unit

(** [None] before the first value. *)
val estimate : t -> float option

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal]
  end
end
