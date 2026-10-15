(** A set of ISO 3166-1 alpha-2 country codes as a 676-bit bitmap: exact, constant size
    (11 words), no allocation on lookup. *)

open! Core

type t [@@deriving sexp_of]

val create : unit -> t
val copy : t -> t
val mem : t -> Risk_rules.Country.t -> bool
val add : t -> Risk_rules.Country.t -> unit
val cardinal : t -> int

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal]
  end
end
