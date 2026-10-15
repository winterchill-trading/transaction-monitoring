open! Core

(* 62 bits per word keeps every bit away from OCaml's sign bit. *)
let bits_per_word = 62
let words = (26 * 26 / bits_per_word) + 1

module Stable = struct
  module V1 = struct
    type t = int array [@@deriving bin_io, sexp, compare, equal]
  end
end

include Stable.V1

let create () = Array.create ~len:words 0
let copy = Array.copy

let index country =
  let s = Risk_rules.Country.to_string country in
  ((Char.to_int s.[0] - Char.to_int 'A') * 26) + (Char.to_int s.[1] - Char.to_int 'A')
;;

let mem t country =
  let i = index country in
  t.(i / bits_per_word) land (1 lsl (i mod bits_per_word)) <> 0
;;

let add t country =
  let i = index country in
  t.(i / bits_per_word) <- t.(i / bits_per_word) lor (1 lsl (i mod bits_per_word))
;;

let cardinal t = Array.sum (module Int) t ~f:Int.popcount
