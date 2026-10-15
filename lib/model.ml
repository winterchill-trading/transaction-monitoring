open! Core
module Scorer = Risk_models.Scorer

type t = Scorer.t [@@deriving sexp_of]

let load path = Scorer.load ~expected:Features.schema path

let of_scorer s =
  Risk_models.Schema.check_compatible ~expected:Features.schema (Scorer.schema s)
  |> Or_error.map ~f:(fun () -> s)
;;

let scorer t = t
let name = Scorer.name
let version = Scorer.version
let thresholds = Scorer.thresholds
let score t x = Scorer.score t x

let explain t x =
  let s = Scorer.explain t x in
  s.value, s.reasons
;;
