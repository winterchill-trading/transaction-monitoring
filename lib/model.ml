open! Core
module Scorer = Risk_models.Scorer

type t = Scorer.t [@@deriving sexp_of]

let load path = Scorer.load ~expected:Features.schema path
let of_json json = Scorer.of_yojson ~expected:Features.schema json

let of_string text =
  Or_error.try_with_join (fun () -> of_json (Yojson.Safe.from_string text))
;;

let bundled_json = Bundled_model_json.contents
let bundled_model = lazy (of_string bundled_json |> Or_error.tag ~tag:"bundled model")
let bundled () = Lazy.force bundled_model
let env_var = "TRANSACTION_MONITORING_MODEL"

let default () =
  match Sys.getenv env_var with
  | Some path when not (String.is_empty (String.strip path)) -> load path
  | Some _ | None -> bundled ()
;;

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

let explainer t = Risk_models.Explainer.create t
