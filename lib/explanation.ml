open! Core

module Source = struct
  module Stable = struct
    module V1 = struct
      type t =
        | Rule of string
        | Model of
            { name : string
            ; version : string
            }
      [@@deriving bin_io, sexp, compare, equal, hash]
    end
  end

  include Stable.V1
end

module Stable = struct
  module V1 = struct
    type t =
      { source : Source.Stable.V1.t
      ; typology : Typology.Stable.V1.t option
      ; severity : Risk_rules.Severity.Stable.V1.t
      ; code : string
      ; summary : string
      ; facts : (string * string) list
      ; reference : string
      }
    [@@deriving bin_io, sexp, compare, equal, hash]
  end
end

include Stable.V1

let of_hit (hit : Risk_rules.Hit.t) =
  let typology = Typology.of_name hit.rule in
  { source = Rule hit.rule
  ; typology
  ; severity = hit.severity
  ; code = Risk_rules.Reason_code.to_string hit.code
  ; summary = hit.reason
  ; facts = hit.evidence
  ; reference = Option.value_map typology ~f:Typology.reference ~default:""
  }
;;

let of_score ~name ~version ~severity ~score ~reasons =
  { source = Model { name; version }
  ; typology = None
  ; severity
  ; code = Risk_rules.Reason_code.(to_string Score_high)
  ; summary =
      sprintf
        "model score %.4f; top reasons %s"
        score
        (List.map reasons ~f:fst |> String.concat ~sep:", ")
  ; facts =
      ("score", sprintf "%.6f" score)
      :: List.map reasons ~f:(fun (feature, c) -> "reason:" ^ feature, sprintf "%+.4f" c)
  ; reference = "model metrics in models/<name>/metrics.json"
  }
;;

let uncertain_code = "CUSTOM_TM_MODEL_UNCERTAIN"

let of_uncertain ~name ~version ~severity ~score ~band:(lo, hi) ~threshold ~held =
  { source = Model { name; version }
  ; typology = None
  ; severity
  ; code = uncertain_code
  ; summary =
      sprintf
        "model uncertain: score %.6f in the abstain band [%.6f, %.6f) around the hold \
         threshold %.6f; %s"
        score
        lo
        hi
        threshold
        (if held then "held for review" else "the rules decide")
  ; facts =
      [ "score", sprintf "%.6f" score
      ; "band_lower", sprintf "%.6f" lo
      ; "band_upper", sprintf "%.6f" hi
      ]
  ; reference = "policy abstain settings; accuracy-coverage table from replay"
  }
;;

let is_uncertain t = String.equal t.code uncertain_code

let to_string_hum t =
  let who =
    match t.source with
    | Rule r -> r
    | Model { name; version } -> sprintf "%s@%s" name version
  in
  sprintf
    "[%s] %s: %s"
    (Risk_rules.Severity.to_string t.severity |> String.lowercase)
    who
    t.summary
;;
