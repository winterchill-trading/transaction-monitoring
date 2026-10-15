(** Why a rule or the model objected, in a form an investigator and an auditor can read:
    the typology, the reason code, a one-line summary naming the facts compared, the facts
    themselves and the public source the rule implements. Never shown to the customer
    (tipping off). *)

open! Core

module Source : sig
  module Stable : sig
    module V1 : sig
      type t =
        | Rule of string (** the rule name *)
        | Model of
            { name : string
            ; version : string
            }
      [@@deriving bin_io, sexp, compare, equal, hash]
    end
  end

  type t = Stable.V1.t =
    | Rule of string
    | Model of
        { name : string
        ; version : string
        }
  [@@deriving sexp, compare, equal, hash]
end

module Stable : sig
  module V1 : sig
    type t =
      { source : Source.Stable.V1.t
      ; typology : Typology.Stable.V1.t option (** [None] for the model *)
      ; severity : Risk_rules.Severity.Stable.V1.t
      ; code : string (** risk-rules reason code text, e.g. ["STRUCTURING"] *)
      ; summary : string
      ; facts : (string * string) list
      ; reference : string
      }
    [@@deriving bin_io, sexp, compare, equal, hash]
  end
end

type t = Stable.V1.t =
  { source : Source.t
  ; typology : Typology.t option
  ; severity : Risk_rules.Severity.t
  ; code : string
  ; summary : string
  ; facts : (string * string) list
  ; reference : string
  }
[@@deriving sexp, compare, equal, hash]

(** From a risk-rules hit of one of this library's rules (looked up by rule name); a hit
    of another rule keeps its code and reason with no typology or reference. *)
val of_hit : Risk_rules.Hit.t -> t

(** The model's contribution: score, thresholds and top reasons as facts. *)
val of_score
  :  name:string
  -> version:string
  -> severity:Risk_rules.Severity.t
  -> score:float
  -> reasons:(string * float) list
  -> t

(** ["[review] structuring: 3 amounts in [8500.00, 10000.00) within 7d ..."]. *)
val to_string_hum : t -> string
