(** A seeded generator of realistic synthetic bank activity with injected laundering and
    fraud typologies, labelled per event. It feeds the model training pipeline in [ml/],
    the replay and bench commands, and tests.

    The population mixes salaried customers, students, small businesses (many payers in,
    suppliers out, invoices near 10 000) and expatriates (remittances abroad, travel,
    foreign cash), with salaries, rent, bills, card spending at a shared pool of
    merchants, ATM withdrawals, payments between friends and transfers to savings. A
    share of customers each carries one typology episode: structuring, smurfing, rapid
    movement, mule fan-in (with shared collector accounts), dormant reactivation, a
    higher-risk corridor, card testing, ATM cash runs, or sanctions-adjacent payments.
    About a third of the episodes are deliberately subtle (fewer payments, longer gaps)
    and stay under the default rule thresholds, and the benign population includes look-
    alikes (business fan-in, near-threshold invoices, large one-off purchases, travel
    cash, dormant accounts that wake up with small payments), so neither rules nor model
    see a clean separation.

    The same seed and config always give the same events, on every platform. *)

open! Core

module Config : sig
  type t =
    { customers : int
    ; days : int
    ; seed : int
    ; start : Time_ns.t (** midnight UTC of day 0 *)
    ; bad_share : float (** share of customers with a typology episode *)
    }
  [@@deriving sexp_of]

  (** 4 000 customers, 75 days from 2026-01-05, seed 42, 4% with an episode. *)
  val default : t
end

type labelled =
  { event : Event.t
  ; label : Typology.t option (** the typology the event belongs to, if any *)
  }

(** Every event, ordered by [occurred_at] (ties by id). *)
val generate : Config.t -> labelled list

(** The event's JSON with a ["label"] field (the typology name or [null]). *)
val to_json_line : labelled -> string

(** Reads a line written by {!to_json_line} (a line without a label reads as [None]). *)
val of_json_line : string -> labelled Or_error.t
