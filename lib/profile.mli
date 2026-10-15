(** Rolling behavioural profiles, maintained one event at a time with bounded memory.

    A {!Customer} profile follows one customer's account activity: counts and sums in
    sliding windows of 1 hour, 24 hours, 7 days and 30 days (bucket-aligned, see
    {!Window}), outgoing amount quantiles (P-square) and a running mean and spread of log
    amounts, recently seen counterparties, devices and countries, the time since the last
    activity (dormancy), round amounts, amounts just under the reporting threshold, cash
    and card activity. A {!Counterparty} profile follows one counterparty across every
    customer of the bank: who pays it, who it pays, how much.

    Profiles are updated after a decision, and rules and features read a {!Customer.View}
    computed before the update, so "the last 24 hours" never includes the event being
    judged. Times never go backwards inside a profile: an event older than the newest one
    already applied is applied at the newest time (late events are rare and usually
    seconds late; clamping keeps every window consistent and replays deterministic).

    Memory per customer is constant: about 840 counters, two quantile sketches, and tables
    of at most 64 counterparties and 8 devices. A profile is mutable; take a
    {!Customer.val-snapshot} (an immutable copy with a stable bin_io form) to persist it. *)

open! Core

module Params : sig
  (** What the profile counts as "round", "just under the threshold" and "small". They
      shape the stored counters, so they are part of every snapshot, and a snapshot is
      only reused with the same parameters. *)
  type t =
    { reporting_threshold : int
    (** minor units of the reporting currency, e.g. [1_000_000] for 10 000.00 *)
    ; below_threshold_bps : int
    (** an amount in \[threshold * (1 - bps / 10 000), threshold) is "just under" *)
    ; round_unit : int (** an amount that is a positive multiple of this is "round" *)
    ; small_card_amount : int (** a card authorisation below this is "small" *)
    }
  [@@deriving sexp, compare, equal, bin_io]

  (** 10 000.00, 15% band, round to 100.00, small card below 2.00 (all in minor units of a
      two-decimal reporting currency). *)
  val default : t
end

module Customer : sig
  type t [@@deriving sexp_of]

  val create : Params.t -> Ids.Customer_id.t -> t
  val customer : t -> Ids.Customer_id.t
  val params : t -> Params.t

  (** Events applied so far. *)
  val events : t -> int

  (** The time an event at [at] is applied at: [at], or the newest time already applied
      when [at] is earlier. *)
  val clamp : t -> Time_ns.t -> Time_ns.t

  (** Folds the event into every window, sketch and table. Declined events only count as
      declined attempts (and card attempts); no money moved. *)
  val update : t -> Event.t -> unit

  module View : sig
    (** What rules and features read: the profile as of [now], before the event. Amounts
        are minor units of the reporting currency. *)
    type t =
      { now : Time_ns.Alternate_sexp.t
      ; events : int
      ; first_seen : Time_ns.Alternate_sexp.t option
      ; last_seen : Time_ns.Alternate_sexp.t option
      ; last_inbound : Time_ns.Alternate_sexp.t option
      ; out_n_1h : int
      ; out_sum_1h : int
      ; in_n_1h : int
      ; in_sum_1h : int
      ; out_n_24h : int
      ; out_sum_24h : int
      ; in_n_24h : int
      ; in_sum_24h : int
      ; out_n_7d : int
      ; out_sum_7d : int
      ; in_n_7d : int
      ; in_sum_7d : int
      ; out_n_30d : int
      ; out_sum_30d : int
      ; in_n_30d : int
      ; in_sum_30d : int
      ; round_n_30d : int (** round amounts, both directions *)
      ; below_n_24h : int (** amounts just under the threshold, both directions *)
      ; below_sum_24h : int
      ; below_n_7d : int
      ; below_sum_7d : int
      ; cash_n_24h : int (** ATM withdrawals and deposits *)
      ; cash_sum_24h : int
      ; card_n_10m : int
      (** card attempts, approved or declined, in the last three 5-minute buckets (10 to
          15 minutes) *)
      ; card_small_n_10m : int
      ; declined_n_10m : int
      ; declined_n_1h : int
      ; out_p50 : float option (** outgoing amount quantiles, major units *)
      ; out_p95 : float option
      ; out_log_mean : float option (** mean of log (1 + major amount) of outgoing *)
      ; out_log_sd : float option
      ; counterparty_first_seen : Time_ns.Alternate_sexp.t option
      (** of this event's counterparty, when it is in the table *)
      ; senders_24h : int (** distinct counterparties paying in *)
      ; senders_7d : int
      ; beneficiaries_24h : int (** distinct counterparties paid *)
      ; country_seen : bool option
      (** this event's counterparty country seen before; [None] without one *)
      ; countries_seen : int
      ; device_seen : bool option (** [None] without a device id *)
      }
    [@@deriving sexp_of, compare, equal]
  end

  (** The view for [event] (its counterparty, country and device), at the clamped event
      time. *)
  val view : t -> Event.t -> View.t

  type snapshot [@@deriving sexp_of, compare, equal]

  val snapshot : t -> snapshot

  (** A fresh mutable profile from a snapshot; the snapshot stays unchanged. *)
  val of_snapshot : snapshot -> t

  module Stable : sig
    module Snapshot : sig
      module V1 : sig
        type t = snapshot [@@deriving bin_io, sexp, compare, equal]
      end
    end
  end
end

module Counterparty : sig
  type t [@@deriving sexp_of]

  val create : Ids.Counterparty_id.t -> t
  val counterparty : t -> Ids.Counterparty_id.t
  val events : t -> int
  val update : t -> Event.t -> unit

  module View : sig
    (** "Paid" is money from the bank's customers to the counterparty (customer outbound),
        "sent" is money from it to customers (customer inbound). Amounts are minor units
        of the reporting currency. *)
    type t =
      { events : int
      ; first_seen : Time_ns.Alternate_sexp.t option
      ; paid_n_24h : int
      ; paid_sum_24h : int
      ; paid_n_7d : int
      ; paid_sum_7d : int
      ; sent_n_24h : int
      ; sent_sum_24h : int
      ; sent_n_7d : int
      ; sent_sum_7d : int
      ; payers_7d : int (** distinct customers paying it *)
      ; payees_7d : int (** distinct customers it pays *)
      }
    [@@deriving sexp_of, compare, equal]
  end

  val view : t -> Event.t -> View.t

  type snapshot [@@deriving sexp_of, compare, equal]

  val snapshot : t -> snapshot
  val of_snapshot : snapshot -> t

  module Stable : sig
    module Snapshot : sig
      module V1 : sig
        type t = snapshot [@@deriving bin_io, sexp, compare, equal]
      end
    end
  end
end
