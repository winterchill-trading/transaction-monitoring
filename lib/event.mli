(** A monitored transaction event: one payment, card authorisation, ATM operation or
    internal transfer, in or out of a customer's account.

    The event is provider-neutral and carries no names or account numbers: the
    counterparty is a stable key ({!Ids.Counterparty_id}, e.g. a hashed IBAN). Amounts
    come twice: in the payment's own currency ([amount]) and converted by the caller into
    the bank's reporting currency ([base_amount]) at the booking rate, because thresholds
    and profiles must add up across currencies. Times are UTC. *)

open! Core

module Rail : sig
  module Stable : sig
    module V1 : sig
      type t =
        | Sepa
        | Sepa_instant
        | Uk_faster_payments
        | Uk_chaps
        | Us_ach
        | Us_wire
        | Card (** card authorisations, including ATM withdrawals and deposits *)
        | Intraledger (** between two accounts of the same bank *)
        | Crypto (** on-chain or Lightning *)
      [@@deriving bin_io, sexp, compare, equal, hash]
    end
  end

  type t = Stable.V1.t =
    | Sepa
    | Sepa_instant
    | Uk_faster_payments
    | Uk_chaps
    | Us_ach
    | Us_wire
    | Card
    | Intraledger
    | Crypto
  [@@deriving sexp, compare, equal, hash, enumerate, quickcheck]

  (** ["sepa"], ["uk_faster_payments"], ... *)
  val to_string : t -> string

  val of_string : string -> t Or_error.t
end

module Direction : sig
  module Stable : sig
    module V1 : sig
      type t =
        | Inbound (** money into the customer's account *)
        | Outbound
      [@@deriving bin_io, sexp, compare, equal, hash]
    end
  end

  type t = Stable.V1.t =
    | Inbound
    | Outbound
  [@@deriving sexp, compare, equal, hash, enumerate, quickcheck]

  (** ["in"] or ["out"]. *)
  val to_string : t -> string

  val of_string : string -> t Or_error.t
end

module Card_channel : sig
  module Stable : sig
    module V1 : sig
      type t =
        | In_person
        | Online (** card not present *)
        | Atm
      [@@deriving bin_io, sexp, compare, equal, hash]
    end
  end

  type t = Stable.V1.t =
    | In_person
    | Online
    | Atm
  [@@deriving sexp, compare, equal, hash, enumerate, quickcheck]

  (** ["in_person"], ["online"], ["atm"]. *)
  val to_string : t -> string

  val of_string : string -> t Or_error.t
end

module Risk_band : sig
  (** The customer's KYC risk band, when the caller knows it. *)

  module Stable : sig
    module V1 : sig
      type t =
        | Low
        | Medium
        | High
      [@@deriving bin_io, sexp, compare, equal, hash]
    end
  end

  type t = Stable.V1.t =
    | Low
    | Medium
    | High
  [@@deriving sexp, compare, equal, hash, enumerate, quickcheck]

  val to_string : t -> string
  val of_string : string -> t Or_error.t
end

module Stable : sig
  module V1 : sig
    type t =
      { id : Ids.Event_id.Stable.V1.t
      ; customer : Ids.Customer_id.Stable.V1.t
      ; account : Ledger.Account_id.Stable.V1.t
      ; counterparty : Ids.Counterparty_id.Stable.V1.t
      ; counterparty_country : Risk_rules.Country.Stable.V1.t option
      (** where the counterparty's account, merchant or ATM is *)
      ; rail : Rail.Stable.V1.t
      ; direction : Direction.Stable.V1.t
      ; amount : Money.Amount.Stable.V1.t
      ; base_amount : Money.Amount.Stable.V1.t
      (** [amount] in the reporting currency, converted by the caller *)
      ; occurred_at : Time_ns.Stable.Alternate_sexp.V1.t
      (** when the customer or counterparty acted *)
      ; received_at : Time_ns.Stable.Alternate_sexp.V1.t
      (** when the bank saw it; profiles and windows use [occurred_at] *)
      ; mcc : Risk_rules.Mcc.Stable.V1.t option
      ; card_channel : Card_channel.Stable.V1.t option
      ; declined : bool (** a declined authorisation or a rejected payment attempt *)
      ; device_id : string option
      (** hashed device fingerprint for customer-initiated events *)
      ; ip_country : Risk_rules.Country.Stable.V1.t option
      ; residence : Risk_rules.Country.Stable.V1.t option (** the customer's country *)
      ; account_opened_at : Time_ns.Stable.Alternate_sexp.V1.t option
      ; risk_band : Risk_band.Stable.V1.t option
      }
    [@@deriving bin_io, sexp, compare, equal]
  end
end

type t = Stable.V1.t =
  { id : Ids.Event_id.t
  ; customer : Ids.Customer_id.t
  ; account : Ledger.Account_id.t
  ; counterparty : Ids.Counterparty_id.t
  ; counterparty_country : Risk_rules.Country.t option
  ; rail : Rail.t
  ; direction : Direction.t
  ; amount : Money.Amount.t
  ; base_amount : Money.Amount.t
  ; occurred_at : Time_ns.t
  ; received_at : Time_ns.t
  ; mcc : Risk_rules.Mcc.t option
  ; card_channel : Card_channel.t option
  ; declined : bool
  ; device_id : string option
  ; ip_country : Risk_rules.Country.t option
  ; residence : Risk_rules.Country.t option
  ; account_opened_at : Time_ns.t option
  ; risk_band : Risk_band.t option
  }
[@@deriving sexp_of, compare, equal]

(** [base_amount] in minor units of the reporting currency. Fails when it does not fit in
    an [int] (more than 4.6e18 minor units) or is negative: amounts are magnitudes, the
    direction says which way the money went. *)
val base_minor : t -> int

(** Whether the event moves physical cash: an ATM withdrawal or deposit. *)
val is_cash : t -> bool

(** Checks what the types cannot: a non-negative [base_amount] that fits in an [int], a
    card channel only on card events. *)
val validate : t -> t Or_error.t

(** {2 JSON lines}

    One flat object per line, as written by [transaction-monitoring generate]:
    [{"id", "customer", "account", "counterparty", "counterparty_country", "rail", "direction", "amount": "12.34 EUR", "base_amount", "occurred_at" (RFC 3339 UTC), "received_at", "mcc", "card_channel", "declined", "device_id", "ip_country", "residence", "account_opened_at", "risk_band"}].
    Optional fields may be absent or [null]; unknown fields (such as a training [label])
    are ignored. *)

val to_json : t -> Yojson.Safe.t
val of_json : Yojson.Safe.t -> t Or_error.t
val to_json_line : t -> string
val of_json_line : string -> t Or_error.t
