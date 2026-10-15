(** Identifiers used across the library. Each is a distinct type over
    [Ledger.Validated_id] (1 to 128 characters from [A-Z a-z 0-9 _ - . : / @ +]), so a
    customer id can never be passed where an event id is expected. Bin_prot is the plain
    string. *)

open! Core

(** The bank's customer (the party being monitored). *)
module Customer_id : Ledger.Validated_id.S

(** One monitored transaction event, unique per source (payment id, card authorisation id,
    ledger record id). Decisions are idempotent per event id. *)
module Event_id : Ledger.Validated_id.S

(** The other side of a payment: a hashed IBAN or account number, a wallet address, a
    merchant id or an ATM terminal id. Never a name: monitoring needs a stable key, not
    PII. *)
module Counterparty_id : Ledger.Validated_id.S

(** An alert opened by a [Hold] or [Block] decision. *)
module Alert_id : Ledger.Validated_id.S

(** The investigator working an alert. *)
module Analyst_id : Ledger.Validated_id.S
