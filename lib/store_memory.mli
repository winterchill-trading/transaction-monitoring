(** {!Store.S} in memory, following the PostgreSQL store's rules exactly (idempotent
    commits, monotonic profile snapshots, optimistic alert versions, the same orders), so
    both pass the same conformance transcript. Values are kept as copies; nothing a caller
    mutates later changes what is stored. *)

open! Core
include Store.S

val create : unit -> t
val store : t -> Store.t
