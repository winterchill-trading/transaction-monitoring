(** {!Store.S} on PostgreSQL through pg-async, in the [transaction_monitoring] schema (see
    [migrations/]). The only module with SQL in it.

    {!Store.S.commit} runs in one transaction: the event row is inserted first with
    [ON CONFLICT DO NOTHING], so a redelivered or concurrently processed event id finds
    the row and returns the stored decision instead of writing twice. Profile snapshots
    are upserted only over a row with fewer events. *)

open! Core
open! Async
include Store.S

(** Connects a pool to [database_url] ([postgres://user:pass@host:port/db?sslmode=...]),
    or to what [DATABASE_URL] / [PG*] say when it is omitted, and applies pending
    migrations under pg-async's advisory lock, so several instances may start at once. TLS
    is required unless the URL says [sslmode=disable]. *)
val connect
  :  ?database_url:string
  -> ?max_connections:int (** default 4 *)
  -> unit
  -> t Or_error.t Deferred.t

(** The migrations this [connect] applied (empty when the schema was current). *)
val applied_migrations : t -> int list

val store : t -> Store.t
