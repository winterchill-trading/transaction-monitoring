(** {!Store.S} on PostgreSQL through pg-async. The only module with SQL in it.

    The library owns one schema (default [transaction_monitoring], see {!connect}): every
    table, and the [schema_migrations] table that records which of the files in
    [migrations/] were applied, live inside it. Nothing is created outside it, so the host
    application's own migrations table and other libraries' schemas never clash with
    these, even at the same version numbers. No other service should read or join it.

    {!Store.S.commit} runs in one transaction: the event row is inserted first with
    [ON CONFLICT DO NOTHING], so a redelivered or concurrently processed event id finds
    the row and returns the stored decision instead of writing twice. Profile snapshots
    are upserted only over a row with fewer events. *)

open! Core
open! Async
include Store.S

(** The schema used when {!connect} is not given one: ["transaction_monitoring"]. *)
val default_schema : string

(** Connects a pool to [database_url] ([postgres://user:pass@host:port/db?sslmode=...]),
    or to what [DATABASE_URL] / [PG*] say when it is omitted, creates [schema] if needed
    and applies pending migrations inside it, so several instances may start at once. TLS
    is required unless the URL says [sslmode=disable].

    [schema] must be a lower-case identifier ([[a-z_][a-z0-9_]*], not [pg_*]). Two
    instances of the library may share a database under different schemas.

    A database set up by versions 0.1.0 and 0.1.1 (tables in [transaction_monitoring],
    bookkeeping rows in the [schema_migrations] table of the search path) is adopted: the
    rows with this library's old checksums are copied into
    [transaction_monitoring.schema_migrations] and nothing is applied twice. *)
val connect
  :  ?database_url:string
  -> ?schema:string
  -> ?max_connections:int (** default 4 *)
  -> unit
  -> t Or_error.t Deferred.t

(** The schema this store lives in. *)
val schema : t -> string

(** The migrations this [connect] applied (empty when the schema was current). *)
val applied_migrations : t -> int list

val store : t -> Store.t
