(** Fresh PostgreSQL databases for tests: with [TM_TEST_DATABASE_URL] (a superuser URL,
    CI's Postgres service) a new database on that server per call; otherwise pg-async's
    throwaway cluster, which needs the PostgreSQL server binaries. *)

open! Core
open! Async

val fresh_url : unit -> string Deferred.t
