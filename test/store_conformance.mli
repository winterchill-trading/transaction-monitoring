(** The same scenario against any {!Transaction_monitoring.Store.S}; it prints a
    transcript, and the memory and PostgreSQL tests expect the same one. *)

open! Core
open! Async
open Transaction_monitoring

val run : Store.t -> unit Deferred.t
