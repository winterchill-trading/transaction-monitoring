(** The files of [migrations/], embedded at build time: (version, name, SQL), in version
    order. Only {!Store_pg} reads it. *)

val all : (int * string * string) list
