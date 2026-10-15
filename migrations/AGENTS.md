# AGENTS.md

PostgreSQL migrations for `Store_pg`, embedded into the library at build time by
`lib/gen/gen_migrations.ml` (files named `<version>_<name>.sql`, applied in version
order under an advisory lock by pg-async `Migrate`).

- Never edit an applied migration (pg-async checks checksums); add a new file.
- Everything lives in the `transaction_monitoring` schema; no other service reads it.
- Body columns hold Bin_prot `Stable.V1` encodings: a type change needs a `V2` and a
  migration plan, never an in-place edit.
