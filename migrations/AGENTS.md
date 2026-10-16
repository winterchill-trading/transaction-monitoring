# AGENTS.md

PostgreSQL migrations for `Store_pg`, embedded into the library at build time by
`lib/gen/gen_migrations.ml` (files named `<version>_<name>.sql`, applied in version
order under an advisory lock by pg-async `Migrate`).

- Never edit an applied migration (pg-async checks checksums); add a new file.
- Everything lives in the `transaction_monitoring` schema; no other service reads it.
- Body columns hold Bin_prot `Stable` encodings. Decisions and alerts carry a
  `body_version` (since `0002`): 1 for V1 rows, 2 for V2; a new version needs a new
  number, a reader for every old one, and a migration, never an in-place edit.
- `attribution` (jsonb) mirrors the explanation in the body for operator queries; the
  body is the record of truth.
