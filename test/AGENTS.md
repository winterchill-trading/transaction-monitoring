# AGENTS.md (test/)

Inline expect tests and Quickcheck properties (`-source-tree-root .` in `dune`).

- Run: `opam exec --switch 5.2.0+ox -- dune build --root .. @transaction-monitoring/runtest`;
  promote with `dune promote --root ..` (same `DUNE_BUILD_DIR`).
- `reference.ml` recomputes profile views from scratch; `test_profile.ml` compares it
  with the incremental profile. Keep it plain and independent of `Profile` internals.
- `store_conformance.ml` must print the same transcript on both stores
  (`test_store_memory.ml`, `test_store_pg.ml`); PostgreSQL comes from
  `TM_TEST_DATABASE_URL` or a throwaway `Pg_test` cluster (`test_db.ml`).
- `fixtures/parity.csv` is written by `../ml/train.py`; regenerate it with every model.
- `test_revoke.ml`: the revocation property (stored profiles after `Monitor.revoke`
  equal profiles that never saw the event, and later decisions match) on the memory
  store over 150 seeded streams and on PostgreSQL over three; its `check` is the place
  to extend when `Store.S` grows.
- `test_two_libraries.ml`: the host app, sanctions-screening and this library migrate
  into one database (needs `sanctions-screening-pg`).
- `fixtures/legacy-migrations/` are the migration files as 0.1.x shipped them, for the
  adoption test; never update them.
