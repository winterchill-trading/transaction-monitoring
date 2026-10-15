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
