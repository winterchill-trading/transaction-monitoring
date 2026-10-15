# AGENTS.md (lib/)

The `transaction-monitoring` library. Data flows `Event` -> `Profile` views ->
`Context` -> `Rules` and `Features`/`Model` -> `Policy` -> `Decision` -> `Alert`;
`Evaluator` is the synchronous core, `Monitor` adds Async and the `Store`, `Replay`
runs histories.

- `Window`, `P2_quantile`, `Recent`, `Country_set` are the bounded building blocks of
  `Profile`; changing their layout changes the snapshot digests in
  `../test/test_event.ml`.
- `store_pg.ml` is the only file with SQL; the migrations come from `../migrations/`
  through the rule in `dune` and `gen/gen_migrations.ml`.
- Keep `Features.extra_names` in step with `Features.extras` (a test checks it).
- New typology: add the constructor to `Typology` (a `Stable.V2` if `V1` is pinned),
  its rule in `rules.ml`, an expect test in `../test/test_rules.ml` and a row in the
  README table with its reference.
