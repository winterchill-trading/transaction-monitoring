# AGENTS.md

Guide for AI coding agents working in this repository. Read this first; the subdirectory
`AGENTS.md` files add detail for `lib/`, `test/`, `bin/`, `examples/`, `ml/`, `models/`,
`migrations/` and `.github/`. The workspace-wide guide is
`../oxcaml-claude-skills/AGENTS.md`.

## Purpose

`transaction-monitoring` screens payments in real time for money-laundering and fraud
typologies: rolling customer and counterparty profiles, typology rules on `risk-rules`,
a gradient-boosted score through `risk-models`, and an Allow, Hold or Block decision
with alerts and a case lifecycle. Holds and blocks carry the rule hits and the model's
exact feature contributions; a policy may let the model abstain near its threshold. State persists behind `Store.S` (memory and
PostgreSQL through `pg-async`).

## Layout

| Path | Holds |
| --- | --- |
| `lib/` | The `transaction-monitoring` library (`Transaction_monitoring`) |
| `test/` | Expect tests, Quickcheck properties, store conformance, model parity |
| `bin/` | The `transaction-monitoring` command (generate, features, replay, bench, ...) |
| `examples/` | Rules config and policy files; `quickstart.ml` with its expected output |
| `migrations/` | PostgreSQL tables (unqualified), embedded into the library at build time and applied inside the library's own schema |
| `ml/` | Python training, evaluation and export (`train.py`, `run.sh`) |
| `models/` | The committed model export and its metrics; embedded into the library (`Model.bundled`) and installed under `share/` (`models/dune`) |
| `.github/` | CI workflow |

## Build, test, docs

From this directory inside the workspace:

```sh
opam exec --switch 5.2.0+ox -- dune build --root .. @transaction-monitoring/all
opam exec --switch 5.2.0+ox -- dune build --root .. @transaction-monitoring/runtest
opam exec --switch 5.2.0+ox -- dune build --root .. @transaction-monitoring/doc
```

Parallel work: prefix with `DUNE_BUILD_DIR=_build-transaction-monitoring` (resolved from
the workspace root because of `--root ..`). Promote expect output with
`opam exec --switch 5.2.0+ox -- dune promote --root ..` and the same `DUNE_BUILD_DIR`.
PostgreSQL tests need `TM_TEST_DATABASE_URL` or the server binaries (`initdb`).

Standalone: `opam repo add winterchill https://github.com/winterchill-trading/opam-repository.git`,
`opam install --deps-only --with-test .`, then `dune build && dune test`.

## Conventions

- `.mli` with odoc on every public value; `open! Core`; labelled arguments; ppx_jane.
- Persisted types have `Stable.V1` bin_io; digests are pinned in `test/test_event.ml`.
  Add a `V2`, never edit a pinned version.
- `Evaluator` is pure apart from mutating the profiles it is given; no clock, no I/O.
  Decisions are stamped with the event's `received_at`, so replays reproduce them.
- `Features` is the only feature code; `ml/train.py` reads the CSV that
  `transaction-monitoring features` writes. A feature change means a new schema
  version, a retrained model and a new parity fixture.
- SQL and pg-async types only in `lib/store_pg.ml` and `migrations/`. Queries name
  tables as `{s}.table` and `q` substitutes the configured schema; never write
  `transaction_monitoring.` or an unqualified table there. Never edit `0001` or `0002`:
  their checksums identify 0.1.x installs for adoption (`legacy_checksums`).
- `Store.S` changes need both stores and `test/test_revoke.ml`'s `check` (it runs on
  both) or the conformance suite.
- Profiles only move forward; anything that must forget an event rebuilds from
  `customer_history` / `counterparty_history` (see `Monitor.revoke`).
- Comments for humans at non-obvious choices (bucket-aligned windows, clamped times,
  the prior shift in the model export).
- Formatting: `opam exec --switch 5.2.0+ox -- dune build --root .. @transaction-monitoring/fmt --auto-promote`.
  Never run `dune fmt` at the workspace root. Odoc tables are mangled by the formatter;
  use lists in doc comments.
- User-visible changes get a line under "Unreleased" in `CHANGES.md`.

## Dependencies and dependents

- Depends on `money`, `ledger`, `risk-rules`, `risk-models`, `pg-async` (and through
  them `fixed-decimal`, `payments-core`), plus `core`, `async`, `core_unix`,
  `core_bench`, `yojson`, `zarith`.
- CI pin order: `pg-async`, `fixed-decimal`, `money`, `ledger`, `payments-core`,
  `risk-rules`, `risk-models`, `kyc-core`, `sanctions-screening`,
  `sanctions-screening-pg` (test only: `test/test_two_libraries.ml`).
- No org repository depends on it yet.

## CI

`.github/workflows/ci.yml`: libpq, a Postgres service (`TM_TEST_DATABASE_URL`), the infra
`ox-setup` action with the pins above, `dune build`, `dune test`, an odoc build that
fails on warnings, and the shared `obj-magic` workflow. Check a run with
`gh run list --limit 1`, then at most one `gh run watch <id> --exit-status`.

## Pitfalls

- With `open! Async`, Async's `printf` is buffered: flush `Writer.stdout` before a
  program exits, and every `let%expect_test` in such a file returns a deferred.
- Base's `List.init` calls `f` from the last index down; tests that care about order
  use `List.map` over explicit lists.
- `List.split_n` returns an unboxed pair on this switch: `let #(a, b) = ...`.
- Profiles warmed on empty history make every payee new: synthetic replays and training
  drop the first 14 days.
- scikit-learn's binning fails on all-missing columns; `train.py` fits on the columns
  with at least two values and maps tree features back to the full schema.
- Floats from `exp`/`log1p` may differ in the last bit between macOS and Linux; tests
  print rounded values or counts, never raw scores of synthetic events. Contributions
  are printed with two decimals for the same reason.
- Model contributions are not causes. Any text a person reads says "model
  contributions"; customers never see them (tipping off).
- Retraining or re-exporting the model changes its version string, which the quickstart
  and some tests print: update those expectations in the same change. The model is also
  embedded (`lib/bundled_model_json.ml` is generated from `models/tm-aml-gbdt/model.json`),
  so a retrain changes the library.
- Severity overrides never create hits; a test that expects a block from a country
  override needs a rule that fires on that event first.
- Another agent's half-written sibling can break a workspace build; filter the output
  for `transaction-monitoring/` paths before treating it as a failure.

## Where to read more

- `README.md`: integration, profiles, typologies with references, model results,
  latency, storage, known gaps.
- `../specs/repos/transaction-monitoring.md`.
- `../oxcaml-claude-skills/skills/banking-standards/` and
  `../oxcaml-claude-skills/skills/oxcaml-performance/`.
- `../risk-rules/README.md`, `../risk-models/README.md`.
- `../oxcaml-claude-skills/AGENTS.md`, the workspace-wide guide.

## Never do

- Install or upgrade opam packages that change the compiler, `core`, `base`, `async` or
  `ppx_jane` on the shared switch. Run `opam install --switch 5.2.0+ox --dry-run <pkg>`
  first and stop if the plan touches any of them.
- Publish, tag or release.
- Show alerts, reasons or typologies to customers (tipping off).
- Commit credentials, print secrets, or poll CI in a loop.
- Commit `ml/data/` (generated, hundreds of MB).
