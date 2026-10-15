# AGENTS.md

Guide for AI coding agents working in this repository. Read this first; the subdirectory
`AGENTS.md` files add detail. The workspace-wide guide is
`../oxcaml-claude-skills/AGENTS.md`.

## Purpose

`transaction-monitoring` screens payments in real time for anti-money-laundering
typologies: rolling customer and counterparty profiles, typology rules on `risk-rules`,
a gradient-boosted score through `risk-models`, and an Allow, Hold or Block decision
with alerts for review. Storage is a `Store.S` module type with memory and PostgreSQL
implementations.

## Layout

| Path | Holds |
| --- | --- |
| `lib/` | The `transaction-monitoring` library |
| `test/` | Expect tests, Quickcheck properties, store conformance |
| `bin/` | The `transaction-monitoring` command |
| `migrations/` | PostgreSQL schema, embedded into the library at build time |
| `ml/` | Python generator, training and export of the model |
| `models/` | Committed model exports scored by the library |
| `.github/` | CI workflow |

## Build, test, docs

From this directory inside the workspace:

```sh
opam exec --switch 5.2.0+ox -- dune build --root .. @transaction-monitoring/all
opam exec --switch 5.2.0+ox -- dune build --root .. @transaction-monitoring/runtest
opam exec --switch 5.2.0+ox -- dune build --root .. @transaction-monitoring/doc
```

Prefix with `DUNE_BUILD_DIR=_build-transaction-monitoring` for parallel work.

## Never do

- Install or upgrade opam packages that change the compiler, `core`, `base`, `async` or
  `ppx_jane` on the shared switch.
- Publish, tag or release.
- Commit credentials, print secrets, or poll CI in a loop.
