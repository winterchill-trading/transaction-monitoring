#!/usr/bin/env bash
# The whole training pipeline: synthetic events, feature rows computed by the OCaml
# library, then training, evaluation and export. Run from the ml/ directory.
# TM is the transaction-monitoring command (default: the one dune builds).
set -euo pipefail
cd "$(dirname "$0")"
TM=${TM:-"opam exec --switch 5.2.0+ox -- dune exec -- transaction-monitoring"}
mkdir -p data
$TM generate -out data/events.jsonl "$@"
$TM features -events data/events.jsonl -out data/features.csv
uv run python train.py --features data/features.csv
$TM replay -events data/events.jsonl -model ../models/tm-aml-gbdt/model.json \
  -policy ../examples/policy-active.sexp
