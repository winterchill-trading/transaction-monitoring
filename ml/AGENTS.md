# AGENTS.md (ml/)

Python training for the model (`uv sync`, then `uv run python train.py`). `run.sh` runs
the whole pipeline: `transaction-monitoring generate`, `features`, `train.py`, then a
replay. Data goes to `data/` (gitignored). `train.py` writes
`../models/tm-aml-gbdt/model.json`, `metrics.json` and `../test/fixtures/parity.csv`;
update the README model section from `metrics.json` after a retrain. Never compute a
feature in Python: features come from the OCaml `features` command.
