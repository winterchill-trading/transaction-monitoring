# AGENTS.md (models/)

`tm-aml-gbdt/model.json` is risk-models' format 1 (kind `gbdt`) on the
`transaction-monitoring-features` schema; `metrics.json` holds every number in the
README. Both are written by `../ml/train.py`; never edit them by hand. The model
version is the MD5 prefix of the file, recorded on every decision.
