# AGENTS.md (models/)

`tm-aml-gbdt/model.json` is risk-models' format 1 (kind `gbdt`) on the
`transaction-monitoring-features` schema; `metrics.json` holds every number in the
README. Both are written by `../ml/train.py`; never edit them by hand. The model
version is the MD5 prefix of the file, recorded on every decision.

`model.json` is also embedded into the library at build time (`lib/gen/gen_embed.ml`,
`Model.bundled`) and installed under `share/transaction-monitoring/models/` by
`models/dune`; a test checks the embedded copy equals the file and scores the same.
