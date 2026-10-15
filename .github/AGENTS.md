# AGENTS.md (.github/)

`workflows/ci.yml`: Postgres service and `TM_TEST_DATABASE_URL`, libpq, infra
`ox-setup` with sibling pins in dependency order (`pg-async`, `fixed-decimal`, `money`,
`ledger`, `payments-core`, `risk-rules`, `risk-models`), build, test, odoc with warnings
as errors, `obj-magic`. When `dune-project` gains a sibling dependency, add its pin in
the same commit.
