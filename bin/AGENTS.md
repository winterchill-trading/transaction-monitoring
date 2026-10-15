# AGENTS.md (bin/)

The `transaction-monitoring` command: `generate`, `features`, `replay`, `bench`,
`typologies`, `check-model`, `default-config`. `bench_cmd.ml` holds the core_bench
benchmark; run it on a release build
(`dune build --root .. --profile release ./transaction-monitoring/bin/main.exe`) and
record the numbers in the README latency table.
