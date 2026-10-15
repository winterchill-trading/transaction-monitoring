# AGENTS.md (examples/)

- `config.sexp`: the default rules config (`transaction-monitoring default-config`);
  `test/test_examples.ml` checks it equals `Rules.Config.default`.
- `policy-active.sexp`, `policy-shadow.sexp`: model modes.
- `quickstart.ml`: the consumer wiring (payment initiation, inbound booking, review
  queue); `runtest` diffs its output against `quickstart.expected`.
