# AGENTS.md (examples/)

- `config.sexp`: the default rules config (`transaction-monitoring default-config`);
  `test/test_examples.ml` checks it equals `Rules.Config.default`.
- `policy-active.sexp`, `policy-shadow.sexp`: model modes; `policy-abstain.sexp`: the
  active model with an abstain band routed to holds.
- `quickstart.ml`: the consumer wiring (payment initiation, inbound booking, review
  queue with the model's contributions); `runtest` diffs its output against
  `quickstart.expected`, which pins the model version string: a re-export changes it.
