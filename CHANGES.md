# Changes

## Unreleased

## 0.1.1

- Every hold and block (and every score from the hold threshold or the abstain band up)
  carries the model's exact per-feature contributions (risk-models `Explainer`), top
  five under reviewer-facing names from `Features.label`; the model's reasons come from
  them. `Decision.why` renders rule hits and contributions.
- `Decision.Stable.V2` and `Alert.Stable.V2` add the contributions (and the abstention
  flag); V1 is kept and upgraded on read.
- Migration `0002_explanations`: `body_version` and an `attribution` JSON column on
  decisions and alerts.
- `Policy.abstain`: an abstain band around the hold threshold, routed to a hold with
  reason "model uncertain" or left to the rules; `examples/policy-abstain.sexp`.
- `replay` lists the reasons for each changed decision, counts abstentions and prints the
  model's accuracy-coverage table on labelled histories.
- `bench` times model contributions and explanations.
- `tm-aml-gbdt` re-exported with node counts (same trees, new version string).

## 0.1.0

- `Event`: stable transaction event with a JSON lines codec.
- `Profile`: rolling customer and counterparty profiles with bounded memory and
  bin_prot snapshots.
- `Rules`: eleven typologies on risk-rules with explanations and references.
- `Features`, `Model`: risk-rules features plus AML features, scored by risk-models.
- `Policy`, `Decision`, `Alert`: Allow, Hold or Block; alert case lifecycle.
- `Store.S` with memory and PostgreSQL implementations and a conformance suite.
- `Monitor`: Async `on_event` with bounded intake; `Replay` with baseline diffs.
- `Synthetic` generator, `ml/` training pipeline and the `tm-aml-gbdt` model.
- `transaction-monitoring` command: generate, features, replay, bench, typologies,
  check-model, default-config.
