# Changes

## Unreleased

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
