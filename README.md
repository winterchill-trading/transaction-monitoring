# transaction-monitoring

Real-time anti-money-laundering transaction monitoring for a bank, in OCaml
([OxCaml](https://oxcaml.org)). Every payment the bank initiates or books goes through
`Monitor.on_event`: it updates rolling profiles of the customer and the counterparty,
runs a library of typology rules (structuring, smurfing, rapid movement, mules,
dormant accounts, high-risk corridors, card testing, ATM cash, sanctions-adjacent
countries, threshold reporting), scores the event with a gradient-boosted model, and
returns `Allow`, `Hold` or `Block` with every reason. Holds open alerts with a case
lifecycle for investigators. A decision takes about 6 us in process.

Provider-neutral: events carry no names or account numbers, only stable keys
(a hashed IBAN, a merchant or ATM id), amounts and countries.

> **Synthetic data only.** The bundled model was trained and evaluated on events the
> generator in this repository makes up, with typologies it injects itself. The numbers
> below show that the pipeline works end to end and how well the features separate
> those made-up patterns. They say nothing about performance on real payments. Retrain,
> re-evaluate and re-choose every threshold on real, labelled cases before relying on
> the model; the rules' defaults are starting points to tune with the compliance
> function.

## Why separate

Payment plumbing, fraud rules and AML monitoring change on different clocks and answer
to different people. `risk-rules` decides one operation from a context the caller
assembles; `risk-models` scores feature vectors. Monitoring needs state of its own: the
behaviour of every customer and counterparty over hours, days and months, kept
incrementally because nobody can re-read 30 days of history for each payment, plus
alerts and cases that live for weeks. Keeping that state, the AML typologies and the
case lifecycle in one library over plain values (`Event.t` in, `Decision.t` out, a
`Store.S` for persistence) means a decision can be replayed from history, a new model
or policy can be compared against the current one on months of events before it goes
live, and a payment service only has to call one function.

## Quick start

```ocaml
open! Core
open! Async
open Transaction_monitoring

let start () =
  let model = Model.bundled () |> ok_exn in   (* or Model.load path, Model.of_string *)
  let evaluator =
    Evaluator.create ~model ~policy:Policy.default Rules.Config.default |> ok_exn
  in
  let%bind store = Store_pg.connect () >>| ok_exn in      (* DATABASE_URL *)
  Monitor.create ~now:(Time_ns.now ()) ~store:(Store_pg.store store) evaluator
  >>| ok_exn

(* For each payment initiated or booked: *)
let screen monitor (event : Event.t) =
  match%map Monitor.on_event monitor event with
  | Ok (Decided d | Already_decided d) -> d.outcome         (* Allow | Hold | Block *)
  | Ok Overloaded | Error _ -> Hold                         (* fail closed *)
```

The complete program, wired into payment initiation, inbound booking and a review
queue, with its output checked by `dune test`, is
[examples/quickstart.ml](examples/quickstart.ml).

CLI (`transaction-monitoring`):

```sh
transaction-monitoring generate -out events.jsonl          # synthetic labelled events
transaction-monitoring features -events events.jsonl -out features.csv
transaction-monitoring replay -events events.jsonl -model models/tm-aml-gbdt/model.json \
  -policy examples/policy-active.sexp -baseline-policy examples/policy-shadow.sexp
                                         # what changed (with reasons), accuracy-coverage table
transaction-monitoring bench -model models/tm-aml-gbdt/model.json
transaction-monitoring typologies                          # codes, descriptions, references
transaction-monitoring check-model models/tm-aml-gbdt/model.json
transaction-monitoring default-config > config.sexp
```

No credentials and no network. The PostgreSQL store reads `DATABASE_URL` (or `PG*`);
TLS is required unless the URL says `sslmode=disable`.

## Integrating with an application

`on_event` is the only call on the payment path. Build an `Event.t` from the payment
(`base_amount` converted into the reporting currency at the booking rate, the
counterparty as a stable hashed key, device and IP country for customer-initiated
payments) and act on the reply:

| Flow | Where to call | `Allow` | `Hold` | `Block` | `Overloaded` or `Error` |
|---|---|---|---|---|---|
| Payment initiation (SEPA, SCT Inst, Faster Payments, ACH, wires, crypto) | after the customer confirms, before submission to the scheme | submit | park the payment; the customer sees "under review" | refuse with a generic message | hold (fail closed) and retry |
| Inbound booking (credits, ATM deposits) | when the credit is booked | nothing | book, then freeze the amount with a ledger hold until review | book, freeze, escalate | book, retry monitoring later |
| Card authorisations | from the authorisation handler (well inside the 2 s budget) | approve | decline or approve under review, per card policy | decline | approve or decline per stand-in policy |

- Idempotency: the event id is the key. A retried call returns the stored decision
  (`Already_decided`) and changes no profile; alerts are keyed by event id too.
- Review queue: `Monitor.review_queue` lists open and assigned alerts, oldest first, each
  with its rule explanations and `attribution` (the model's top five contributions,
  `Decision.why` renders both);
  `Monitor.apply_alert` moves one through its lifecycle (assign, flag as SAR candidate,
  close as true or false positive, reopen by another analyst). Closing a held payment's
  alert as a false positive is the signal to release it; the application owns that
  step, because only it knows the payment.
- Tipping off: `Decision.customer_status` is the only text for customers. Reasons,
  alerts and typologies stay internal.
- Threshold reports: `Decision.threshold_report` marks cash at or above the reporting
  threshold; it is an obligation, not a suspicion, and does not hold the payment.
- Payments that do not happen: when a decided payment never goes ahead (the customer
  cancels a held payment, the scheme rejects it, an authorisation is reversed), call
  `Monitor.revoke monitor event_id ~reason ~at`. Both profiles are rebuilt from the
  stored history without the event, so velocity windows, new-counterparty and every
  other feature behave as if it had never been seen; the revocation is stored with the
  rebuilt snapshots in one transaction, and the event's decision stays (a redelivery is
  still `Already_decided`). It needs every event of the profile in the store, so warm up
  through `on_event`; otherwise it refuses and changes nothing.
- Warm-up: profiles need history. Before going live, replay at least 30 days of booked
  events through the monitor (or `Replay`) so that windows, quantiles and the
  counterparty table are full; on empty profiles every payee is new and the
  collector-account rule fires on ordinary billers.

## Modules

| Module | What it is |
|---|---|
| `Event` | the stable transaction event (`Stable.V1` bin_io): customer, account (`Ledger.Account_id`), counterparty key and country, rail (SEPA, SCT Inst, Faster Payments, CHAPS, ACH, wire, card, intraledger, crypto), direction, amount and base amount (`money`), occurred and received times, MCC, card channel, declined, device, IP country, residence, account opening, KYC risk band; JSON lines codec |
| `Profile` | `Customer` and `Counterparty` rolling profiles, updated incrementally with bounded memory, snapshot and restore (`Stable.Snapshot.V1`) |
| `Window`, `P2_quantile`, `Recent`, `Country_set` | bucketed sliding counters, P-square quantiles, a bounded recent-keys table, a 676-bit country set |
| `Typology` | the typologies with codes, descriptions and public references |
| `Context`, `Rules` | what rules see; the typology rules as `Risk_rules.Rule.t`, config as sexp, shadow and disabled typologies |
| `Explanation` | why a rule or the model objected: typology, code, summary, facts, reference; "model uncertain" for abstentions |
| `Features` | the feature vector: risk-rules' `Features.t` built from event and profiles, laid out by `Risk_models.Encoding`, then 32 AML features; a reviewer-facing label for each |
| `Model` | a risk-models scorer on this feature schema, and its explainer; `Model.bundled ()` (embedded at build time), `of_string`, `of_json`, `default ()` (`TRANSACTION_MONITORING_MODEL` or the bundled one) |
| `Policy`, `Decision` | how rule hits and the score combine, with an optional abstain band; the decision record (`Stable.V2`: rule explanations, score, model contributions, abstention) |
| `Alert` | the alert and its case lifecycle as a pure state machine returning the record and its events; carries the decision's contributions |
| `Evaluator` | the synchronous core: decide, apply, in-memory profiles |
| `Store`, `Store_memory`, `Store_pg` | persistence behind `Store.S`; SQL only in `store_pg.ml` and `migrations/` |
| `Monitor` | the Async `on_event` pipeline with bounded intake, `revoke` for decided events that did not happen, the review queue, alert commands |
| `Replay` | re-run a history through a candidate model or policy next to a baseline and report what changed, why, and the model's accuracy-coverage table |
| `Synthetic` | the seeded generator of labelled synthetic activity |

## Profiles

Per customer, maintained one event at a time:

- counts and sums in and out over 1 h (12 x 5 min buckets), 24 h (24 x 1 h), 7 days
  (28 x 6 h) and 30 days (30 x 1 day); windows are bucket-aligned, so "24 h" reaches
  between 23 and 24 hours back;
- round amounts (multiples of 100.00), amounts just under the reporting threshold
  (default \[8 500, 10 000)) with their sum, cash (ATM) count and sum, card attempts,
  small card attempts and declines in 10 to 15 minutes;
- outgoing amount p50 and p95 (P-square) and a running mean and standard deviation of
  log amounts;
- up to 64 recent counterparties with first and last sighting in each direction (new
  counterparty, distinct payers in 24 h and 7 days, distinct payees in 24 h), up to 8
  devices, the set of countries seen;
- first and last activity (dormancy), last inbound credit.

Per counterparty, across all customers: paid to it and sent by it over 24 h and 7 days,
and up to 32 recent customers (how many distinct customers pay it).

Memory is constant: about 840 counters per customer plus the bounded tables; bin_prot
writes a snapshot in a few kilobytes (zero counters take one byte). Times never go backwards inside a profile: a late event is
applied at the newest time already seen, which keeps replays deterministic.

Tested: a Quickcheck property checks every view of the incremental profile against a
from-scratch recomputation over the full history (random streams with late events,
declines, cards, cash, countries and devices); another checks that a snapshot taken
mid-stream, written and read with bin_prot, resumes to exactly the same profile.

## Typologies

| Typology | Code | Triggers when (defaults) | Severity | Implements |
|---|---|---|---|---|
| `structuring` | `STRUCTURING` | 3 or more amounts in \[8 500, 10 000) within 7 days | Review | FATF R.20; 31 CFR 1010.314 (structuring to evade reporting) |
| `smurfing` | `CUSTOM_TM_SMURFING` | the 4th or later distinct payer in 24 h sends under 10 000, inbound total reaches 10 000 | Review | FATF R.20; EBA/GL/2021/02 4.46 (unusual patterns); 31 CFR 1010.314 |
| `rapid_movement` | `MULE_PASS_THROUGH` | an outbound payment of at least a quarter of the 24 h inflow (at least 1 000), outbound in 24 h within 80% to 120% of it | Review | FATF Professional Money Laundering (2018), layering; EBA/GL/2021/02 4.72-4.75 |
| `mule_fan_in` | `MULE_FAN_IN` | 5 or more payers in 7 days, then a transfer of at least a quarter of the inflow to at most 2 payees, from an account under 90 days old or draining half the inflow; or a payee first seen within 14 days collecting from 5 or more customers | Review | FATF Illicit Financial Flows from Cyber-Enabled Fraud (2023); FATF Professional Money Laundering (2018) |
| `dormant_reactivation` | `CUSTOM_TM_DORMANT_REACTIVATION` | 5 000 or more after 180 days without activity | Review | EBA/GL/2021/02 4.46; AMLR (EU) 2024/1624 Art. 26 |
| `high_risk_corridor` | `GEO_HIGH_RISK` | counterparty in a higher-risk country (default: the FATF call-for-action list), 1 000 or more or the first time | Review | FATF R.19; AMLR (EU) 2024/1624 Arts. 29-31 |
| `card_testing` | `CARD_TESTING` | 5 or more small (under 2.00) or 3 or more declined card attempts within 10 to 15 min | Block | RTS (EU) 2018/389 Art. 2(2) |
| `atm_cash` | `CUSTOM_TM_ATM_CASH` | more than 6 ATM operations or more than 3 000 cash in 24 h | Review | FATF Professional Money Laundering (2018), mule cash withdrawals; EBA/GL/2021/02 Guideline 2 |
| `sanctions_adjacent` | `CUSTOM_TM_SANCTIONS_ADJACENT` | counterparty or IP country under sectoral restrictive measures (default RU, BY) | Review | FATF R.6 and R.7; AMLR (EU) 2024/1624 Art. 20(1)(d) |
| `threshold_report` | `CUSTOM_TM_THRESHOLD_REPORT` | cash in 24 h reaches the reporting threshold (10 000) | Allow, report flagged | 31 CFR 1010.311; AMLR (EU) 2024/1624 Art. 80 |
| `unusual_amount` | `CUSTOM_TM_UNUSUAL_AMOUNT` | outbound 3 000 or more and 10 times the customer's own p95 | Review | EBA/GL/2021/02 4.46; AMLR (EU) 2024/1624 Art. 34 |

Amounts are in the reporting currency. Every threshold, window and country list is in
the rules config ([examples/config.sexp](examples/config.sexp)); a typology can be
disabled or run in shadow mode (hits recorded at Allow).

Severities can be overridden per typology, per country (the event's counterparty or IP
country), or both, with `overrides` in the same file
([examples/config-countermeasures.sexp](examples/config-countermeasures.sexp)):

```
(overrides
 (((countries (KP IR MM)) (severity Block))       ; FATF countermeasures: block, not hold
  ((typologies (structuring)) (severity Block))))
```

An override changes the severity of a rule's hit, and with it the decision (`Allow`
allows, `Challenge` and `Review` hold, `Block` blocks); it never creates a hit. When
several match, the most specific wins (typologies and countries, then countries only,
then typologies only), then the most severe. The applied override is recorded in the
hit's evidence (`severity_override`). Validation refuses an override with neither list,
one naming `threshold_report` (a reporting obligation, always Allow), a disabled or
shadow typology, a repeated entry, or two overrides covering the same typologies and
countries. The country lists are
examples; replace them with lists the compliance function maintains. Counterparty names
are screened by the sibling `sanctions-screening` library; these rules only see
countries.

Every hit carries an `Explanation`: the typology, the risk-rules reason code (built-in
codes where risk-rules has the typology), a summary naming the facts compared
("3 amounts in \[8500.00, 10000.00) within 7d, this one 9900.00; together 28600.00"),
the facts as key-value evidence, and the reference above. `transaction-monitoring
typologies` prints the catalogue.

## Model

Gradient-boosted trees (scikit-learn `HistGradientBoostingClassifier`, 102 trees)
trained by [ml/train.py](ml/train.py) on rows that `transaction-monitoring features`
computes with the same OCaml code that scores live events, so there is no second
implementation of any feature. The model file is risk-models' format 1 and is scored
by `Risk_models.Scorer` against this library's feature schema (76 columns: risk-rules'
encoded features, then the AML features); loading refuses a model built on another
schema.

Pipeline ([ml/run.sh](ml/run.sh)): `generate` (5 000 customers, 75 days, 862 904
events, 8% of customers with one episode of a typology, a third of the episodes
deliberately under the rule thresholds, benign look-alikes such as businesses paid by
many clients, invoices just under 10 000, travel cash and large one-off purchases),
`features`, then `train.py`. The split is by time with a gap for the label delay: days
0-13 warm the profiles and are dropped, 14-44 train, 45-54 calibrate and choose
thresholds, 55-58 are a gap, 59-74 test. Positive events are about 0.2% of rows;
positives are weighted 50x in training and the weight is taken out of the base score,
so the output is a probability under the training prior.

Thresholds (chosen on the calibration window): review at 0.000524, the lowest score
that keeps model alerts within 60 a day (0.5% of the 11 500 events a day); block at
0.117, the lowest score above it with 98% precision.

Results on the test window (193 840 events, 428 positive events in 92 episodes):

| | Rules alone | Model alone (review threshold) | Rules or model |
|---|---|---|---|
| Alerts a day | 57.6 | 40.5 | 83.9 |
| Alert precision (events) | 0.207 | 0.610 | 0.296 |
| Event recall | 0.474 | 0.981 | 0.988 |
| Episode recall (any event of the episode alerted) | 81 of 92 | 91 of 92 | 92 of 92 |

| Model metric | Value |
|---|---|
| ROC-AUC | 0.9999 |
| Average precision | 0.960 |
| Precision / recall when investigators review the top 10 a day | 0.877 / 0.332 |
| Top 25 a day | 0.719 / 0.675 |
| Top 50 a day | 0.499 / 0.935 |
| Block threshold on test: alerts a day, precision, episode recall | 22.4, 0.947, 0.891 |

Episode recall by typology on the test window (rules / model / either): structuring
12/13, 13/13, 13/13; smurfing 6/7, 7/7, 7/7; rapid movement 9/9 for all; mule fan-in
8/8 for all; dormant reactivation 6/6 for all; high-risk corridor 14/15, 14/15, 15/15;
card testing 5/8, 8/8, 8/8; ATM cash 8/13, 13/13, 13/13; sanctions-adjacent 13/13 for
all. The rules miss exactly the subtle episodes the generator places under their
thresholds; the model catches those because it sees the same signals without a hard
cut-off. On made-up data with injected patterns this separation is far easier than on
real traffic, where a ROC-AUC near 1 would mean a leak.

`models/tm-aml-gbdt/metrics.json` has every number above. The default policy runs the
model in shadow mode: its score and top three reasons are recorded on every decision,
and only the rules decide. Make it `Active` after a shadow period (risk-models
suggests two weeks with score PSI below 0.1), and compare first with `replay
-baseline-policy`.

### Explanations

Every decision a person will look at (every `Hold` and `Block`, and every score from the
abstain band or the hold threshold up) carries `attribution`: risk-models' exact
path-dependent attributions (TreeSHAP-style, from the node counts in the export), the
five features with the largest contributions to the log-odds under the names in
`Features.label`, the base value and the sum of the rest, so that base plus
contributions equals the model's margin. The model explanation's top reasons come from
the same attributions. They are model contributions, not causes, and say so wherever
they are printed. The review queue in [examples/quickstart.ml](examples/quickstart.ml):

```
review alert-out-2:
  [review] high_risk_corridor: 1200.00 to IR, first time
  ...
  model contributions to the log-odds (base -13.48):
    amount                                     +3.00
    account age in days                        +2.91
    hours since the payee was added            +2.56
    counterparty in the country of residence   +1.33
    customers paying the counterparty, 7 days  +1.30
    all other features                         +3.91
```

A model export without node counts still loads; decisions then carry the cheaper path
reason codes and no attribution.

### Abstaining

A policy may let the model abstain on scores too close to the hold threshold to trust
([examples/policy-abstain.sexp](examples/policy-abstain.sexp)): within `min_confidence`
of `hold_at` in log-odds, an abstention either holds the payment with the explanation
"model uncertain" (`route Hold`) or leaves the decision to the rules
(`route Rules_only`). A score at or above the block threshold never abstains; in shadow
mode abstentions are only recorded (`Decision.model_abstained`).

`replay` prints the table to choose the band from: the accuracy, precision and recall of
the model's own decision at the hold threshold when it abstains on the scores closest to
it, per coverage. On the whole synthetic history (862 904 events, 2 004 labelled,
including the warm-up days):

| Coverage | Accuracy | Precision | Recall | Abstain band |
|---|---|---|---|---|
| 1.00 | 0.9820 | 0.114 | 0.996 | none |
| 0.99 | 0.9865 | 0.146 | 0.988 | \[0.000206, 0.00133) |
| 0.95 | 0.9960 | 0.374 | 0.968 | \[1.72e-05, 0.0157) |
| 0.90 | 0.9974 | 0.484 | 0.960 | \[6.04e-06, 0.0435) |
| 0.80 | 0.9983 | 0.623 | 0.949 | \[2.41e-06, 0.102) |

Area under the risk-coverage curve 0.00355. With the example band (0.5 in log-odds,
scores in \[0.000318, 0.000864), 4 415 abstentions) against the active policy without
one: routed to holds, 2 131 more alerts and 2 more labelled events caught (recall 0.997
to 0.998, alert precision 0.063 to 0.059); left to the rules, 1 932 fewer alerts and 5
labelled events missed (recall 0.995, precision 0.067).

Parity: `test/fixtures/parity.csv` holds 400 test rows (with extra missing values and
values exactly on split thresholds) and scikit-learn's own margin, probability and
score; the OCaml scores match within 1e-9 (the margin bit for bit: trees only add and
compare).

## Latency

`transaction-monitoring bench -model models/tm-aml-gbdt/model.json`, release build,
Apple M-series laptop, profiles warmed with 28 days of 2 000 customers, measured on
the following two days' events:

| Step | Mean (core_bench) | Words allocated |
|---|---|---|
| Both profile views | 1.5 us | 112 |
| Feature vector | 2.4 us | 940 |
| Views and rules | 2.3 us | 639 |
| Views, rules and model | 5.9 us | 1 424 |
| Model contributions (attributions only) | 65 us | 3 |
| Explanation (attributions and top five) | 68 us | 1 060 |

Full step (decide, then update both profiles), per event: rules p50 4 us, p99 9 us;
rules and model p50 5 us, p99 37 us, the tail being the decisions that get an
explanation (percentiles quantised to 1 us by the macOS clock; the explanation rows were
measured with other builds running). The explanation cost comes from the model's deep
trees: leaves with up to 14 distinct features, tabulated up to 10 (27 MB per explainer)
and integrated at run time beyond.
The store write is extra: one transaction per event on PostgreSQL. A full replay of the
862 904 synthetic events with rules, model, explanations and a baseline runs in about
8 s.

## Storage

`Store.S` in domain types with `Store_memory` and `Store_pg` (migrations embedded from
[migrations/](migrations/)).

The library owns one PostgreSQL schema, `transaction_monitoring` by default
(`Store_pg.connect ~schema`): it creates it, and its tables and its own
`schema_migrations` table live inside it. Nothing is written outside it, so the host
application's migrations table and other libraries (sanctions-screening keeps
`sanctions_screening.schema_migrations`) never clash, though all start at version 1; a
test migrates the host, sanctions-screening and this library into one database. A
database set up by 0.1.0 or 0.1.1 (bookkeeping rows in the shared `schema_migrations` of
the search path) is adopted on connect: the rows with this library's old checksums are
copied into the schema and nothing is applied twice; the old rows may then be deleted by
hand.

Revocations (migration `0003`) live in a `revocations` table; events gained their
commit sequence and counterparty so a profile's history can be read back in the order it
was applied (rows written before `0003` get their counterparty from the body on the
first connect). One
commit per event, in one transaction: the event row first with `ON CONFLICT DO
NOTHING` (a redelivered or concurrently processed event finds the stored decision),
then the decision, its alert and alert events, and both profile snapshots, which only
replace a stored snapshot with fewer events. Alert updates are optimistic on the
alert's version. Decision and alert bodies are versioned (migration `0002`): new rows
are `Stable.V2` with `body_version` 2 and the explanation also as JSON in an
`attribution` column (risk-models' format 1, queryable with `->>`); rows written before
it are V1 and read back upgraded. A conformance scenario prints the same transcript on both stores, and
a test restarts the monitor halfway through a synthetic history, on each store, and
checks every decision equals an uninterrupted replay.

## Stable types and tests

`Event`, `Decision` (V1 and V2), `Alert` (V1 and V2, and its events), `Explanation`,
`Typology` and the profile snapshots have `Stable` bin_io types; the event, snapshot,
decision and alert digests are pinned in `test/test_event.ml`. Tests: profile properties (incremental equals
recomputed, snapshot resume), sketches (P-square accuracy on skewed data), one expect
test per typology with its explanation, features, the alert lifecycle with a
Quickcheck invariant over random command sequences, model parity, the conformance
scenario on memory and PostgreSQL, the monitor (idempotency, overload, restart), the
replay diff and accuracy-coverage report, explanations on held decisions and both
abstain routes, V1 rows read back from PostgreSQL, the example files and the quickstart
output.

PostgreSQL tests use `TM_TEST_DATABASE_URL` when set (CI's Postgres service) and
otherwise start a throwaway cluster with pg-async's `Pg_test` (needs the server
binaries).

## Build

```sh
dune build @all @runtest
```

CI pins `pg-async`, `fixed-decimal`, `money`, `ledger`, `payments-core`, `risk-rules`,
`risk-models`, `kyc-core` and `sanctions-screening` (with `sanctions-screening-pg`, for
the two-libraries test) from GitHub in that order, installs libpq, runs a Postgres service,
and builds the docs with odoc warnings as errors.

## Known gaps

- Synthetic data only; no public transaction-monitoring dataset with these typologies
  is downloadable without registration.
- Windows are bucket-aligned approximations (exact and reproducible, but "1 h" reaches
  55 to 60 minutes back); the counterparty table keeps 64 entries, so distinct-payer
  counts saturate for customers with more counterparties than that.
- No graph features beyond one hop (customer and counterparty profiles); cycles and
  multi-hop layering need a graph store.
- One monitor instance per customer partition: events of one customer must reach the
  same instance in order. Several instances on one store stay correct (idempotent
  commits, monotonic snapshots) but a customer's profile is only as fresh as the last
  snapshot an instance loaded.
- The reporting currency is assumed to have two decimals in the model features
  (thresholds compare integer minor units and are exact for any currency).
- Alerts are per event; grouping several alerts of one customer into one case is left
  to the case-management layer.
- `Monitor.revoke` reads the customer's and the counterparty's whole stored history; a
  counterparty paid by very many customers makes it slow. Decisions taken after the
  revoked event are not revisited.
- No name screening here (see `sanctions-screening`), no FX conversion (callers supply
  `base_amount`).
