(lang dune 3.17)

(name transaction-monitoring)

(generate_opam_files true)

(license MIT)

(authors "WinterChill AI")

(maintainers "hello@winterchill.xyz")

(source
 (github winterchill-trading/transaction-monitoring))

(homepage "https://github.com/winterchill-trading/transaction-monitoring")

(bug_reports
 "https://github.com/winterchill-trading/transaction-monitoring/issues")

(documentation
 "https://github.com/winterchill-trading/transaction-monitoring#readme")

(package
 (name transaction-monitoring)
 (synopsis
  "Real-time AML transaction monitoring: rolling profiles, typology rules, GBDT scores, alerts")
 (description
  "transaction-monitoring screens every payment a bank books or initiates, in process and in tens of microseconds. Each event updates bounded-memory rolling profiles of the customer and the counterparty (velocity windows, amount quantiles, new counterparties, dormancy, round amounts, sub-threshold sums, pass-through, geographic novelty); a library of typology rules (structuring, smurfing, rapid movement, mules, dormant accounts, high-risk corridors, card testing, ATM cash, threshold reporting) built on risk-rules and a gradient-boosted model scored through risk-models produce an Allow, Hold or Block decision with reasons. Holds open alerts with a case lifecycle; decisions, alerts and profile snapshots persist through a Store module type with memory and PostgreSQL implementations. Requires the OxCaml compiler.")
 (tags
  (aml transaction-monitoring fintech machine-learning oxcaml))
 (depends
  oxcaml
  (ocaml
   (and
    (>= 5.2.0)
    (< 5.3~)))
  dune
  (core
   (and
    (>= v0.18~preview.130.106+341)
    (< v0.19~)))
  (ppx_jane
   (and
    (>= v0.18~preview.130.106+341)
    (< v0.19~)))
  core_unix
  async
  core_bench
  yojson
  zarith
  money
  ledger
  risk-rules
  risk-models
  pg-async
  (base_quickcheck :with-test)
  (sanctions-screening-pg :with-test)
  (odoc :with-doc)))
