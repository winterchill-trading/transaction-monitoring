-- transaction-monitoring owns this schema; no other service reads or joins it.
-- Bodies are the Bin_prot Stable.V1 encodings of the library types; the other columns
-- exist for lookups and operator queries.
CREATE SCHEMA IF NOT EXISTS transaction_monitoring;

CREATE TABLE transaction_monitoring.events (
  event_id text PRIMARY KEY,
  customer text NOT NULL,
  occurred_at timestamptz NOT NULL,
  body bytea NOT NULL
);

CREATE TABLE transaction_monitoring.decisions (
  event_id text PRIMARY KEY REFERENCES transaction_monitoring.events (event_id),
  customer text NOT NULL,
  outcome text NOT NULL CHECK (outcome IN ('allow', 'hold', 'block')),
  score double precision,
  model_version text,
  decided_at timestamptz NOT NULL,
  body bytea NOT NULL
);

CREATE INDEX decisions_customer ON transaction_monitoring.decisions (customer, decided_at);

CREATE TABLE transaction_monitoring.alerts (
  alert_id text PRIMARY KEY,
  event_id text NOT NULL UNIQUE REFERENCES transaction_monitoring.events (event_id),
  customer text NOT NULL,
  status text NOT NULL CHECK (status IN ('open', 'assigned', 'closed')),
  sar_candidate boolean NOT NULL,
  version integer NOT NULL CHECK (version > 0),
  opened_at timestamptz NOT NULL,
  updated_at timestamptz NOT NULL,
  body bytea NOT NULL
);

CREATE INDEX alerts_status ON transaction_monitoring.alerts (status, opened_at);
CREATE INDEX alerts_customer ON transaction_monitoring.alerts (customer, opened_at);

CREATE TABLE transaction_monitoring.alert_events (
  alert_id text NOT NULL REFERENCES transaction_monitoring.alerts (alert_id),
  seq integer NOT NULL,
  body bytea NOT NULL,
  PRIMARY KEY (alert_id, seq)
);

CREATE TABLE transaction_monitoring.customer_profiles (
  customer text PRIMARY KEY,
  events bigint NOT NULL,
  body bytea NOT NULL
);

CREATE TABLE transaction_monitoring.counterparty_profiles (
  counterparty text PRIMARY KEY,
  events bigint NOT NULL,
  body bytea NOT NULL
);

CREATE TABLE transaction_monitoring.model_versions (
  version text PRIMARY KEY,
  name text NOT NULL,
  policy text NOT NULL,
  activated_at timestamptz NOT NULL
);
