-- Revoked events: decided payments that never happened. The event and its decision stay;
-- profiles are rebuilt from the events that are not revoked, in commit order, so events
-- gain their commit sequence and counterparty. Rows from before this migration get a
-- sequence in physical order (insertion order for an append-only table) and their
-- counterparty is filled in by Store_pg.connect from the body.
ALTER TABLE events
  ADD COLUMN counterparty text,
  ADD COLUMN seq bigserial;

CREATE INDEX events_customer_seq ON events (customer, seq);
CREATE INDEX events_counterparty_seq ON events (counterparty, seq);

CREATE TABLE revocations (
  event_id text PRIMARY KEY REFERENCES events (event_id),
  reason text NOT NULL,
  revoked_at timestamptz NOT NULL
);
