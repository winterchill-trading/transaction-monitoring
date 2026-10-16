-- Decisions and alerts now carry the model's per-feature explanation. Bodies written from
-- this migration on are the Stable.V2 encodings (body_version 2); older rows keep their
-- V1 bodies and are upgraded on read. The explanation is also stored as JSON (the
-- risk-models Explanation format 1) so operators can query it without decoding bodies.
ALTER TABLE decisions
  ADD COLUMN body_version smallint NOT NULL DEFAULT 1 CHECK (body_version IN (1, 2)),
  ADD COLUMN attribution jsonb;

ALTER TABLE alerts
  ADD COLUMN body_version smallint NOT NULL DEFAULT 1 CHECK (body_version IN (1, 2)),
  ADD COLUMN attribution jsonb;
