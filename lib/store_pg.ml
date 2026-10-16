open! Core
open! Async
open Pg_async

type t =
  { pool : Pool.t
  ; schema : string
  ; description : string
  ; applied_migrations : int list
  }

let kind t = t.description
let applied_migrations t = t.applied_migrations
let schema t = t.schema
let default_schema = "transaction_monitoring"

(* Every query names its tables as [{s}.table]; [q] puts the configured schema in. The
   prepared-statement cache is keyed by the final text, so each schema gets its own. *)
let q schema sql = String.substr_replace_all sql ~pattern:"{s}." ~with_:(schema ^ ".")

let or_error ~label d =
  d >>| fun r -> Pg_error.to_or_error r |> Or_error.tag ~tag:("store: " ^ label)
;;

let migrations =
  List.map Migrations_sql.all ~f:(fun (version, name, sql) ->
    { Migrate.Migration.version; name; sql })
;;

(* Schema names are spliced into SQL, so only plain lower-case identifiers are accepted:
   they need no quoting and cannot inject anything. *)
let validate_schema s =
  let ok i c = Char.is_lowercase c || Char.equal c '_' || (i > 0 && Char.is_digit c) in
  if (not (String.is_empty s))
     && String.length s <= 63
     && String.for_alli s ~f:ok
     && not (String.is_prefix s ~prefix:"pg_")
  then Ok s
  else
    Or_error.error_s
      [%message
        "schema must be a lower-case identifier ([a-z_][a-z0-9_]*, at most 63 bytes, not \
         pg_*)"
          s]
;;

(* Versions 0.1.0 and 0.1.1 qualified every table with [transaction_monitoring.] and let
   pg-async record the migrations in the unqualified [schema_migrations] of the
   connection's search path, a table the host application (and any other library) shares.
   These are the checksums those files had; a row with one of them is ours. *)
let legacy_checksums =
  [ 1, "d27893bacb3d7761112ee47fd1249ca1"; 2, "21eb8b5fd740fc9ad3e4cd5d2cdaaf58" ]
;;

let text_row name = Row.column name Type.(nullable string)

(* Moves the bookkeeping of an older install into [schema.schema_migrations]: only when
   that table does not exist yet, the schema is the one the old versions used, and the
   shared table has rows with our old checksums. The shared rows are left alone (they may
   be deleted by hand: versions 1 and 2 with the checksums above). *)
let adopt_legacy conn ~schema =
  let open Deferred.Result.Let_syntax in
  let%bind ours =
    Query.fetch_one
      conn
      ~label:"migrations_table"
      "SELECT to_regclass($1)::text AS t"
      ~params:[ Param.string (schema ^ ".schema_migrations") ]
      ~row:(text_row "t")
  in
  let%bind shared =
    Query.fetch_one
      conn
      ~label:"legacy_migrations_table"
      "SELECT (SELECT quote_ident(n.nspname) FROM pg_class c JOIN pg_namespace n ON \
       n.oid = c.relnamespace WHERE c.oid = to_regclass('schema_migrations')) AS t"
      ~row:(text_row "t")
  in
  match ours, shared with
  | Some _, _ | None, None -> return []
  | None, Some _ when not (String.equal schema default_schema) -> return []
  | None, Some shared_schema ->
    let%bind rows =
      Query.fetch_all
        conn
        ~label:"legacy_migrations"
        (sprintf
           "SELECT version, checksum, applied_at FROM %s.schema_migrations"
           shared_schema)
        ~row:
          (let open Row.Let_syntax in
           let%map_open version = column "version" int
           and checksum = column "checksum" string
           and applied_at = column "applied_at" time_ns in
           version, checksum, applied_at)
    in
    let adopted =
      List.filter_map rows ~f:(fun (version, checksum, applied_at) ->
        if List.mem legacy_checksums (version, checksum) ~equal:[%equal: int * string]
        then
          List.find migrations ~f:(fun m -> m.version = version)
          |> Option.map ~f:(fun m -> m, applied_at)
        else None)
    in
    if List.is_empty adopted
    then return []
    else
      Tx.with_transaction conn ~f:(fun conn ->
        (* The same table pg-async's Migrate creates, in our schema. *)
        let%bind () =
          Connection.execute_script
            conn
            ~label:"adopt_migrations"
            (sprintf
               "CREATE TABLE IF NOT EXISTS %s.schema_migrations (version bigint PRIMARY \
                KEY, name text NOT NULL, checksum text NOT NULL, applied_at timestamptz \
                NOT NULL DEFAULT now())"
               schema)
        in
        Deferred.Result.List.map adopted ~f:(fun ((m : Migrate.Migration.t), at) ->
          Query.execute
            conn
            ~label:"adopt_migration"
            (sprintf
               "INSERT INTO %s.schema_migrations (version, name, checksum, applied_at) \
                VALUES ($1, $2, $3, $4)"
               schema)
            ~params:
              [ Param.int m.version
              ; Param.string m.name
              ; Param.string (Migrate.Migration.checksum m)
              ; Param.time_ns at
              ]
          >>| fun (_ : int) -> m.version))
;;

(* Creates the schema, adopts an older install's bookkeeping, then lets pg-async apply the
   pending migrations with [search_path] set to the schema, so its [schema_migrations]
   table and every table the migrations create land inside the schema. The session lock
   serializes instances starting at once (CREATE SCHEMA IF NOT EXISTS is not safe to
   race); closing the connection releases it. *)
let migrate conn ~schema =
  let open Deferred.Result.Let_syntax in
  let%bind () =
    Connection.execute_script
      conn
      ~label:"migration_lock"
      (sprintf "SELECT pg_advisory_lock(hashtext('transaction-monitoring:%s'))" schema)
  in
  let%bind () =
    Connection.execute_script
      conn
      ~label:"create_schema"
      (sprintf "CREATE SCHEMA IF NOT EXISTS %s" schema)
  in
  let%bind adopted = adopt_legacy conn ~schema in
  let%bind () =
    Connection.execute_script
      conn
      ~label:"search_path"
      (sprintf "SET search_path TO %s" schema)
  in
  return adopted
;;

(* Events stored before migration 0003 have no counterparty column value; it is in the
   body. Filled in batches, once (later events are written with it). *)
let backfill_counterparties conn ~schema =
  let open Deferred.Result.Let_syntax in
  let rec batch () =
    let%bind rows =
      Query.fetch_all
        conn
        ~label:"backfill_select"
        (q schema "SELECT body FROM {s}.events WHERE counterparty IS NULL LIMIT 1000")
        ~row:
          (Row.map (Row.column "body" Type.bytea) ~f:(fun b ->
             Bin_prot.Reader.of_string Event.Stable.V1.bin_reader_t b))
    in
    if List.is_empty rows
    then return ()
    else (
      let%bind () =
        Deferred.Result.List.iter rows ~f:(fun (e : Event.t) ->
          Query.execute
            conn
            ~label:"backfill_update"
            (q schema "UPDATE {s}.events SET counterparty = $2 WHERE event_id = $1")
            ~params:
              [ Param.string (Ids.Event_id.to_string e.id)
              ; Param.string (Ids.Counterparty_id.to_string e.counterparty)
              ]
          |> Deferred.Result.ignore_m)
      in
      batch ())
  in
  batch ()
;;

let connect ?database_url ?(schema = default_schema) ?(max_connections = 4) () =
  let open Deferred.Or_error.Let_syntax in
  let%bind schema = Deferred.return (validate_schema schema) in
  let%bind config =
    Deferred.return
      (match database_url with
       | Some url -> Config.of_url url
       | None -> Config.of_env ())
  in
  let config = { config with application_name = "transaction-monitoring" } in
  let%bind conn = or_error ~label:"connect" (Connection.connect config) in
  let%bind.Deferred result =
    match%bind.Deferred migrate conn ~schema with
    | Error e -> Deferred.return (Error (Pg_error.to_error e))
    | Ok (_ : int list) ->
      Deferred.map
        (Migrate.apply conn migrations)
        ~f:(Result.map_error ~f:Migrate.Error.to_error)
  in
  let%bind.Deferred result =
    match result with
    | Error _ -> Deferred.return result
    | Ok applied ->
      Deferred.map (backfill_counterparties conn ~schema) ~f:(function
        | Ok () -> Ok applied
        | Error e -> Error (Pg_error.to_error e))
  in
  let%bind.Deferred () = Connection.close conn in
  let%map applied_migrations =
    Deferred.return (Or_error.tag result ~tag:("migrating schema " ^ schema))
  in
  { pool = Pool.create ~max_size:max_connections config
  ; schema
  ; description = sprintf "postgres %s schema %s" (Config.to_string_hum config) schema
  ; applied_migrations
  }
;;

let close t = Pool.close t.pool

(* Bin_prot bodies *)

let encode writer v = Bin_prot.Writer.to_string writer v
let decode reader s = Bin_prot.Reader.of_string reader s

let body_row reader =
  let open Row.Let_syntax in
  let%map_open body = column "body" bytea in
  decode reader body
;;

(* Decisions and alerts have versioned bodies (migration 0002): V1 rows written before it
   are upgraded on read. *)
let versioned_row ~v1 ~v2 ~upgrade =
  let open Row.Let_syntax in
  let%map_open body = column "body" bytea
  and version = column "body_version" int in
  match version with
  | 1 -> upgrade (decode v1 body)
  | _ -> decode v2 body
;;

let decision_row =
  versioned_row
    ~v1:Decision.Stable.V1.bin_reader_t
    ~v2:Decision.Stable.V2.bin_reader_t
    ~upgrade:Decision.Stable.V2.of_v1
;;

let alert_row =
  versioned_row
    ~v1:Alert.Stable.V1.bin_reader_t
    ~v2:Alert.Stable.V2.bin_reader_t
    ~upgrade:Alert.Stable.V2.of_v1
;;

let attribution_param a =
  Param.option Type.json (Option.map a ~f:Risk_models.Explanation.to_json_string)
;;

let alert_event_row = body_row Alert.Event.Stable.V1.bin_reader_t
let customer_row = body_row Profile.Customer.Stable.Snapshot.V1.bin_reader_t
let counterparty_row = body_row Profile.Counterparty.Stable.Snapshot.V1.bin_reader_t

let find_decision t id =
  Pool.fetch_opt
    t.pool
    ~label:"find_decision"
    (q t.schema "SELECT body, body_version FROM {s}.decisions WHERE event_id = $1")
    ~params:[ Param.string (Ids.Event_id.to_string id) ]
    ~row:decision_row
  |> or_error ~label:"find_decision"
;;

let insert_alert_events t conn (alert : Alert.t) events =
  let first = alert.version - List.length events + 1 in
  Deferred.Result.List.iteri events ~f:(fun i e ->
    Query.execute
      conn
      ~label:"insert_alert_event"
      (q
         t.schema
         "INSERT INTO {s}.alert_events (alert_id, seq, body) VALUES ($1, $2, $3)")
      ~params:
        [ Param.string (Ids.Alert_id.to_string alert.id)
        ; Param.int (first + i)
        ; Param.bytea (encode Alert.Event.Stable.V1.bin_writer_t e)
        ]
    |> Deferred.Result.ignore_m)
;;

let alert_params (a : Alert.t) =
  [ Param.string (Ids.Alert_id.to_string a.id)
  ; Param.string (Ids.Event_id.to_string a.event_id)
  ; Param.string (Ids.Customer_id.to_string a.customer)
  ; Param.string (Alert.Status.to_string a.status)
  ; Param.bool a.sar_candidate
  ; Param.int a.version
  ; Param.time_ns a.opened_at
  ; Param.time_ns a.updated_at
  ; Param.bytea (encode Alert.Stable.V2.bin_writer_t a)
  ; Param.int 2
  ; attribution_param a.attribution
  ]
;;

let commit t (c : Store.Commit.t) =
  Pool.with_transaction t.pool ~f:(fun conn ->
    let open Deferred.Result.Let_syntax in
    let%bind inserted =
      Query.execute
        conn
        ~label:"insert_event"
        (q
           t.schema
           "INSERT INTO {s}.events (event_id, customer, occurred_at, body, counterparty) \
            VALUES ($1, $2, $3, $4, $5) ON CONFLICT (event_id) DO NOTHING")
        ~params:
          [ Param.string (Ids.Event_id.to_string c.event.id)
          ; Param.string (Ids.Customer_id.to_string c.event.customer)
          ; Param.time_ns c.event.occurred_at
          ; Param.bytea (encode Event.Stable.V1.bin_writer_t c.event)
          ; Param.string (Ids.Counterparty_id.to_string c.event.counterparty)
          ]
    in
    if inserted = 0
    then
      Query.fetch_one
        conn
        ~label:"duplicate_decision"
        (q t.schema "SELECT body, body_version FROM {s}.decisions WHERE event_id = $1")
        ~params:[ Param.string (Ids.Event_id.to_string c.event.id) ]
        ~row:decision_row
      >>| fun d -> Store.Commit_result.Duplicate d
    else (
      let d = c.decision in
      let%bind (_ : int) =
        Query.execute
          conn
          ~label:"insert_decision"
          (q
             t.schema
             "INSERT INTO {s}.decisions (event_id, customer, outcome, score, \
              model_version, decided_at, body, body_version, attribution) VALUES ($1, \
              $2, $3, $4, $5, $6, $7, $8, $9)")
          ~params:
            [ Param.string (Ids.Event_id.to_string d.event_id)
            ; Param.string (Ids.Customer_id.to_string d.customer)
            ; Param.string (Decision.Outcome.to_string d.outcome)
            ; Param.option Type.float d.score
            ; Param.option Type.string (Option.map d.model ~f:(fun m -> m.version))
            ; Param.time_ns d.decided_at
            ; Param.bytea (encode Decision.Stable.V2.bin_writer_t d)
            ; Param.int 2
            ; attribution_param d.attribution
            ]
      in
      let%bind () =
        match c.alert with
        | None -> return ()
        | Some (alert, events) ->
          let%bind (_ : int) =
            Query.execute
              conn
              ~label:"insert_alert"
              (q
                 t.schema
                 "INSERT INTO {s}.alerts (alert_id, event_id, customer, status, \
                  sar_candidate, version, opened_at, updated_at, body, body_version, \
                  attribution) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11) ON \
                  CONFLICT (alert_id) DO NOTHING")
              ~params:(alert_params alert)
          in
          insert_alert_events t conn alert events
      in
      let customer = Profile.Customer.of_snapshot c.customer in
      let%bind (_ : int) =
        Query.execute
          conn
          ~label:"upsert_customer_profile"
          (q
             t.schema
             "INSERT INTO {s}.customer_profiles (customer, events, body) VALUES ($1, $2, \
              $3) ON CONFLICT (customer) DO UPDATE SET events = EXCLUDED.events, body = \
              EXCLUDED.body WHERE {s}.customer_profiles.events < EXCLUDED.events")
          ~params:
            [ Param.string
                (Ids.Customer_id.to_string (Profile.Customer.customer customer))
            ; Param.int (Profile.Customer.events customer)
            ; Param.bytea
                (encode Profile.Customer.Stable.Snapshot.V1.bin_writer_t c.customer)
            ]
      in
      let cp = Profile.Counterparty.of_snapshot c.counterparty in
      let%map (_ : int) =
        Query.execute
          conn
          ~label:"upsert_counterparty_profile"
          (q
             t.schema
             "INSERT INTO {s}.counterparty_profiles (counterparty, events, body) VALUES \
              ($1, $2, $3) ON CONFLICT (counterparty) DO UPDATE SET events = \
              EXCLUDED.events, body = EXCLUDED.body WHERE \
              {s}.counterparty_profiles.events < EXCLUDED.events")
          ~params:
            [ Param.string
                (Ids.Counterparty_id.to_string (Profile.Counterparty.counterparty cp))
            ; Param.int (Profile.Counterparty.events cp)
            ; Param.bytea
                (encode
                   Profile.Counterparty.Stable.Snapshot.V1.bin_writer_t
                   c.counterparty)
            ]
      in
      Store.Commit_result.Committed))
  |> or_error ~label:"commit"
;;

let load_customer t id =
  Pool.fetch_opt
    t.pool
    ~label:"load_customer"
    (q t.schema "SELECT body FROM {s}.customer_profiles WHERE customer = $1")
    ~params:[ Param.string (Ids.Customer_id.to_string id) ]
    ~row:customer_row
  |> or_error ~label:"load_customer"
;;

let load_counterparty t id =
  Pool.fetch_opt
    t.pool
    ~label:"load_counterparty"
    (q t.schema "SELECT body FROM {s}.counterparty_profiles WHERE counterparty = $1")
    ~params:[ Param.string (Ids.Counterparty_id.to_string id) ]
    ~row:counterparty_row
  |> or_error ~label:"load_counterparty"
;;

let find_alert t id =
  Pool.fetch_opt
    t.pool
    ~label:"find_alert"
    (q t.schema "SELECT body, body_version FROM {s}.alerts WHERE alert_id = $1")
    ~params:[ Param.string (Ids.Alert_id.to_string id) ]
    ~row:alert_row
  |> or_error ~label:"find_alert"
;;

let update_alert t (alert : Alert.t) events =
  let expected = alert.version - List.length events in
  Pool.with_transaction t.pool ~f:(fun conn ->
    let open Deferred.Result.Let_syntax in
    let%bind updated =
      Query.execute
        conn
        ~label:"update_alert"
        (q
           t.schema
           "UPDATE {s}.alerts SET status = $4, sar_candidate = $5, version = $6, \
            updated_at = $8, body = $9, body_version = $10, attribution = $11 WHERE \
            alert_id = $1 AND event_id = $2 AND customer = $3 AND opened_at = $7 AND \
            version = $12")
        ~params:(alert_params alert @ [ Param.int expected ])
    in
    if updated = 1
    then insert_alert_events t conn alert events >>| fun () -> Store.Alert_update.Updated
    else
      Query.fetch_opt
        conn
        ~label:"alert_conflict"
        (q t.schema "SELECT body, body_version FROM {s}.alerts WHERE alert_id = $1")
        ~params:[ Param.string (Ids.Alert_id.to_string alert.id) ]
        ~row:alert_row
      >>| fun stored -> Store.Alert_update.Conflict stored)
  |> or_error ~label:"update_alert"
;;

let alerts ?status ?customer t =
  Pool.fetch_all
    t.pool
    ~label:"alerts"
    (q
       t.schema
       "SELECT body, body_version FROM {s}.alerts WHERE ($1::text IS NULL OR status = \
        $1) AND ($2::text IS NULL OR customer = $2) ORDER BY opened_at, alert_id COLLATE \
        \"C\"")
    ~params:
      [ Param.option Type.string (Option.map status ~f:Alert.Status.to_string)
      ; Param.option Type.string (Option.map customer ~f:Ids.Customer_id.to_string)
      ]
    ~row:alert_row
  |> or_error ~label:"alerts"
;;

let alert_events t id =
  Pool.fetch_all
    t.pool
    ~label:"alert_events"
    (q t.schema "SELECT body FROM {s}.alert_events WHERE alert_id = $1 ORDER BY seq")
    ~params:[ Param.string (Ids.Alert_id.to_string id) ]
    ~row:alert_event_row
  |> or_error ~label:"alert_events"
;;

let decisions ?customer t =
  Pool.fetch_all
    t.pool
    ~label:"decisions"
    (q
       t.schema
       "SELECT body, body_version FROM {s}.decisions WHERE ($1::text IS NULL OR customer \
        = $1) ORDER BY decided_at, event_id COLLATE \"C\"")
    ~params:
      [ Param.option Type.string (Option.map customer ~f:Ids.Customer_id.to_string) ]
    ~row:decision_row
  |> or_error ~label:"decisions"
;;

let model_row =
  let open Row.Let_syntax in
  let%map_open name = column "name" string
  and version = column "version" string
  and policy = column "policy" string
  and activated_at = column "activated_at" time_ns in
  { Store.Model_version.name; version; policy; activated_at }
;;

let record_model t (m : Store.Model_version.t) =
  Pool.execute
    t.pool
    ~label:"record_model"
    (q
       t.schema
       "INSERT INTO {s}.model_versions (version, name, policy, activated_at) VALUES ($1, \
        $2, $3, $4) ON CONFLICT (version) DO NOTHING")
    ~params:
      [ Param.string m.version
      ; Param.string m.name
      ; Param.string m.policy
      ; Param.time_ns m.activated_at
      ]
  |> Deferred.Result.ignore_m
  |> or_error ~label:"record_model"
;;

let current_model t =
  Pool.fetch_opt
    t.pool
    ~label:"current_model"
    (q
       t.schema
       "SELECT name, version, policy, activated_at FROM {s}.model_versions ORDER BY \
        activated_at DESC, version COLLATE \"C\" DESC LIMIT 1")
    ~row:model_row
  |> or_error ~label:"current_model"
;;

let event_row = body_row Event.Stable.V1.bin_reader_t

let revocation_row =
  let open Row.Let_syntax in
  let%map_open event_id = column "event_id" string
  and reason = column "reason" string
  and revoked_at = column "revoked_at" time_ns in
  { Store.Revocation.event_id = Ids.Event_id.of_string event_id; reason; revoked_at }
;;

let find_event t id =
  let open Deferred.Or_error.Let_syntax in
  let params = [ Param.string (Ids.Event_id.to_string id) ] in
  match%bind
    Pool.fetch_opt
      t.pool
      ~label:"find_event"
      (q t.schema "SELECT body FROM {s}.events WHERE event_id = $1")
      ~params
      ~row:event_row
    |> or_error ~label:"find_event"
  with
  | None -> return None
  | Some event ->
    let%map revocation =
      Pool.fetch_opt
        t.pool
        ~label:"find_revocation"
        (q
           t.schema
           "SELECT event_id, reason, revoked_at FROM {s}.revocations WHERE event_id = $1")
        ~params
        ~row:revocation_row
      |> or_error ~label:"find_revocation"
    in
    Some (event, revocation)
;;

let customer_history t id =
  Pool.fetch_all
    t.pool
    ~label:"customer_history"
    (q
       t.schema
       "SELECT e.body FROM {s}.events e WHERE e.customer = $1 AND NOT EXISTS (SELECT 1 \
        FROM {s}.revocations r WHERE r.event_id = e.event_id) ORDER BY e.seq")
    ~params:[ Param.string (Ids.Customer_id.to_string id) ]
    ~row:event_row
  |> or_error ~label:"customer_history"
;;

let counterparty_history t id =
  Pool.fetch_all
    t.pool
    ~label:"counterparty_history"
    (q
       t.schema
       "SELECT e.body FROM {s}.events e WHERE e.counterparty = $1 AND NOT EXISTS (SELECT \
        1 FROM {s}.revocations r WHERE r.event_id = e.event_id) ORDER BY e.seq")
    ~params:[ Param.string (Ids.Counterparty_id.to_string id) ]
    ~row:event_row
  |> or_error ~label:"counterparty_history"
;;

let revoke t (r : Store.Revocation.t) ~customer ~counterparty =
  Pool.with_transaction t.pool ~f:(fun conn ->
    let open Deferred.Result.Let_syntax in
    let params = [ Param.string (Ids.Event_id.to_string r.event_id) ] in
    match%bind
      Query.fetch_opt
        conn
        ~label:"lock_event"
        (q t.schema "SELECT event_id FROM {s}.events WHERE event_id = $1 FOR UPDATE")
        ~params
        ~row:Row.(column "event_id" Type.string)
    with
    | None -> return Store.Revoke_result.Unknown_event
    | Some _ ->
      (match%bind
         Query.fetch_opt
           conn
           ~label:"find_revocation"
           (q
              t.schema
              "SELECT event_id, reason, revoked_at FROM {s}.revocations WHERE event_id = \
               $1")
           ~params
           ~row:revocation_row
       with
       | Some stored -> return (Store.Revoke_result.Already_revoked stored)
       | None ->
         let%bind (_ : int) =
           Query.execute
             conn
             ~label:"insert_revocation"
             (q
                t.schema
                "INSERT INTO {s}.revocations (event_id, reason, revoked_at) VALUES ($1, \
                 $2, $3)")
             ~params:(params @ [ Param.string r.reason; Param.time_ns r.revoked_at ])
         in
         (* Unconditional: the rebuilt profiles have fewer events than the stored ones. *)
         let c = Profile.Customer.of_snapshot customer in
         let%bind (_ : int) =
           Query.execute
             conn
             ~label:"replace_customer_profile"
             (q
                t.schema
                "INSERT INTO {s}.customer_profiles (customer, events, body) VALUES ($1, \
                 $2, $3) ON CONFLICT (customer) DO UPDATE SET events = EXCLUDED.events, \
                 body = EXCLUDED.body")
             ~params:
               [ Param.string (Ids.Customer_id.to_string (Profile.Customer.customer c))
               ; Param.int (Profile.Customer.events c)
               ; Param.bytea
                   (encode Profile.Customer.Stable.Snapshot.V1.bin_writer_t customer)
               ]
         in
         let cp = Profile.Counterparty.of_snapshot counterparty in
         let%map (_ : int) =
           Query.execute
             conn
             ~label:"replace_counterparty_profile"
             (q
                t.schema
                "INSERT INTO {s}.counterparty_profiles (counterparty, events, body) \
                 VALUES ($1, $2, $3) ON CONFLICT (counterparty) DO UPDATE SET events = \
                 EXCLUDED.events, body = EXCLUDED.body")
             ~params:
               [ Param.string
                   (Ids.Counterparty_id.to_string (Profile.Counterparty.counterparty cp))
               ; Param.int (Profile.Counterparty.events cp)
               ; Param.bytea
                   (encode
                      Profile.Counterparty.Stable.Snapshot.V1.bin_writer_t
                      counterparty)
               ]
         in
         Store.Revoke_result.Revoked))
  |> or_error ~label:"revoke"
;;

let store t =
  Store.T
    ( (module struct
        type nonrec t = t

        let kind = kind
        let find_decision = find_decision
        let commit = commit
        let load_customer = load_customer
        let load_counterparty = load_counterparty
        let find_alert = find_alert
        let update_alert = update_alert
        let alerts = alerts
        let alert_events = alert_events
        let decisions = decisions
        let record_model = record_model
        let current_model = current_model
        let find_event = find_event
        let customer_history = customer_history
        let counterparty_history = counterparty_history
        let revoke = revoke
        let close = close
      end)
    , t )
;;
