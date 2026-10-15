open! Core
open! Async
open Pg_async

type t =
  { pool : Pool.t
  ; description : string
  ; applied_migrations : int list
  }

let kind t = t.description
let applied_migrations t = t.applied_migrations

let or_error ~label d =
  d >>| fun r -> Pg_error.to_or_error r |> Or_error.tag ~tag:("store: " ^ label)
;;

let migrations =
  List.map Migrations_sql.all ~f:(fun (version, name, sql) ->
    { Migrate.Migration.version; name; sql })
;;

let connect ?database_url ?(max_connections = 4) () =
  let open Deferred.Or_error.Let_syntax in
  let%bind config =
    Deferred.return
      (match database_url with
       | Some url -> Config.of_url url
       | None -> Config.of_env ())
  in
  let config = { config with application_name = "transaction-monitoring" } in
  let%bind conn = or_error ~label:"connect" (Connection.connect config) in
  let%bind.Deferred applied = Migrate.apply conn migrations in
  let%bind.Deferred () = Connection.close conn in
  let%map applied_migrations =
    Deferred.return (Result.map_error applied ~f:Migrate.Error.to_error)
  in
  { pool = Pool.create ~max_size:max_connections config
  ; description = "postgres " ^ Config.to_string_hum config
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

let decision_row = body_row Decision.Stable.V1.bin_reader_t
let alert_row = body_row Alert.Stable.V1.bin_reader_t
let alert_event_row = body_row Alert.Event.Stable.V1.bin_reader_t
let customer_row = body_row Profile.Customer.Stable.Snapshot.V1.bin_reader_t
let counterparty_row = body_row Profile.Counterparty.Stable.Snapshot.V1.bin_reader_t

let find_decision t id =
  Pool.fetch_opt
    t.pool
    ~label:"find_decision"
    "SELECT body FROM transaction_monitoring.decisions WHERE event_id = $1"
    ~params:[ Param.string (Ids.Event_id.to_string id) ]
    ~row:decision_row
  |> or_error ~label:"find_decision"
;;

let insert_alert_events conn (alert : Alert.t) events =
  let first = alert.version - List.length events + 1 in
  Deferred.Result.List.iteri events ~f:(fun i e ->
    Query.execute
      conn
      ~label:"insert_alert_event"
      "INSERT INTO transaction_monitoring.alert_events (alert_id, seq, body) VALUES ($1, \
       $2, $3)"
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
  ; Param.bytea (encode Alert.Stable.V1.bin_writer_t a)
  ]
;;

let commit t (c : Store.Commit.t) =
  Pool.with_transaction t.pool ~f:(fun conn ->
    let open Deferred.Result.Let_syntax in
    let%bind inserted =
      Query.execute
        conn
        ~label:"insert_event"
        "INSERT INTO transaction_monitoring.events (event_id, customer, occurred_at, \
         body) VALUES ($1, $2, $3, $4) ON CONFLICT (event_id) DO NOTHING"
        ~params:
          [ Param.string (Ids.Event_id.to_string c.event.id)
          ; Param.string (Ids.Customer_id.to_string c.event.customer)
          ; Param.time_ns c.event.occurred_at
          ; Param.bytea (encode Event.Stable.V1.bin_writer_t c.event)
          ]
    in
    if inserted = 0
    then
      Query.fetch_one
        conn
        ~label:"duplicate_decision"
        "SELECT body FROM transaction_monitoring.decisions WHERE event_id = $1"
        ~params:[ Param.string (Ids.Event_id.to_string c.event.id) ]
        ~row:decision_row
      >>| fun d -> Store.Commit_result.Duplicate d
    else (
      let d = c.decision in
      let%bind (_ : int) =
        Query.execute
          conn
          ~label:"insert_decision"
          "INSERT INTO transaction_monitoring.decisions (event_id, customer, outcome, \
           score, model_version, decided_at, body) VALUES ($1, $2, $3, $4, $5, $6, $7)"
          ~params:
            [ Param.string (Ids.Event_id.to_string d.event_id)
            ; Param.string (Ids.Customer_id.to_string d.customer)
            ; Param.string (Decision.Outcome.to_string d.outcome)
            ; Param.option Type.float d.score
            ; Param.option Type.string (Option.map d.model ~f:(fun m -> m.version))
            ; Param.time_ns d.decided_at
            ; Param.bytea (encode Decision.Stable.V1.bin_writer_t d)
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
              "INSERT INTO transaction_monitoring.alerts (alert_id, event_id, customer, \
               status, sar_candidate, version, opened_at, updated_at, body) VALUES ($1, \
               $2, $3, $4, $5, $6, $7, $8, $9) ON CONFLICT (alert_id) DO NOTHING"
              ~params:(alert_params alert)
          in
          insert_alert_events conn alert events
      in
      let customer = Profile.Customer.of_snapshot c.customer in
      let%bind (_ : int) =
        Query.execute
          conn
          ~label:"upsert_customer_profile"
          "INSERT INTO transaction_monitoring.customer_profiles (customer, events, body) \
           VALUES ($1, $2, $3) ON CONFLICT (customer) DO UPDATE SET events = \
           EXCLUDED.events, body = EXCLUDED.body WHERE \
           transaction_monitoring.customer_profiles.events < EXCLUDED.events"
          ~params:
            [ Param.string (Ids.Customer_id.to_string (Profile.Customer.customer customer))
            ; Param.int (Profile.Customer.events customer)
            ; Param.bytea (encode Profile.Customer.Stable.Snapshot.V1.bin_writer_t c.customer)
            ]
      in
      let cp = Profile.Counterparty.of_snapshot c.counterparty in
      let%map (_ : int) =
        Query.execute
          conn
          ~label:"upsert_counterparty_profile"
          "INSERT INTO transaction_monitoring.counterparty_profiles (counterparty, \
           events, body) VALUES ($1, $2, $3) ON CONFLICT (counterparty) DO UPDATE SET \
           events = EXCLUDED.events, body = EXCLUDED.body WHERE \
           transaction_monitoring.counterparty_profiles.events < EXCLUDED.events"
          ~params:
            [ Param.string
                (Ids.Counterparty_id.to_string (Profile.Counterparty.counterparty cp))
            ; Param.int (Profile.Counterparty.events cp)
            ; Param.bytea
                (encode Profile.Counterparty.Stable.Snapshot.V1.bin_writer_t c.counterparty)
            ]
      in
      Store.Commit_result.Committed))
  |> or_error ~label:"commit"
;;

let load_customer t id =
  Pool.fetch_opt
    t.pool
    ~label:"load_customer"
    "SELECT body FROM transaction_monitoring.customer_profiles WHERE customer = $1"
    ~params:[ Param.string (Ids.Customer_id.to_string id) ]
    ~row:customer_row
  |> or_error ~label:"load_customer"
;;

let load_counterparty t id =
  Pool.fetch_opt
    t.pool
    ~label:"load_counterparty"
    "SELECT body FROM transaction_monitoring.counterparty_profiles WHERE counterparty = \
     $1"
    ~params:[ Param.string (Ids.Counterparty_id.to_string id) ]
    ~row:counterparty_row
  |> or_error ~label:"load_counterparty"
;;

let find_alert t id =
  Pool.fetch_opt
    t.pool
    ~label:"find_alert"
    "SELECT body FROM transaction_monitoring.alerts WHERE alert_id = $1"
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
        "UPDATE transaction_monitoring.alerts SET status = $4, sar_candidate = $5, \
         version = $6, updated_at = $8, body = $9 WHERE alert_id = $1 AND event_id = $2 \
         AND customer = $3 AND opened_at = $7 AND version = $10"
        ~params:(alert_params alert @ [ Param.int expected ])
    in
    if updated = 1
    then insert_alert_events conn alert events >>| fun () -> Store.Alert_update.Updated
    else
      Query.fetch_opt
        conn
        ~label:"alert_conflict"
        "SELECT body FROM transaction_monitoring.alerts WHERE alert_id = $1"
        ~params:[ Param.string (Ids.Alert_id.to_string alert.id) ]
        ~row:alert_row
      >>| fun stored -> Store.Alert_update.Conflict stored)
  |> or_error ~label:"update_alert"
;;

let alerts ?status ?customer t =
  Pool.fetch_all
    t.pool
    ~label:"alerts"
    "SELECT body FROM transaction_monitoring.alerts WHERE ($1::text IS NULL OR status = \
     $1) AND ($2::text IS NULL OR customer = $2) ORDER BY opened_at, alert_id COLLATE \"C\""
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
    "SELECT body FROM transaction_monitoring.alert_events WHERE alert_id = $1 ORDER BY seq"
    ~params:[ Param.string (Ids.Alert_id.to_string id) ]
    ~row:alert_event_row
  |> or_error ~label:"alert_events"
;;

let decisions ?customer t =
  Pool.fetch_all
    t.pool
    ~label:"decisions"
    "SELECT body FROM transaction_monitoring.decisions WHERE ($1::text IS NULL OR \
     customer = $1) ORDER BY decided_at, event_id COLLATE \"C\""
    ~params:[ Param.option Type.string (Option.map customer ~f:Ids.Customer_id.to_string) ]
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
    "INSERT INTO transaction_monitoring.model_versions (version, name, policy, \
     activated_at) VALUES ($1, $2, $3, $4) ON CONFLICT (version) DO NOTHING"
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
    "SELECT name, version, policy, activated_at FROM \
     transaction_monitoring.model_versions ORDER BY activated_at DESC, version COLLATE \
     \"C\" DESC LIMIT 1"
    ~row:model_row
  |> or_error ~label:"current_model"
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
        let close = close
      end)
    , t )
;;
