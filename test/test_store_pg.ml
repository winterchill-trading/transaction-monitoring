open! Core
open! Async
open Transaction_monitoring

let%expect_test "postgres store conformance" =
  let%bind url = Test_db.fresh_url () in
  let%bind store = Store_pg.connect ~database_url:url () >>| Or_error.ok_exn in
  print_s [%message "migrations" ~_:(Store_pg.applied_migrations store : int list)];
  let%bind () = Store_conformance.run (Store_pg.store store) in
  let%map () = Store_pg.close store in
  [%expect
    {|
    (migrations (1 2 3))
    (commit (event e1) (r Committed))
    (commit (event e2) (r Committed))
    (commit (event e3) (r Committed))
    (commit (event e4) (r Committed))
    ("recommit e3" (outcome hold))
    ("find e2" (outcome (Allow)))
    ("find nope" (found false))
    (decisions ((e1 allow) (e2 allow) (e3 hold) (e4 allow)))
    ("decisions of cust-2" ((e4 allow)))
    ("cust-1 profile events" (3))
    ("after a stale snapshot" (3))
    ("landlord profile events" (2))
    ("unknown customer" (found false))
    (alerts ((alert-e3 open 1)))
    (assign (r Updated))
    ("assign again from the old version" (conflict (2)))
    ("unknown alert" (r (Conflict ())))
    (history
     ((Opened (at "2026-03-02 09:20:00Z"))
      (Assigned (analyst ana) (previous ()) (at "2026-03-02 09:50:00Z"))))
    (open ())
    ("assigned to cust-1" ((alert-e3 assigned 2)))
    (stored (equal true))
    ("no model" ())
    (current
     (((name tm) (version v2) (policy shadow)
       (activated_at "2026-03-02 10:00:00Z"))))
    |}]
;;

(* Rows written before migration 0002 have V1 bodies; a held decision now stores its
   explanation in the body and as queryable JSON. *)
let%expect_test "V1 rows still read; explanations are stored and queryable" =
  let open Pg_async in
  let%bind url = Test_db.fresh_url () in
  let%bind store = Store_pg.connect ~database_url:url () >>| Or_error.ok_exn in
  let pool = Pool.create ~max_size:1 (Config.of_url url |> Or_error.ok_exn) in
  let ok r = Pg_error.to_or_error r |> Or_error.ok_exn in
  let old_event = Fixtures.event ~id:"old" ~at:(Fixtures.at_min 0.) 10_000 in
  let old : Decision.Stable.V1.t =
    { event_id = old_event.id
    ; customer = old_event.customer
    ; outcome = Allow
    ; explanations = []
    ; score = Some 0.01
    ; model = None
    ; threshold_report = false
    ; policy = "default"
    ; decided_at = old_event.received_at
    }
  in
  let%bind () =
    Pool.execute
      pool
      "INSERT INTO transaction_monitoring.events (event_id, customer, occurred_at, body) \
       VALUES ($1, $2, $3, $4)"
      ~params:
        [ Param.string "old"
        ; Param.string (Ids.Customer_id.to_string old_event.customer)
        ; Param.time_ns old_event.occurred_at
        ; Param.bytea (Bin_prot.Writer.to_string Event.Stable.V1.bin_writer_t old_event)
        ]
    >>| ok
    >>| ignore
  in
  let%bind () =
    Pool.execute
      pool
      "INSERT INTO transaction_monitoring.decisions (event_id, customer, outcome, score, \
       decided_at, body) VALUES ($1, $2, 'allow', 0.01, $3, $4)"
      ~params:
        [ Param.string "old"
        ; Param.string (Ids.Customer_id.to_string old_event.customer)
        ; Param.time_ns old_event.received_at
        ; Param.bytea (Bin_prot.Writer.to_string Decision.Stable.V1.bin_writer_t old)
        ]
    >>| ok
    >>| ignore
  in
  let (Store.T ((module S), s)) = Store_pg.store store in
  let%bind found = S.find_decision s old_event.id >>| Or_error.ok_exn in
  print_s
    [%message
      "V1 row"
        ~equal_after_upgrade:
          (Option.equal Decision.equal found (Some (Decision.Stable.V2.of_v1 old)) : bool)];
  let model = Model.load "../models/tm-aml-gbdt/model.json" |> Or_error.ok_exn in
  let evaluator = Evaluator.create ~model Rules.Config.default |> Or_error.ok_exn in
  let%bind monitor =
    Monitor.create ~now:Fixtures.t0 ~store:(Store_pg.store store) evaluator
    >>| Or_error.ok_exn
  in
  let held =
    Fixtures.event
      ~id:"held"
      ~counterparty:"new-payee"
      ~counterparty_country:"IR"
      ~at:(Fixtures.at_min 90.)
      120_000
  in
  let%bind decided =
    match%map Monitor.on_event monitor held >>| Or_error.ok_exn with
    | Decided d -> d
    | _ -> failwith "not decided"
  in
  let%bind alert = S.find_alert s (Alert.id_for held.id) >>| Or_error.ok_exn in
  let alert = Option.value_exn alert in
  let%bind stored = S.find_decision s held.id >>| Or_error.ok_exn in
  let%bind rows =
    Pool.fetch_all
      pool
      "SELECT body_version::bigint AS v, attribution->'top'->0->>'label' AS label, \
       attribution->>'method' AS method FROM transaction_monitoring.alerts"
      ~row:
        Row.Let_syntax.(
          let%map_open v = column "v" int
          and label = column "label" (nullable string)
          and meth = column "method" (nullable string) in
          v, label, meth)
    >>| ok
  in
  print_s
    [%message
      "held"
        ~outcome:(Decision.Outcome.to_string decided.outcome : string)
        ~decision_round_trips:(Option.equal Decision.equal stored (Some decided) : bool)
        ~alert_has_attribution:(Option.is_some alert.attribution : bool)
        ~alert_matches_decision:
          (Option.equal
             Risk_models.Explanation.equal
             alert.attribution
             decided.attribution
           : bool)
        (rows : (int * string option * string option) list)];
  let%bind () = Pool.close pool in
  let%map () = Store_pg.close store in
  [%expect
    {|
    ("V1 row" (equal_after_upgrade true))
    (held (outcome hold) (decision_round_trips true) (alert_has_attribution true)
     (alert_matches_decision true) (rows ((2 (amount) (path_dependent)))))
    |}]
;;

let migrations_tables pool =
  Pg_async.Pool.fetch_all
    pool
    "SELECT table_schema::text AS s FROM information_schema.tables WHERE table_name = \
     'schema_migrations' ORDER BY 1"
    ~row:Pg_async.Row.(column "s" Pg_async.Type.string)
  >>| Pg_async.Pg_error.to_or_error
  >>| Or_error.ok_exn
;;

(* The host application keeps its own [schema_migrations] (here in [public], at version 1
   like ours); the library records its migrations inside its own schema, and a second
   instance under another schema does the same. *)
let%expect_test "migrations are recorded inside the library's schema" =
  let open Pg_async in
  let%bind url = Test_db.fresh_url () in
  let config = Config.of_url url |> Or_error.ok_exn in
  let%bind conn = Connection.connect config >>| Pg_error.ok_exn in
  let%bind host =
    Migrate.apply
      conn
      [ { version = 1; name = "init"; sql = "CREATE TABLE host_accounts (id text)" } ]
    >>| Result.map_error ~f:Migrate.Error.to_error
    >>| Or_error.ok_exn
  in
  let%bind () = Connection.close conn in
  let%bind first = Store_pg.connect ~database_url:url () >>| Or_error.ok_exn in
  let%bind second =
    Store_pg.connect ~database_url:url ~schema:"tm_second" () >>| Or_error.ok_exn
  in
  let%bind again = Store_pg.connect ~database_url:url () >>| Or_error.ok_exn in
  let pool = Pool.create ~max_size:1 config in
  let%bind tables = migrations_tables pool in
  let%bind rejected = Store_pg.connect ~database_url:url ~schema:"Bad-Name" () in
  print_s
    [%message
      ""
        (host : int list)
        ~first:
          ((Store_pg.schema first, Store_pg.applied_migrations first) : string * int list)
        ~second:
          ((Store_pg.schema second, Store_pg.applied_migrations second)
           : string * int list)
        ~again:(Store_pg.applied_migrations again : int list)
        (tables : string list)
        ~bad_name:(Result.is_error rejected : bool)];
  let%bind () = Pool.close pool in
  let%bind () = Store_pg.close first in
  let%bind () = Store_pg.close second in
  let%map () = Store_pg.close again in
  [%expect
    {|
    ((host (1)) (first (transaction_monitoring (1 2 3)))
     (second (tm_second (1 2 3))) (again ())
     (tables (public tm_second transaction_monitoring)) (bad_name true))
    |}]
;;

(* A database set up by 0.1.x: the old migration files applied through pg-async with the
   bookkeeping in the shared table. Connecting adopts them instead of re-running them. *)
let%expect_test "an install from 0.1.x is adopted" =
  let open Pg_async in
  let%bind url = Test_db.fresh_url () in
  let config = Config.of_url url |> Or_error.ok_exn in
  let%bind legacy =
    Migrate.load_dir "fixtures/legacy-migrations"
    >>| Result.map_error ~f:Migrate.Error.to_error
    >>| Or_error.ok_exn
  in
  let%bind conn = Connection.connect config >>| Pg_error.ok_exn in
  let%bind old =
    Migrate.apply conn legacy
    >>| Result.map_error ~f:Migrate.Error.to_error
    >>| Or_error.ok_exn
  in
  (* An event stored by 0.1.x: no counterparty column yet, filled in on connect. *)
  let old_event =
    Fixtures.event ~id:"old" ~counterparty:"old-payee" ~at:Fixtures.t0 10_000
  in
  let%bind (_ : int) =
    Query.execute
      conn
      "INSERT INTO transaction_monitoring.events (event_id, customer, occurred_at, body) \
       VALUES ($1, $2, $3, $4)"
      ~params:
        [ Param.string "old"
        ; Param.string (Ids.Customer_id.to_string old_event.customer)
        ; Param.time_ns old_event.occurred_at
        ; Param.bytea (Bin_prot.Writer.to_string Event.Stable.V1.bin_writer_t old_event)
        ]
    >>| Pg_error.ok_exn
  in
  let%bind () = Connection.close conn in
  let%bind store = Store_pg.connect ~database_url:url () >>| Or_error.ok_exn in
  let (Store.T ((module S), s)) = Store_pg.store store in
  let%bind backfilled =
    S.counterparty_history s old_event.counterparty >>| Or_error.ok_exn
  in
  print_s
    [%message
      "backfilled"
        ~_:(List.map backfilled ~f:(fun (e : Event.t) -> e.id) : Ids.Event_id.t list)];
  let pool = Pool.create ~max_size:1 config in
  let%bind tables = migrations_tables pool in
  let%bind adopted =
    Pool.fetch_all
      pool
      "SELECT version FROM transaction_monitoring.schema_migrations ORDER BY version"
      ~row:Row.(column "version" Type.int)
    >>| Pg_error.to_or_error
    >>| Or_error.ok_exn
  in
  print_s
    [%message
      ""
        (old : int list)
        ~applied_now:(Store_pg.applied_migrations store : int list)
        (adopted : int list)
        (tables : string list)];
  let%bind () = Pool.close pool in
  let%map () = Store_pg.close store in
  [%expect
    {|
    (backfilled (old))
    ((old (1 2)) (applied_now (3)) (adopted (1 2 3))
     (tables (public transaction_monitoring)))
    |}]
;;
