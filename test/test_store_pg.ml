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
    (migrations (1 2))
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
