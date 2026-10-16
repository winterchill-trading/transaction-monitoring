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
