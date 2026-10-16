open! Core
open! Async
open Transaction_monitoring
open Fixtures

let ok = Or_error.ok_exn
let config = Rules.Config.default
let evaluator () = Evaluator.create config |> ok

(* Events from the shared generator, renumbered and spread over three customers so that
   counterparty profiles see more than one customer. *)
let renumber events =
  List.mapi events ~f:(fun i (e : Event.t) ->
    { e with
      id = Ids.Event_id.of_string (sprintf "e%d" i)
    ; customer = Ids.Customer_id.of_string (sprintf "cust-%d" (i % 3))
    })
;;

(* What a monitor that never saw [revoked] holds: every event but it, in order. *)
let reference events ~revoked =
  let profiles = Evaluator.Profiles.create config.profile in
  let ev = evaluator () in
  let decisions =
    List.filter_map events ~f:(fun (e : Event.t) ->
      if Ids.Event_id.equal e.id revoked
      then None
      else Some (e.id, Evaluator.process ev profiles e))
  in
  profiles, decisions
;;

(* Process [before], revoke one of them, process [after]; then every stored profile and
   every decision taken after the revocation must equal the reference. *)
let check store events ~revoke_at ~cut =
  let (Store.T ((module S), s)) = store in
  let #(before, after) = List.split_n events cut in
  let revoked = (List.nth_exn events revoke_at : Event.t).id in
  let%bind m = Monitor.create ~now:t0 ~store (evaluator ()) >>| ok in
  let%bind () =
    Deferred.List.iter before ~how:`Sequential ~f:(fun e ->
      Monitor.on_event m e >>| ok >>| ignore)
  in
  let%bind reply =
    Monitor.revoke m revoked ~reason:"scheme rejected" ~at:(at_min 10_000.) >>| ok
  in
  let%bind later =
    Deferred.List.map after ~how:`Sequential ~f:(fun (e : Event.t) ->
      match%map Monitor.on_event m e >>| ok with
      | Decided d -> e.id, d
      | r -> raise_s [%message "not decided" (r : Monitor.Reply.t)])
  in
  let profiles, decisions = reference events ~revoked in
  let%bind customers_equal =
    Deferred.List.for_all [ "cust-0"; "cust-1"; "cust-2" ] ~f:(fun c ->
      let id = Ids.Customer_id.of_string c in
      let%map stored = S.load_customer s id >>| ok in
      let expected =
        Profile.Customer.snapshot (Evaluator.Profiles.customer profiles id)
      in
      match stored with
      | Some stored -> Profile.Customer.equal_snapshot stored expected
      | None -> Profile.Customer.events (Profile.Customer.of_snapshot expected) = 0)
  in
  let%map counterparties_equal =
    Deferred.List.for_all
      (List.dedup_and_sort
         (List.map events ~f:(fun (e : Event.t) -> e.counterparty))
         ~compare:Ids.Counterparty_id.compare)
      ~f:(fun id ->
        let%map stored = S.load_counterparty s id >>| ok in
        let expected =
          Profile.Counterparty.snapshot (Evaluator.Profiles.counterparty profiles id)
        in
        match stored with
        | Some stored -> Profile.Counterparty.equal_snapshot stored expected
        | None ->
          Profile.Counterparty.events (Profile.Counterparty.of_snapshot expected) = 0)
  in
  let decisions_equal =
    List.for_all later ~f:(fun (id, d) ->
      match List.Assoc.find decisions id ~equal:Ids.Event_id.equal with
      | Some expected -> Decision.equal d expected
      | None -> false)
  in
  reply, customers_equal && counterparties_equal && decisions_equal
;;

let%expect_test "profiles after a revocation equal profiles that never saw the event" =
  let failures = ref 0
  and trials = 150 in
  let%map () =
    Deferred.List.iter (List.range 0 trials) ~how:`Sequential ~f:(fun seed ->
      let random = Splittable_random.of_int seed in
      let events =
        Quickcheck.Generator.generate stream_generator ~size:40 ~random |> renumber
      in
      let n = List.length events in
      let revoke_at = Splittable_random.int random ~lo:0 ~hi:(n - 1) in
      let cut = Splittable_random.int random ~lo:(revoke_at + 1) ~hi:n in
      let%map reply, equal =
        check (Store_memory.store (Store_memory.create ())) events ~revoke_at ~cut
      in
      match reply with
      | Revoked _ when equal -> ()
      | _ ->
        incr failures;
        print_s
          [%message
            "mismatch"
              (seed : int)
              (revoke_at : int)
              (cut : int)
              (reply : Monitor.Revoke_reply.t)])
  in
  printf "%d trials, %d failures\n" trials !failures;
  [%expect {| 150 trials, 0 failures |}]
;;

let%expect_test "revoke is idempotent, redelivery changes nothing, unknown ids" =
  let store = Store_memory.store (Store_memory.create ()) in
  let (Store.T ((module S), s)) = store in
  let%bind m = Monitor.create ~now:t0 ~store (evaluator ()) >>| ok in
  let pay i ~at amount =
    event ~id:(sprintf "pay-%d" i) ~counterparty:"payee" ~at amount
  in
  let%bind () =
    Deferred.List.iter
      [ pay 1 ~at:(at_min 0.) 950_000
      ; pay 2 ~at:(at_min 60.) 920_000
      ; pay 3 ~at:(at_min 120.) 5_000
      ]
      ~how:`Sequential
      ~f:(fun e -> Monitor.on_event m e >>| ok >>| ignore)
  in
  let id = Ids.Event_id.of_string "pay-2" in
  let%bind first =
    Monitor.revoke m id ~reason:"cancelled by the customer" ~at:(at_min 200.) >>| ok
  in
  let%bind second = Monitor.revoke m id ~reason:"again" ~at:(at_min 201.) >>| ok in
  let%bind unknown =
    Monitor.revoke m (Ids.Event_id.of_string "nope") ~reason:"x" ~at:(at_min 202.) >>| ok
  in
  let%bind redelivered = Monitor.on_event m (pay 2 ~at:(at_min 60.) 920_000) >>| ok in
  let%bind profile = S.load_customer s (Ids.Customer_id.of_string "cust-1") >>| ok in
  let%bind stored = S.find_event s id >>| ok in
  let%bind history = S.customer_history s (Ids.Customer_id.of_string "cust-1") >>| ok in
  (* A fourth payment just under the threshold would be the third in the band within a
     week (pay-1, pay-2, pay-4) and hold for structuring; with pay-2 revoked it is the
     second, and is allowed. *)
  let%map fourth = Monitor.on_event m (pay 4 ~at:(at_min 180.) 900_000) >>| ok in
  print_s
    [%message
      ""
        (first : Monitor.Revoke_reply.t)
        (second : Monitor.Revoke_reply.t)
        (unknown : Monitor.Revoke_reply.t)
        ~redelivered:
          ((match redelivered with
            | Already_decided _ -> "already decided"
            | _ -> "decided again")
           : string)
        ~profile_events:
          (Option.map profile ~f:(fun p ->
             Profile.Customer.events (Profile.Customer.of_snapshot p))
           : int option)
        ~revocation:(Option.bind stored ~f:snd : Store.Revocation.t option)
        ~history:(List.map history ~f:(fun (e : Event.t) -> e.id) : Ids.Event_id.t list)
        ~fourth:
          (Option.value_map (Monitor.Reply.decision fourth) ~default:"none" ~f:(fun d ->
             Decision.Outcome.to_string d.outcome)
           : string)];
  [%expect
    {|
    ((first (Revoked (customer_events 2) (counterparty_events 2)))
     (second
      (Already_revoked
       ((event_id pay-2) (reason "cancelled by the customer")
        (revoked_at "2026-03-02 12:20:00Z"))))
     (unknown Unknown_event) (redelivered "already decided") (profile_events (2))
     (revocation
      (((event_id pay-2) (reason "cancelled by the customer")
        (revoked_at "2026-03-02 12:20:00Z"))))
     (history (pay-1 pay-3)) (fourth allow))
    |}]
;;

(* A profile that saw events the store does not hold (warmed elsewhere) cannot be rebuilt
   exactly; the revocation is refused and nothing changes. *)
let%expect_test "revoking with an incomplete history is refused" =
  let store = Store_memory.store (Store_memory.create ()) in
  let (Store.T ((module S), s)) = store in
  let ev = evaluator () in
  let profiles = Evaluator.Profiles.create config.profile in
  let warm =
    [ event ~id:"old-1" ~at:(at_min 0.) 10_000; event ~id:"old-2" ~at:(at_min 1.) 10_000 ]
  in
  List.iter warm ~f:(fun e -> ignore (Evaluator.process ev profiles e : Decision.t));
  let e = event ~id:"new" ~at:(at_min 2.) 10_000 in
  let decision = Evaluator.process ev profiles e in
  let%bind committed =
    S.commit
      s
      { event = e
      ; decision
      ; alert = None
      ; customer =
          Profile.Customer.snapshot (Evaluator.Profiles.customer profiles e.customer)
      ; counterparty =
          Profile.Counterparty.snapshot
            (Evaluator.Profiles.counterparty profiles e.counterparty)
      }
    >>| ok
  in
  let%bind m = Monitor.create ~now:t0 ~store ev >>| ok in
  let%bind refused = Monitor.revoke m e.id ~reason:"x" ~at:(at_min 3.) in
  let%map stored = S.find_event s e.id >>| ok in
  print_s
    [%message
      ""
        (committed : Store.Commit_result.t)
        (refused : Monitor.Revoke_reply.t Or_error.t)
        ~still_not_revoked:
          (Option.for_all stored ~f:(fun (_, r) -> Option.is_none r) : bool)];
  [%expect
    {|
    ((committed Committed)
     (refused
      (Error
       ("cannot revoke: the store does not hold the whole history of the profile"
        cust-1 (profile_events 3) (stored_events 1))))
     (still_not_revoked true))
    |}]
;;

(* The same property on PostgreSQL, a few streams, then a restarted store still knows the
   revocation. *)
let%expect_test "revocations persist on PostgreSQL" =
  let%bind url = Test_db.fresh_url () in
  let%bind results =
    Deferred.List.map [ 3; 17; 42 ] ~how:`Sequential ~f:(fun seed ->
      let%bind pg =
        Store_pg.connect ~database_url:url ~schema:(sprintf "tm_revoke_%d" seed) () >>| ok
      in
      let random = Splittable_random.of_int seed in
      let events =
        Quickcheck.Generator.generate stream_generator ~size:30 ~random |> renumber
      in
      let n = List.length events in
      let revoke_at = Splittable_random.int random ~lo:0 ~hi:(n - 1) in
      let cut = Splittable_random.int random ~lo:(revoke_at + 1) ~hi:n in
      let%bind reply, equal = check (Store_pg.store pg) events ~revoke_at ~cut in
      let%bind () = Store_pg.close pg in
      let%bind again =
        Store_pg.connect ~database_url:url ~schema:(sprintf "tm_revoke_%d" seed) () >>| ok
      in
      let%bind m =
        Monitor.create ~now:t0 ~store:(Store_pg.store again) (evaluator ()) >>| ok
      in
      let revoked = (List.nth_exn events revoke_at : Event.t).id in
      let%bind repeat =
        Monitor.revoke m revoked ~reason:"again" ~at:(at_min 20_000.) >>| ok
      in
      let%map () = Store_pg.close again in
      let revoked_ok =
        match reply with
        | Revoked _ -> true
        | _ -> false
      in
      let repeat_ok =
        match repeat with
        | Already_revoked r -> String.equal r.reason "scheme rejected"
        | _ -> false
      in
      seed, revoked_ok, equal, repeat_ok)
  in
  print_s [%sexp (results : (int * bool * bool * bool) list)];
  [%expect {| ((3 true true true) (17 true true true) (42 true true true)) |}];
  return ()
;;
