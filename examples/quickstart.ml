(* How an application wires the monitor into payment initiation and inbound booking, and
   surfaces holds to a review queue. The store is in memory here; a service passes
   [Store_pg.store] instead. *)

open! Core
open! Async
open Transaction_monitoring

let eur cents = Money.Amount.of_minor_units_int Money.Currency.eur cents

let at minutes =
  Time_ns.add
    (Time_ns.of_string_with_utc_offset "2026-03-02T09:00:00Z")
    (Time_ns.Span.of_min minutes)
;;

(* The application's payment, reduced to what monitoring needs. *)
let event ~id ~direction ~counterparty ?country ~minutes cents : Event.t =
  { id = Ids.Event_id.of_string id
  ; customer = Ids.Customer_id.of_string "cust-7"
  ; account = Ledger.Account_id.of_string "customer:7/eur"
  ; counterparty = Ids.Counterparty_id.of_string counterparty
  ; counterparty_country = Option.map country ~f:Risk_rules.Country.of_string
  ; rail = Sepa_instant
  ; direction
  ; amount = eur cents
  ; base_amount = eur cents
  ; occurred_at = at minutes
  ; received_at = at minutes
  ; mcc = None
  ; card_channel = None
  ; declined = false
  ; device_id = Some "dev-1"
  ; ip_country = Some (Risk_rules.Country.of_string "DE")
  ; residence = Some (Risk_rules.Country.of_string "DE")
  ; account_opened_at = None
  ; risk_band = None
  }
;;

(* Outgoing: decide before the payment leaves. Hold parks it (the customer sees "under
   review", never the reason); Overloaded fails closed. *)
let initiate monitor e =
  match%map Monitor.on_event monitor e with
  | Error err ->
    printf
      "%s: monitoring failed, held: %s\n"
      (Ids.Event_id.to_string e.Event.id)
      (Error.to_string_hum err)
  | Ok Overloaded ->
    printf "%s: monitor busy, held for retry\n" (Ids.Event_id.to_string e.id)
  | Ok (Decided d | Already_decided d) ->
    (match d.outcome with
     | Allow -> printf "%s: submitted to the scheme\n" (Ids.Event_id.to_string e.id)
     | Hold | Block ->
       printf
         "%s: %s, customer sees %S\n"
         (Ids.Event_id.to_string e.id)
         (Decision.Outcome.to_string d.outcome)
         (Option.value (Decision.customer_status d) ~default:""))
;;

(* Incoming: the money has arrived and is booked either way; a hold freezes it (a ledger
   hold on the customer's account) until an investigator decides. *)
let book_inbound monitor e =
  match%map Monitor.on_event monitor e with
  | Ok (Decided { outcome = Hold | Block; _ }) ->
    printf "%s: booked and frozen pending review\n" (Ids.Event_id.to_string e.Event.id)
  | Ok _ | Error _ -> printf "%s: booked\n" (Ids.Event_id.to_string e.id)
;;

let main () =
  let model = Model.bundled () |> ok_exn in
  let evaluator = Evaluator.create ~model Rules.Config.default |> ok_exn in
  let%bind monitor =
    Monitor.create
      ~now:(at 0.)
      ~store:(Store_memory.store (Store_memory.create ()))
      evaluator
    >>| ok_exn
  in
  let%bind () =
    book_inbound
      monitor
      (event ~id:"in-1" ~direction:Inbound ~counterparty:"employer" ~minutes:0. 310_000)
  in
  let%bind () =
    initiate
      monitor
      (event ~id:"out-1" ~direction:Outbound ~counterparty:"landlord" ~minutes:60. 95_000)
  in
  let%bind () =
    initiate
      monitor
      (event
         ~id:"out-2"
         ~direction:Outbound
         ~counterparty:"new-payee"
         ~country:"IR"
         ~minutes:90.
         120_000)
  in
  (* a retried request is answered from the store *)
  let%bind () =
    initiate
      monitor
      (event
         ~id:"out-2"
         ~direction:Outbound
         ~counterparty:"new-payee"
         ~country:"IR"
         ~minutes:90.
         120_000)
  in
  (* the review queue an investigator works through *)
  let%bind queue = Monitor.review_queue monitor >>| ok_exn in
  List.iter queue ~f:(fun (a : Alert.t) ->
    printf "review %s:\n" (Ids.Alert_id.to_string a.id);
    List.iter a.explanations ~f:(fun e ->
      printf "  %s\n    %s\n" (Explanation.to_string_hum e) e.reference);
    (* The model's exact feature contributions to this score, under reviewer-facing names:
       what the model leaned on, not proof of what happened. *)
    Option.iter a.attribution ~f:(fun x ->
      printf "  model contributions to the log-odds (base %.2f):\n" x.base;
      List.iter x.top ~f:(fun c -> printf "    %-42s %+.2f\n" c.label c.contribution);
      printf "    %-42s %+.2f\n" "all other features" x.rest));
  let alert = (List.hd_exn queue).id in
  let analyst = Ids.Analyst_id.of_string "analyst-1" in
  let%bind _ =
    Monitor.apply_alert monitor alert (Assign { analyst; at = at 100. }) >>| ok_exn
  in
  let%map closed, _ =
    Monitor.apply_alert
      monitor
      alert
      (Close
         { by = analyst
         ; verdict = False_positive
         ; note = "tuition fee, invoice on file"
         ; at = at 120.
         })
    >>| ok_exn
  in
  printf "alert %s, release the payment\n" (Alert.Status.to_string closed.status)
;;

let () =
  Thread_safe.block_on_async_exn (fun () ->
    (* Async's printf is buffered; flush before the program exits *)
    let%bind () = main () in
    Writer.flushed (force Writer.stdout))
;;
