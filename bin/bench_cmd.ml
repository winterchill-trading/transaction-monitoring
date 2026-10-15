open! Core
open Core_bench
open Transaction_monitoring

(* Warm profiles on the first [days - 2] days of a synthetic population and keep the
   events of the last two days as the benchmark inputs, so every measured event meets a
   profile with history. *)
let setup ~customers ~days =
  let config = Rules.Config.default in
  let events = Synthetic.generate { Synthetic.Config.default with customers; days } in
  let cut =
    Time_ns.add
      Synthetic.Config.default.start
      (Time_ns.Span.of_day (Float.of_int (days - 2)))
  in
  let warm, measured =
    List.partition_tf events ~f:(fun l -> Time_ns.(l.event.occurred_at < cut))
  in
  let profiles = Evaluator.Profiles.create config.profile in
  let rules_only = Evaluator.create config |> Or_error.ok_exn in
  List.iter warm ~f:(fun l ->
    ignore (Evaluator.process rules_only profiles l.event : Decision.t));
  profiles, Array.of_list_map measured ~f:(fun l -> l.event)
;;

let percentiles name (spans : float array) =
  Array.sort spans ~compare:Float.compare;
  let at q =
    spans.(Int.min
             (Array.length spans - 1)
             (Float.iround_down_exn (q *. Float.of_int (Array.length spans))))
  in
  printf
    "%-22s p50 %6.2f us  p90 %6.2f us  p99 %6.2f us  max %7.2f us  (%d events)\n"
    name
    (at 0.5)
    (at 0.9)
    (at 0.99)
    spans.(Array.length spans - 1)
    (Array.length spans)
;;

let command =
  Command.basic
    ~summary:"Benchmark per-event latency (core_bench means, then percentiles)"
    (let%map_open.Command model = flag "-model" (optional string) ~doc:"FILE model.json"
     and customers = flag "-customers" (optional_with_default 2_000 int) ~doc:"N (2000)"
     and days = flag "-days" (optional_with_default 30 int) ~doc:"N (30)"
     and quota =
       flag "-quota" (optional_with_default 2. float) ~doc:"SECONDS per test (2)"
     in
     fun () ->
       let config = Rules.Config.default in
       let profiles, events = setup ~customers ~days in
       let n = Array.length events in
       let rules_only = Evaluator.create config |> Or_error.ok_exn in
       let with_model =
         Option.map model ~f:(fun path ->
           Evaluator.create
             ~model:(Model.load path |> Or_error.ok_exn)
             ~policy:{ Policy.default with model_mode = Active }
             config
           |> Or_error.ok_exn)
       in
       let next = ref 0 in
       let event () =
         let e = events.(!next) in
         next := (!next + 1) % n;
         e
       in
       let ctx_of e =
         Evaluator.context
           e
           ~customer:(Evaluator.Profiles.customer profiles e.Event.customer)
           ~counterparty:(Evaluator.Profiles.counterparty profiles e.counterparty)
       in
       let decide_test name evaluator =
         Bench.Test.create ~name (fun () ->
           ignore
             (Sys.opaque_identity (Evaluator.decide evaluator (ctx_of (event ())))
              : Decision.t))
       in
       let tests =
         [ Bench.Test.create ~name:"views (both profiles)" (fun () ->
             ignore (Sys.opaque_identity (ctx_of (event ())) : Context.t))
         ; Bench.Test.create ~name:"features" (fun () ->
             ignore
               (Sys.opaque_identity (Features.encode config (ctx_of (event ())))
                : float array))
         ; decide_test "views + rules" rules_only
         ]
         @ Option.value_map with_model ~default:[] ~f:(fun ev ->
           [ decide_test "views + rules + model" ev ])
       in
       Bench.bench
         ~run_config:
           (Bench.Run_config.create
              ~quota:(Bench.Quota.Span (Time_float.Span.of_sec quota))
              ())
         tests;
       (* Percentiles of one full step (decide and apply) per event, on fresh copies of
          the warmed profiles so each event is applied once, in order. *)
       let measure name evaluator =
         let fresh_profiles, events = setup ~customers ~days in
         let spans =
           Array.map events ~f:(fun e ->
             let t0 = Time_ns.now () in
             ignore
               (Sys.opaque_identity (Evaluator.process evaluator fresh_profiles e)
                : Decision.t);
             Time_ns.Span.to_us (Time_ns.diff (Time_ns.now ()) t0))
         in
         percentiles name spans
       in
       printf
         "\nfull step (decide, then apply), %d customers, last 2 of %d days:\n"
         customers
         days;
       measure "rules" rules_only;
       Option.iter with_model ~f:(measure "rules + model"))
;;
