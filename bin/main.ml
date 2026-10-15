open! Core
open Transaction_monitoring

let config_flag =
  let open Command.Param in
  flag
    "-config"
    (optional string)
    ~doc:"FILE rules config sexp (default: built-in defaults)"
  |> map ~f:(function
    | None -> Rules.Config.default
    | Some path -> Rules.Config.load path |> Or_error.ok_exn)
;;

let policy_flag =
  let open Command.Param in
  flag "-policy" (optional string) ~doc:"FILE policy sexp (default: model in shadow mode)"
  |> map ~f:(function
    | None -> Policy.default
    | Some path -> Policy.load path |> Or_error.ok_exn)
;;

let load_model path = Model.load path |> Or_error.ok_exn

(* Events from a JSON lines file; a ["label"] field, when present, is kept. *)
let read_labelled path =
  In_channel.with_file path ~f:(fun ic ->
    In_channel.input_lines ic
    |> List.filter ~f:(fun l -> not (String.is_empty (String.strip l)))
    |> List.mapi ~f:(fun i line ->
      Synthetic.of_json_line line
      |> Or_error.tag_s ~tag:[%message "reading events" path ~line:(i + 1 : int)]
      |> Or_error.ok_exn))
;;

let generate =
  Command.basic
    ~summary:"Write synthetic labelled events as JSON lines"
    (let d = Synthetic.Config.default in
     let%map_open.Command customers =
       flag
         "-customers"
         (optional_with_default d.customers int)
         ~doc:(sprintf "N customers (%d)" d.customers)
     and days =
       flag "-days" (optional_with_default d.days int) ~doc:(sprintf "N days (%d)" d.days)
     and seed =
       flag "-seed" (optional_with_default d.seed int) ~doc:(sprintf "N seed (%d)" d.seed)
     and bad_share =
       flag
         "-bad-share"
         (optional_with_default d.bad_share float)
         ~doc:(sprintf "P share of customers with a typology episode (%g)" d.bad_share)
     and out = flag "-out" (required string) ~doc:"FILE output (JSON lines)" in
     fun () ->
       let config = { Synthetic.Config.default with customers; days; seed; bad_share } in
       let events = Synthetic.generate config in
       Out_channel.with_file out ~f:(fun oc ->
         List.iter events ~f:(fun l ->
           Out_channel.output_string oc (Synthetic.to_json_line l);
           Out_channel.newline oc));
       let labelled = List.count events ~f:(fun l -> Option.is_some l.label) in
       eprintf "%d events, %d labelled, written to %s\n" (List.length events) labelled out)
;;

let features =
  Command.basic
    ~summary:"Export one feature row per event as CSV, for model training"
    ~readme:(fun () ->
      "Columns: event_id, customer, day (days since 1970-01-01 UTC, fractional), label \
       (typology or empty), rules (the outcome of the rules alone), rule_hits (typologies, \
       semicolon-separated), then every feature of the schema in order. Features are \
       computed by the same code that scores live events, from profiles built over the \
       file in order.")
    (let%map_open.Command events = flag "-events" (required string) ~doc:"FILE JSON lines"
     and out = flag "-out" (required string) ~doc:"FILE CSV output"
     and config = config_flag in
     fun () ->
       let evaluator = Evaluator.create config |> Or_error.ok_exn in
       let profiles = Evaluator.Profiles.create config.profile in
       let labelled = read_labelled events in
       let x = Array.create ~len:(Risk_models.Schema.length Features.schema) 0. in
       let float_text v = if Float.is_nan v then "" else sprintf "%.17g" v in
       Out_channel.with_file out ~f:(fun oc ->
         Out_channel.output_string
           oc
           (String.concat
              ~sep:","
              ([ "event_id"; "customer"; "day"; "label"; "rules"; "rule_hits" ]
               @ Array.to_list Features.schema.features));
         Out_channel.newline oc;
         List.iter labelled ~f:(fun { event; label } ->
           let customer = Evaluator.Profiles.customer profiles event.customer in
           let counterparty = Evaluator.Profiles.counterparty profiles event.counterparty in
           let ctx = Evaluator.context event ~customer ~counterparty in
           let decision = Evaluator.decide evaluator ctx in
           Features.encode_into config ctx x;
           Evaluator.apply event ~customer ~counterparty;
           let hits =
             List.filter_map decision.explanations ~f:(fun e ->
               match e.severity, e.typology with
               | (Challenge | Review | Block), Some t -> Some (Typology.name t)
               | _ -> None)
           in
           Out_channel.output_string
             oc
             (String.concat
                ~sep:","
                ([ Ids.Event_id.to_string event.id
                 ; Ids.Customer_id.to_string event.customer
                 ; sprintf
                     "%.6f"
                     (Time_ns.Span.to_day (Time_ns.to_span_since_epoch event.occurred_at))
                 ; Option.value_map label ~default:"" ~f:Typology.name
                 ; Decision.Outcome.to_string decision.outcome
                 ; String.concat ~sep:";" hits
                 ]
                 @ Array.to_list (Array.map x ~f:float_text)));
           Out_channel.newline oc));
       eprintf "%d rows written to %s\n" (List.length labelled) out)
;;

let replay =
  Command.basic
    ~summary:"Re-run a history of events through a model and report the decisions"
    ~readme:(fun () ->
      "Profiles start empty and are built from the file in order. With -baseline-model, \
       every event is also decided with the baseline and the report lists what changed. \
       Labels in the file (synthetic data) are used for recall and precision. \
       -decisions-out writes every candidate decision as a sexp line.")
    (let%map_open.Command events = flag "-events" (required string) ~doc:"FILE JSON lines"
     and model = flag "-model" (optional string) ~doc:"FILE candidate model.json"
     and baseline_model =
       flag "-baseline-model" (optional string) ~doc:"FILE baseline model.json"
     and baseline_policy =
       flag "-baseline-policy" (optional string) ~doc:"FILE baseline policy sexp"
     and policy = policy_flag
     and config = config_flag
     and decisions_out =
       flag "-decisions-out" (optional string) ~doc:"FILE write decisions (sexp lines)"
     and max_changes =
       flag "-show-changes" (optional_with_default 20 int) ~doc:"N changes to list (20)"
     in
     fun () ->
       let candidate =
         Evaluator.create ?model:(Option.map model ~f:load_model) ~policy config
         |> Or_error.ok_exn
       in
       let baseline =
         match baseline_model, baseline_policy with
         | None, None -> None
         | _ ->
           Some
             (Evaluator.create
                ?model:(Option.map baseline_model ~f:load_model)
                ~policy:
                  (Option.value_map baseline_policy ~default:policy ~f:(fun p ->
                     Policy.load p |> Or_error.ok_exn))
                config
              |> Or_error.ok_exn)
       in
       let replay = Replay.create ?baseline ~max_changes candidate in
       let out = Option.map decisions_out ~f:Out_channel.create in
       let labelled = read_labelled events in
       let start = Time_ns.now () in
       List.iter labelled ~f:(fun { event; label } ->
         let d = Replay.step ?label replay event in
         Option.iter out ~f:(fun oc ->
           Out_channel.output_string oc (Sexp.to_string [%sexp (d : Decision.t)]);
           Out_channel.newline oc));
       let elapsed = Time_ns.diff (Time_ns.now ()) start in
       Option.iter out ~f:Out_channel.close;
       print_string (Replay.Report.to_string_hum (Replay.report replay));
       printf
         "%d events in %s (%.1f us per event, %s)\n"
         (List.length labelled)
         (Time_ns.Span.to_string_hum elapsed)
         (Time_ns.Span.to_us elapsed /. Float.of_int (Int.max 1 (List.length labelled)))
         (match baseline with
          | None -> "candidate only"
          | Some _ -> "candidate and baseline"))
;;

let typologies =
  Command.basic
    ~summary:"List the typologies with their codes and references"
    (Command.Param.return (fun () ->
       List.iter Typology.all ~f:(fun t ->
         printf
           "%-21s %-28s %s\n%21s %s\n"
           (Typology.name t)
           (Risk_rules.Reason_code.to_string (Typology.code t))
           (Typology.description t)
           ""
           (Typology.reference t))))
;;

let check_model =
  Command.basic
    ~summary:"Load a model and check it against the feature schema"
    (let%map_open.Command path = anon ("MODEL" %: string) in
     fun () ->
       let m = load_model path in
       let th = Model.thresholds m in
       printf
         "%s version %s, %d features (schema digest %s)\n"
         (Model.name m)
         (Model.version m)
         (Risk_models.Schema.length Features.schema)
         Features.schema.digest;
       Option.iter th ~f:(fun th ->
         printf
           "review at %.4f (budget %d alerts a day), block at %s\n"
           th.review
           th.alerts_per_day
           (Option.value_map th.block ~default:"none" ~f:(sprintf "%.4f"))))
;;

let () =
  Command_unix.run
    (Command.group
       ~summary:"Real-time AML transaction monitoring: profiles, typology rules, scores"
       [ "generate", generate
       ; "features", features
       ; "replay", replay
       ; "bench", Bench_cmd.command
       ; "typologies", typologies
       ; "check-model", check_model
       ])
;;
