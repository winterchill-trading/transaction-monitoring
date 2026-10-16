open! Core

module Side = struct
  type t =
    { decisions : int
    ; allow : int
    ; hold : int
    ; block : int
    ; threshold_reports : int
    ; alerts_by_typology : (string * int) list
    ; labelled : int
    ; labelled_alerted : int
    ; unlabelled_alerted : int
    ; abstained : int
    }
  [@@deriving sexp_of]

  type side = t

  module Acc = struct
    type t =
      { mutable decisions : int
      ; mutable allow : int
      ; mutable hold : int
      ; mutable block : int
      ; mutable threshold_reports : int
      ; by_typology : int String.Table.t
      ; mutable labelled : int
      ; mutable labelled_alerted : int
      ; mutable unlabelled_alerted : int
      ; mutable abstained : int
      }

    let create () =
      { decisions = 0
      ; allow = 0
      ; hold = 0
      ; block = 0
      ; threshold_reports = 0
      ; by_typology = String.Table.create ()
      ; labelled = 0
      ; labelled_alerted = 0
      ; unlabelled_alerted = 0
      ; abstained = 0
      }
    ;;

    let add t (d : Decision.t) ~label =
      t.decisions <- t.decisions + 1;
      if d.model_abstained then t.abstained <- t.abstained + 1;
      if d.threshold_report then t.threshold_reports <- t.threshold_reports + 1;
      let alerted =
        match d.outcome with
        | Allow ->
          t.allow <- t.allow + 1;
          false
        | Hold ->
          t.hold <- t.hold + 1;
          true
        | Block ->
          t.block <- t.block + 1;
          true
      in
      if alerted
      then (
        let names =
          List.filter_map d.explanations ~f:(fun e ->
            match e.severity with
            | Allow -> None
            | Challenge | Review | Block ->
              (match e.typology, e.source with
               | Some ty, _ -> Some (Typology.name ty)
               | None, Model _ when Explanation.is_uncertain e -> Some "model_uncertain"
               | None, Model _ -> Some "model"
               | None, Rule r -> Some r))
          |> List.dedup_and_sort ~compare:String.compare
        in
        List.iter names ~f:(Hashtbl.incr t.by_typology));
      match label with
      | Some (_ : Typology.t) ->
        t.labelled <- t.labelled + 1;
        if alerted then t.labelled_alerted <- t.labelled_alerted + 1
      | None -> if alerted then t.unlabelled_alerted <- t.unlabelled_alerted + 1
    ;;

    let finish t : side =
      { decisions = t.decisions
      ; allow = t.allow
      ; hold = t.hold
      ; block = t.block
      ; threshold_reports = t.threshold_reports
      ; alerts_by_typology =
          Hashtbl.to_alist t.by_typology |> List.sort ~compare:[%compare: string * int]
      ; labelled = t.labelled
      ; labelled_alerted = t.labelled_alerted
      ; unlabelled_alerted = t.unlabelled_alerted
      ; abstained = t.abstained
      }
    ;;
  end
end

module Change = struct
  type t =
    { event_id : Ids.Event_id.t
    ; baseline : Decision.Outcome.t
    ; candidate : Decision.Outcome.t
    ; baseline_score : float option
    ; candidate_score : float option
    ; label : Typology.t option
    ; why : string list
    }
  [@@deriving sexp_of]
end

module Report = struct
  type t =
    { candidate : Side.t
    ; baseline : Side.t option
    ; changed : int
    ; new_alerts : int
    ; cleared_alerts : int
    ; sample_changes : Change.t list
    ; accuracy_coverage : Risk_models.Abstain.Curve.t option
    }
  [@@deriving sexp_of]

  let side_lines name (s : Side.t) =
    [ sprintf
        "%s: %d events, %d allow, %d hold, %d block, %d threshold reports"
        name
        s.decisions
        s.allow
        s.hold
        s.block
        s.threshold_reports
    ; sprintf
        "  alerts by typology: %s%s"
        (List.map s.alerts_by_typology ~f:(fun (k, v) -> sprintf "%s %d" k v)
         |> String.concat ~sep:", ")
        (if s.abstained = 0 then "" else sprintf "; model abstained %d" s.abstained)
    ]
    @
    if s.labelled = 0
    then []
    else (
      let alerts = s.hold + s.block in
      [ sprintf
          "  labelled events %d, alerted %d (recall %.3f); alert precision %.3f"
          s.labelled
          s.labelled_alerted
          (Float.of_int s.labelled_alerted /. Float.of_int s.labelled)
          (if alerts = 0
           then Float.nan
           else Float.of_int s.labelled_alerted /. Float.of_int alerts)
      ])
  ;;

  let to_string_hum t =
    let lines =
      side_lines "candidate" t.candidate
      @ (match t.baseline with
         | None -> []
         | Some b -> side_lines "baseline" b)
      @
      match t.baseline with
      | None -> []
      | Some _ ->
        sprintf
          "changed %d decisions: %d new alerts, %d cleared"
          t.changed
          t.new_alerts
          t.cleared_alerts
        :: List.concat_map t.sample_changes ~f:(fun c ->
          sprintf
            "  %s %s -> %s (score %s -> %s)%s"
            (Ids.Event_id.to_string c.event_id)
            (Decision.Outcome.to_string c.baseline)
            (Decision.Outcome.to_string c.candidate)
            (Option.value_map c.baseline_score ~default:"-" ~f:(sprintf "%.4f"))
            (Option.value_map c.candidate_score ~default:"-" ~f:(sprintf "%.4f"))
            (Option.value_map c.label ~default:"" ~f:(fun l ->
               " label " ^ Typology.name l))
          :: List.map c.why ~f:(fun w -> "    " ^ w))
    in
    let lines =
      lines
      @
      match t.accuracy_coverage with
      | None -> []
      | Some curve ->
        String.split_lines (Risk_models.Abstain.Curve.to_string_hum curve)
        @ [ sprintf
              "  area under the risk-coverage curve %.5f"
              (Risk_models.Abstain.Curve.aurc curve)
          ]
    in
    String.concat ~sep:"\n" lines ^ "\n"
  ;;
end

type t =
  { candidate : Evaluator.t
  ; baseline : Evaluator.t option
  ; profiles : Evaluator.Profiles.t
  ; candidate_acc : Side.Acc.t
  ; baseline_acc : Side.Acc.t
  ; max_changes : int
  ; mutable changed : int
  ; mutable new_alerts : int
  ; mutable cleared_alerts : int
  ; mutable changes : Change.t list (** newest first *)
  ; scored : (float * bool) Queue.t (** candidate scores and labels *)
  }

let create ?baseline ?(max_changes = 20) candidate =
  { candidate
  ; baseline
  ; profiles = Evaluator.Profiles.create (Evaluator.config candidate).profile
  ; candidate_acc = Side.Acc.create ()
  ; baseline_acc = Side.Acc.create ()
  ; max_changes
  ; changed = 0
  ; new_alerts = 0
  ; cleared_alerts = 0
  ; changes = []
  ; scored = Queue.create ()
  }
;;

let alerted (d : Decision.t) =
  match d.outcome with
  | Allow -> false
  | Hold | Block -> true
;;

let step ?label t (event : Event.t) =
  let customer = Evaluator.Profiles.customer t.profiles event.customer in
  let counterparty = Evaluator.Profiles.counterparty t.profiles event.counterparty in
  let ctx = Evaluator.context event ~customer ~counterparty in
  let decision = Evaluator.decide t.candidate ctx in
  Side.Acc.add t.candidate_acc decision ~label;
  Option.iter decision.score ~f:(fun s ->
    Queue.enqueue t.scored (s, Option.is_some label));
  Option.iter t.baseline ~f:(fun b ->
    let before = Evaluator.decide b ctx in
    Side.Acc.add t.baseline_acc before ~label;
    if not (Decision.Outcome.equal before.outcome decision.outcome)
    then (
      t.changed <- t.changed + 1;
      (match alerted before, alerted decision with
       | false, true -> t.new_alerts <- t.new_alerts + 1
       | true, false -> t.cleared_alerts <- t.cleared_alerts + 1
       | _ -> ());
      if t.changed <= t.max_changes
      then
        t.changes
        <- { Change.event_id = event.id
           ; baseline = before.outcome
           ; candidate = decision.outcome
           ; baseline_score = before.score
           ; candidate_score = decision.score
           ; label
           ; why = Decision.why decision
           }
           :: t.changes));
  Evaluator.apply event ~customer ~counterparty;
  decision
;;

let report t : Report.t =
  { candidate = Side.Acc.finish t.candidate_acc
  ; baseline = Option.map t.baseline ~f:(fun _ -> Side.Acc.finish t.baseline_acc)
  ; changed = t.changed
  ; new_alerts = t.new_alerts
  ; cleared_alerts = t.cleared_alerts
  ; sample_changes = List.rev t.changes
  ; accuracy_coverage =
      (* Only with labels (synthetic data, or confirmed cases joined by the caller): what
         the model's decision at its hold threshold gets right at each coverage. *)
      (match Evaluator.hold_at t.candidate with
       | Some h when (not (Queue.is_empty t.scored)) && t.candidate_acc.labelled > 0 ->
         Risk_models.Abstain.Curve.compute
           ~threshold:h
           ~confidence:Logit_margin
           (Queue.to_array t.scored)
         |> Result.ok
       | Some _ | None -> None)
  }
;;
