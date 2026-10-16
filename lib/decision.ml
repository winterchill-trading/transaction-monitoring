open! Core

module Outcome = struct
  module Stable = struct
    module V1 = struct
      type t =
        | Allow
        | Hold
        | Block
      [@@deriving bin_io, sexp, compare, equal, hash, enumerate]
    end
  end

  include Stable.V1

  let to_string = function
    | Allow -> "allow"
    | Hold -> "hold"
    | Block -> "block"
  ;;

  let of_string s =
    match String.lowercase s with
    | "allow" -> Ok Allow
    | "hold" -> Ok Hold
    | "block" -> Ok Block
    | _ -> Or_error.error_s [%message "unknown outcome" s]
  ;;

  let worst a b = if compare a b >= 0 then a else b

  let of_severity : Risk_rules.Severity.t -> t = function
    | Allow -> Allow
    | Challenge | Review -> Hold
    | Block -> Block
  ;;
end

module Model_ref = struct
  module Stable = struct
    module V1 = struct
      type t =
        { name : string
        ; version : string
        ; shadow : bool
        }
      [@@deriving bin_io, sexp, compare, equal, hash]
    end
  end

  include Stable.V1
end

module Stable = struct
  module V1 = struct
    type t =
      { event_id : Ids.Event_id.Stable.V1.t
      ; customer : Ids.Customer_id.Stable.V1.t
      ; outcome : Outcome.Stable.V1.t
      ; explanations : Explanation.Stable.V1.t list
      ; score : float option
      ; model : Model_ref.Stable.V1.t option
      ; threshold_report : bool
      ; policy : string
      ; decided_at : Time_ns.Stable.Alternate_sexp.V1.t
      }
    [@@deriving bin_io, sexp, compare, equal]
  end

  module V2 = struct
    type t =
      { event_id : Ids.Event_id.Stable.V1.t
      ; customer : Ids.Customer_id.Stable.V1.t
      ; outcome : Outcome.Stable.V1.t
      ; explanations : Explanation.Stable.V1.t list
      ; score : float option
      ; model : Model_ref.Stable.V1.t option
      ; threshold_report : bool
      ; policy : string
      ; decided_at : Time_ns.Stable.Alternate_sexp.V1.t
      ; attribution : Risk_models.Explanation.Stable.V1.t option
      ; model_abstained : bool
      }
    [@@deriving bin_io, sexp, compare, equal]

    let of_v1 (v1 : V1.t) =
      { event_id = v1.event_id
      ; customer = v1.customer
      ; outcome = v1.outcome
      ; explanations = v1.explanations
      ; score = v1.score
      ; model = v1.model
      ; threshold_report = v1.threshold_report
      ; policy = v1.policy
      ; decided_at = v1.decided_at
      ; attribution = None
      ; model_abstained = false
      }
    ;;
  end
end

include Stable.V2

let create (policy : Policy.t) ~(event : Event.t) ~(rules : Risk_rules.Decision.t) ~model =
  let open Or_error.Let_syntax in
  let rule_outcome = Outcome.of_severity rules.severity in
  let%map model_outcome, model_explanation, score, model_ref, model_abstained =
    match model, policy.model_mode with
    | None, _ | Some _, Off -> return (Outcome.Allow, None, None, None, false)
    | Some (m, score, reasons), ((Shadow | Active) as mode) ->
      let%map hold_at, block_at = Policy.thresholds policy m in
      let reached = Option.exists ~f:(fun th -> Float.(score >= th)) in
      let raw : Outcome.t =
        if reached block_at then Block else if reached hold_at then Hold else Allow
      in
      (* Abstention only concerns the hold threshold: a score that reaches the block
         threshold is far from it and stands. *)
      let abstainer =
        match hold_at with
        | Some h when not (reached block_at) ->
          Option.bind (Policy.abstainer policy ~hold_at:h) ~f:(fun a ->
            match Risk_models.Abstain.decide a score with
            | Abstain _ -> Some a
            | Predict _ -> None)
        | Some _ | None -> None
      in
      let raw : Outcome.t =
        match abstainer, policy.abstain with
        | None, _ | Some _, None -> raw
        | Some _, Some { route = Hold; _ } -> Hold
        | Some _, Some { route = Rules_only; _ } -> Allow
      in
      let shadow = Policy.Model_mode.equal mode Shadow in
      let effective = if shadow then Outcome.Allow else raw in
      let severity : Risk_rules.Severity.t =
        match effective with
        | Allow -> Allow
        | Hold -> Review
        | Block -> Block
      in
      let explanation =
        match abstainer, policy.abstain with
        | Some a, Some { route; _ } ->
          Some
            (Explanation.of_uncertain
               ~name:(Model.name m)
               ~version:(Model.version m)
               ~severity
               ~score
               ~band:(Risk_models.Abstain.band a)
               ~threshold:a.threshold
               ~held:
                 (match route with
                  | Hold -> true
                  | Rules_only -> false))
        | _ ->
          (match raw with
           | Allow -> None
           | Hold | Block ->
             Some
               (Explanation.of_score
                  ~name:(Model.name m)
                  ~version:(Model.version m)
                  ~severity
                  ~score
                  ~reasons))
      in
      ( effective
      , explanation
      , Some score
      , Some { Model_ref.name = Model.name m; version = Model.version m; shadow }
      , Option.is_some abstainer )
  in
  let explanations =
    List.map rules.hits ~f:Explanation.of_hit @ Option.to_list model_explanation
    |> List.stable_sort ~compare:(fun (a : Explanation.t) b ->
      Risk_rules.Severity.compare b.severity a.severity)
  in
  { event_id = event.id
  ; customer = event.customer
  ; outcome = Outcome.worst rule_outcome model_outcome
  ; explanations
  ; score
  ; model = model_ref
  ; threshold_report = Rules.requires_threshold_report rules
  ; policy = policy.name
  ; decided_at = event.received_at
  ; attribution = None
  ; model_abstained
  }
;;

let with_attribution t attribution = { t with attribution = Some attribution }

let why t =
  List.filter_map t.explanations ~f:(fun (e : Explanation.t) ->
    match e.severity with
    | Allow -> None
    | Challenge | Review | Block -> Some (Explanation.to_string_hum e))
  @
  match t.attribution with
  | None -> []
  | Some a ->
    [ "model contributions (log-odds, not causes): "
      ^ String.concat
          ~sep:", "
          (List.map a.top ~f:(fun c -> sprintf "%s %+.2f" c.label c.contribution))
    ]
;;

let typologies t =
  List.filter_map t.explanations ~f:(fun e -> e.typology)
  |> List.dedup_and_sort ~compare:Typology.compare
;;

let customer_status t =
  match t.outcome with
  | Allow -> None
  | Hold -> Some "under review"
  | Block -> Some "this payment could not be completed"
;;
