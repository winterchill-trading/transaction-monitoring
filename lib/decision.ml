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
end

include Stable.V1

let create (policy : Policy.t) ~(event : Event.t) ~(rules : Risk_rules.Decision.t) ~model =
  let open Or_error.Let_syntax in
  let rule_outcome = Outcome.of_severity rules.severity in
  let%map model_outcome, model_explanation, score, model_ref =
    match model, policy.model_mode with
    | None, _ | Some _, Off -> return (Outcome.Allow, None, None, None)
    | Some (m, score, reasons), ((Shadow | Active) as mode) ->
      let%map hold_at, block_at = Policy.thresholds policy m in
      let reached = Option.exists ~f:(fun th -> Float.(score >= th)) in
      let raw : Outcome.t =
        if reached block_at then Block else if reached hold_at then Hold else Allow
      in
      let shadow = Policy.Model_mode.equal mode Shadow in
      let effective = if shadow then Outcome.Allow else raw in
      let explanation =
        match raw with
        | Allow -> None
        | Hold | Block ->
          let severity : Risk_rules.Severity.t =
            match effective with
            | Allow -> Allow
            | Hold -> Review
            | Block -> Block
          in
          Some
            (Explanation.of_score
               ~name:(Model.name m)
               ~version:(Model.version m)
               ~severity
               ~score
               ~reasons)
      in
      ( effective
      , explanation
      , Some score
      , Some { Model_ref.name = Model.name m; version = Model.version m; shadow } )
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
  }
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
