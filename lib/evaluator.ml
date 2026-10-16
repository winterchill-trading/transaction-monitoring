open! Core

type t =
  { config : Rules.Config.t
  ; policy : Policy.t
  ; model : Model.t option
  ; hold_at : float option
  ; explain_from : float option
  ; explainer : Risk_models.Explainer.t option
  ; scratch : float array
  }

let create ?model ?(policy = Policy.default) config =
  let open Or_error.Let_syntax in
  let%bind config = Rules.Config.validate config in
  let%map hold_at =
    match model with
    | None -> return None
    | Some m -> Policy.thresholds policy m >>| fst
  in
  (* Explained from the lower edge of the abstain band, so abstentions carry one too. *)
  let explain_from =
    Option.map hold_at ~f:(fun h ->
      match Policy.abstainer policy ~hold_at:h with
      | None -> h
      | Some a -> Float.min h (fst (Risk_models.Abstain.band a)))
  in
  { config
  ; policy
  ; model
  ; hold_at
  ; explain_from
  ; (* A model exported before node covers falls back to path reason codes and no
       attribution, rather than refusing to start. *)
    explainer = Option.bind model ~f:(fun m -> Model.explainer m |> Result.ok)
  ; scratch = Array.create ~len:(Risk_models.Schema.length Features.schema) 0.
  }
;;

let config t = t.config
let policy t = t.policy
let model t = t.model

let context event ~customer ~counterparty =
  Context.create
    event
    ~customer:(Profile.Customer.view customer event)
    ~counterparty:(Profile.Counterparty.view counterparty event)
;;

(* The three largest positive contributions, as the model explanation's reasons. *)
let reasons_of (a : Risk_models.Explanation.t) =
  List.filter a.top ~f:(fun c -> Float.(c.contribution > 0.))
  |> (fun l -> List.take l 3)
  |> List.map ~f:(fun (c : Risk_models.Explanation.Contribution.t) ->
    c.feature, c.contribution)
;;

let decide t ctx =
  let rules = Rules.evaluate t.config ctx in
  let model, attribution =
    match t.model, t.policy.model_mode with
    | None, _ | Some _, Off -> None, None
    | Some m, (Shadow | Active) ->
      Features.encode_into t.config ctx t.scratch;
      let score = Model.score m t.scratch in
      (* Explanations cost tens of microseconds and some allocation, so only decisions a
         person will look at get one: rule alerts, and scores from the abstain band or the
         hold threshold up. *)
      let rules_alert =
        match rules.severity with
        | Allow -> false
        | Challenge | Review | Block -> true
      in
      let wanted =
        rules_alert || Option.exists t.explain_from ~f:(fun h -> Float.(score >= h))
      in
      (match t.explainer with
       | Some e when wanted ->
         let a = Risk_models.Explainer.explain ~k:5 ~label:Features.label e t.scratch in
         Some (m, score, reasons_of a), Some a
       | Some _ | None ->
         let reasons =
           match t.hold_at with
           | Some h when Float.(score >= h) -> snd (Model.explain m t.scratch)
           | Some _ | None -> []
         in
         Some (m, score, reasons), None)
  in
  (* [create] checked the thresholds, the only way [Decision.create] can fail *)
  let d = Decision.create t.policy ~event:ctx.event ~rules ~model |> Or_error.ok_exn in
  Option.value_map attribution ~default:d ~f:(Decision.with_attribution d)
;;

let apply event ~customer ~counterparty =
  Profile.Customer.update customer event;
  Profile.Counterparty.update counterparty event
;;

module Profiles = struct
  type t =
    { params : Profile.Params.t
    ; customers : Profile.Customer.t Ids.Customer_id.Table.t
    ; counterparties : Profile.Counterparty.t Ids.Counterparty_id.Table.t
    }

  let create params =
    { params
    ; customers = Ids.Customer_id.Table.create ()
    ; counterparties = Ids.Counterparty_id.Table.create ()
    }
  ;;

  let customer t id =
    Hashtbl.find_or_add t.customers id ~default:(fun () ->
      Profile.Customer.create t.params id)
  ;;

  let counterparty t id =
    Hashtbl.find_or_add t.counterparties id ~default:(fun () ->
      Profile.Counterparty.create id)
  ;;

  let customers t = Hashtbl.length t.customers
  let counterparties t = Hashtbl.length t.counterparties
end

let process t profiles (event : Event.t) =
  let customer = Profiles.customer profiles event.customer in
  let counterparty = Profiles.counterparty profiles event.counterparty in
  let decision = decide t (context event ~customer ~counterparty) in
  apply event ~customer ~counterparty;
  decision
;;
