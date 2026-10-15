open! Core

type t =
  { config : Rules.Config.t
  ; policy : Policy.t
  ; model : Model.t option
  ; hold_at : float option
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
  { config
  ; policy
  ; model
  ; hold_at
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

let decide t ctx =
  let rules = Rules.evaluate t.config ctx in
  let model =
    match t.model, t.policy.model_mode with
    | None, _ | Some _, Off -> None
    | Some m, (Shadow | Active) ->
      Features.encode_into t.config ctx t.scratch;
      let score = Model.score m t.scratch in
      (* Reasons cost a tree walk with attributions and some allocation; only scores that
         reach a threshold are explained. *)
      let reasons =
        match t.hold_at with
        | Some h when Float.(score >= h) -> snd (Model.explain m t.scratch)
        | Some _ | None -> []
      in
      Some (m, score, reasons)
  in
  (* [create] checked the thresholds, the only way [Decision.create] can fail *)
  Decision.create t.policy ~event:ctx.event ~rules ~model |> Or_error.ok_exn
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
