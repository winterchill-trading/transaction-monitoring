open! Core

module Model_mode = struct
  type t =
    | Off
    | Shadow
    | Active
  [@@deriving sexp, compare, equal, enumerate]
end

module Abstain = struct
  module Route = struct
    type t =
      | Hold
      | Rules_only
    [@@deriving sexp, compare, equal, enumerate]
  end

  type t =
    { min_confidence : float
    ; confidence : Risk_models.Abstain.Confidence.t [@default Logit_margin]
    ; route : Route.t
    }
  [@@deriving sexp, compare, equal]
end

type t =
  { name : string [@default "default"]
  ; model_mode : Model_mode.t [@default Shadow]
  ; hold_at : float option [@default None]
  ; block_at : float option [@default None]
  ; abstain : Abstain.t option [@sexp.option]
  }
[@@deriving sexp, compare, equal]

let default = t_of_sexp (Sexp.List [])

let thresholds t model =
  let from_model = Model.thresholds model in
  let hold_at =
    Option.first_some t.hold_at (Option.map from_model ~f:(fun th -> th.review))
  in
  let block_at =
    Option.first_some t.block_at (Option.bind from_model ~f:(fun th -> th.block))
  in
  let in_range = Option.for_all ~f:(fun x -> Float.(x >= 0. && x <= 1.)) in
  if not (in_range hold_at && in_range block_at)
  then
    Or_error.error_s
      [%message
        "thresholds outside [0, 1]" (hold_at : float option) (block_at : float option)]
  else (
    match hold_at, block_at with
    | Some h, Some b when Float.(h > b) ->
      Or_error.error_s [%message "hold_at above block_at" (h : float) (b : float)]
    | _ ->
      (match t.abstain, hold_at with
       | Some a, Some h ->
         Risk_models.Abstain.create
           ~threshold:h
           ~confidence:a.confidence
           ~min_confidence:a.min_confidence
         |> Or_error.map ~f:(fun (_ : Risk_models.Abstain.t) -> hold_at, block_at)
       | Some _, None -> Or_error.error_s [%message "abstain needs a hold threshold"]
       | None, _ -> Ok (hold_at, block_at)))
;;

let abstainer t ~hold_at =
  Option.map t.abstain ~f:(fun (a : Abstain.t) ->
    (* [thresholds] validated these when the evaluator was created. *)
    Risk_models.Abstain.create
      ~threshold:hold_at
      ~confidence:a.confidence
      ~min_confidence:a.min_confidence
    |> Or_error.ok_exn)
;;

let load path =
  Or_error.try_with (fun () -> Sexp.load_sexp_conv_exn path t_of_sexp)
  |> Or_error.tag_s ~tag:[%message "loading policy" path]
;;
