open! Core
open Transaction_monitoring

let model = lazy (Model.load "../models/tm-aml-gbdt/model.json" |> Or_error.ok_exn)

(* Rows exported by ml/train.py with scikit-learn's own margin, probability and score;
   empty cells are missing values. *)
let parity_rows () =
  match In_channel.read_lines "fixtures/parity.csv" with
  | [] -> assert false
  | header :: rows ->
    let names = String.split header ~on:',' in
    let n = List.length names - 3 in
    [%test_result: string list]
      (List.take names n)
      ~expect:(Array.to_list Features.schema.features);
    List.map rows ~f:(fun row ->
      let cells =
        String.split row ~on:','
        |> List.map ~f:(fun c -> if String.is_empty c then Float.nan else Float.of_string c)
        |> Array.of_list
      in
      Array.sub cells ~pos:0 ~len:n, cells.(n), cells.(n + 1), cells.(n + 2))
;;

let%expect_test "OCaml scores equal scikit-learn's on the parity rows" =
  let m = Lazy.force model in
  let scorer = Model.scorer m in
  let rows = parity_rows () in
  let worst =
    List.fold rows ~init:0. ~f:(fun worst (x, margin, prob, score) ->
      let d =
        Float.max
          (Float.abs (Risk_models.Scorer.margin scorer x -. margin))
          (Float.max
             (Float.abs (Risk_models.Scorer.probability scorer x -. prob))
             (Float.abs (Model.score m x -. score)))
      in
      Float.max worst d)
  in
  (* Trees add and compare only, so the margin is bit-identical across platforms; the
     sigmoid goes through exp, whose last bit may differ between libm versions. *)
  printf "%d rows, max difference below 1e-9: %b\n" (List.length rows) Float.(worst < 1e-9);
  [%expect {| 400 rows, max difference below 1e-9: true |}]
;;

let%expect_test "model metadata" =
  let m = Lazy.force model in
  let th = Option.value_exn (Model.thresholds m) in
  printf
    "%s: review %.6f for %d alerts a day, block %s\n"
    (Model.name m)
    th.review
    th.alerts_per_day
    (Option.value_map th.block ~default:"none" ~f:(sprintf "%.6f"));
  [%expect {| tm-aml-gbdt: review 0.000524 for 60 alerts a day, block 0.117125 |}]
;;

let%expect_test "a model trained on another schema is refused" =
  let json = Yojson.Safe.from_file "../models/tm-aml-gbdt/model.json" in
  let renamed =
    match json with
    | `Assoc fields ->
      `Assoc
        (List.map fields ~f:(fun (k, v) ->
           if String.equal k "schema"
           then
             ( k
             , match v with
               | `Assoc s ->
                 `Assoc
                   (List.map s ~f:(fun (k, v) ->
                      match k, v with
                      | "features", `List (_ :: rest) -> k, `List (`String "other" :: rest)
                      | "digest", _ ->
                        ( k
                        , `String
                            (Risk_models.Schema.digest_of_names
                               ("other"
                                :: List.tl_exn (Array.to_list Features.schema.features))) )
                      | _ -> k, v))
               | v -> v )
           else k, v))
    | j -> j
  in
  (match Risk_models.Scorer.of_yojson ~expected:Features.schema renamed with
   | Ok _ -> print_endline "accepted"
   | Error _ -> print_endline "refused");
  [%expect {| refused |}]
;;

let%expect_test "decisions with the model: shadow records, active acts" =
  let events =
    Synthetic.generate { Synthetic.Config.default with customers = 200; days = 30; bad_share = 0.3 }
  in
  let run mode =
    let evaluator =
      Evaluator.create
        ~model:(Lazy.force model)
        ~policy:{ Policy.default with model_mode = mode }
        Rules.Config.default
      |> Or_error.ok_exn
    in
    let replay = Replay.create evaluator in
    List.iter events ~f:(fun l -> ignore (Replay.step ?label:l.label replay l.event : Decision.t));
    let r = Replay.report replay in
    printf
      "%s: hold %d, block %d, labelled alerted %d of %d, unlabelled alerted %d\n"
      (Sexp.to_string [%sexp (mode : Policy.Model_mode.t)])
      r.candidate.hold
      r.candidate.block
      r.candidate.labelled_alerted
      r.candidate.labelled
      r.candidate.unlabelled_alerted
  in
  List.iter Policy.Model_mode.all ~f:run;
  [%expect {|
    Off: hold 328, block 41, labelled alerted 123 of 223, unlabelled alerted 246
    Shadow: hold 328, block 41, labelled alerted 123 of 223, unlabelled alerted 246
    Active: hold 915, block 322, labelled alerted 220 of 223, unlabelled alerted 1017
    |}]
;;
