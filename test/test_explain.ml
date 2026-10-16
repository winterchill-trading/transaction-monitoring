open! Core
open Transaction_monitoring

let model = lazy (Model.load "../models/tm-aml-gbdt/model.json" |> Or_error.ok_exn)

(* A first payment of 1 200.00 to a new payee in a higher-risk country: the rules hold it,
   the model scores it high. *)
let events =
  [ Fixtures.event
      ~id:"in-1"
      ~direction:Inbound
      ~counterparty:"employer"
      ~at:(Fixtures.at_min 0.)
      310_000
  ; Fixtures.event ~id:"out-1" ~counterparty:"landlord" ~at:(Fixtures.at_min 60.) 95_000
  ; Fixtures.event
      ~id:"out-2"
      ~counterparty:"new-payee"
      ~counterparty_country:"IR"
      ~at:(Fixtures.at_min 90.)
      120_000
  ]
;;

let run policy =
  let evaluator =
    Evaluator.create ~model:(Lazy.force model) ~policy Rules.Config.default
    |> Or_error.ok_exn
  in
  let profiles = Evaluator.Profiles.create Rules.Config.default.profile in
  List.map events ~f:(Evaluator.process evaluator profiles)
;;

let show (d : Decision.t) =
  printf
    "%s: %s, score %s, abstained %b\n"
    (Ids.Event_id.to_string d.event_id)
    (Decision.Outcome.to_string d.outcome)
    (Option.value_map d.score ~default:"-" ~f:(sprintf "%.4f"))
    d.model_abstained;
  List.iter (Decision.why d) ~f:(printf "  %s\n")
;;

let%expect_test "held decisions carry the model's top contributions; others none" =
  let ds = run Policy.default in
  List.iter ds ~f:show;
  let held = List.last_exn ds in
  let a = Option.value_exn held.attribution in
  (* the explanation adds up to the model's margin *)
  let total =
    a.base +. a.rest +. List.sum (module Float) a.top ~f:(fun c -> c.contribution)
  in
  printf
    "method %s, %d features listed, base + contributions = margin: %b\n"
    (Risk_models.Explanation.Method.to_string a.method_)
    (List.length a.top)
    Float.(abs (total -. a.margin) < 1e-9);
  [%expect
    {|
    in-1: allow, score 0.0776, abstained false
      model contributions (log-odds, not causes): amount +3.44, hours since the payee was added +1.94, amount over the 30-day mean +1.28, amount over the customer's 95th percentile +1.05, customers paying the counterparty, 7 days +0.90
    out-1: allow, score 0.6967, abstained false
      model contributions (log-odds, not causes): amount +2.99, account age in days +2.37, hours since the payee was added +2.34, customers paying the counterparty, 7 days +1.10, amount over the 30-day mean +0.90
    out-2: hold, score 0.6961, abstained false
      [review] high_risk_corridor: 1200.00 to IR, first time
      model contributions (log-odds, not causes): amount +2.95, hours since the payee was added +2.60, account age in days +2.48, customers paying the counterparty, 7 days +1.33, counterparty in the country of residence +1.00
    method path_dependent, 5 features listed, base + contributions = margin: true
    |}]
;;

let%expect_test "abstentions: hold as model uncertain, or leave it to the rules" =
  (* A hold threshold right at the landlord payment's score and a wide band, so the
     ordinary payment abstains; the rules alone would allow it. *)
  let base = run Policy.default in
  let landlord = List.nth_exn base 1 in
  let hold_at = Option.value_exn landlord.score in
  List.iter Policy.Abstain.Route.all ~f:(fun route ->
    let policy =
      { Policy.name = "abstain"
      ; model_mode = Active
      ; hold_at = Some hold_at
      ; block_at = Some 0.99
      ; abstain = Some { min_confidence = 0.5; confidence = Logit_margin; route }
      }
    in
    printf "route %s:\n" (Sexp.to_string [%sexp (route : Policy.Abstain.Route.t)]);
    show (List.nth_exn (run policy) 1));
  [%expect
    {|
    route Hold:
    out-1: hold, score 0.6967, abstained true
      [review] tm-aml-gbdt@2f42f21773ff: model uncertain: score 0.696655 in the abstain band [0.582105, 0.791076) around the hold threshold 0.696655; held for review
      model contributions (log-odds, not causes): amount +2.99, account age in days +2.37, hours since the payee was added +2.34, customers paying the counterparty, 7 days +1.10, amount over the 30-day mean +0.90
    route Rules_only:
    out-1: allow, score 0.6967, abstained true
      model contributions (log-odds, not causes): amount +2.99, account age in days +2.37, hours since the payee was added +2.34, customers paying the counterparty, 7 days +1.10, amount over the 30-day mean +0.90
    |}]
;;
