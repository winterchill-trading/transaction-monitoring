open! Core
module Rule = Risk_rules.Rule
module Country = Risk_rules.Country

module Config = struct
  let countries = List.map ~f:Country.of_string

  type t =
    { profile : Profile.Params.t [@default Profile.Params.default]
    ; structuring_min_count : int [@default 3]
    ; smurfing_min_payers : int [@default 4]
    ; rapid_min_inbound : int [@default 100_000]
    ; rapid_share_bps : int [@default 8_000]
    ; mule_min_payers : int [@default 5]
    ; mule_max_payees : int [@default 2]
    ; mule_max_account_age : Time_ns.Span.t [@default Time_ns.Span.of_day 90.]
    ; mule_collector_min_customers : int [@default 5]
    ; dormant_after : Time_ns.Span.t [@default Time_ns.Span.of_day 180.]
    ; dormant_min_amount : int [@default 500_000]
    ; high_risk_countries : Country.t list [@default countries [ "KP"; "IR"; "MM" ]]
    ; corridor_min_amount : int [@default 100_000]
    ; sanctions_adjacent_countries : Country.t list [@default countries [ "RU"; "BY" ]]
    ; card_testing_min_small : int [@default 5]
    ; card_testing_min_declines : int [@default 3]
    ; atm_max_count_24h : int [@default 6]
    ; atm_max_sum_24h : int [@default 300_000]
    ; unusual_multiple : float [@default 10.]
    ; unusual_min_amount : int [@default 300_000]
    ; unusual_min_history : int [@default 10]
    ; disabled : Typology.t list [@default []]
    ; shadow : Typology.t list [@default []]
    }
  [@@deriving sexp, compare, equal]

  let default = t_of_sexp (Sexp.List [])

  let validate t =
    let positive name v =
      if v > 0 then None else Some [%message "must be positive" name (v : int)]
    in
    let errors =
      List.filter_opt
        [ positive "reporting_threshold" t.profile.reporting_threshold
        ; positive "round_unit" t.profile.round_unit
        ; positive "small_card_amount" t.profile.small_card_amount
        ; (if t.profile.below_threshold_bps > 0 && t.profile.below_threshold_bps < 10_000
           then None
           else
             Some
               [%message
                 "below_threshold_bps outside (0, 10000)"
                   ~bps:(t.profile.below_threshold_bps : int)])
        ; positive "structuring_min_count" t.structuring_min_count
        ; positive "smurfing_min_payers" t.smurfing_min_payers
        ; positive "rapid_min_inbound" t.rapid_min_inbound
        ; positive "rapid_share_bps" t.rapid_share_bps
        ; positive "mule_min_payers" t.mule_min_payers
        ; positive "mule_max_payees" t.mule_max_payees
        ; positive "mule_collector_min_customers" t.mule_collector_min_customers
        ; positive "dormant_min_amount" t.dormant_min_amount
        ; positive "corridor_min_amount" t.corridor_min_amount
        ; positive "card_testing_min_small" t.card_testing_min_small
        ; positive "card_testing_min_declines" t.card_testing_min_declines
        ; positive "atm_max_count_24h" t.atm_max_count_24h
        ; positive "atm_max_sum_24h" t.atm_max_sum_24h
        ; positive "unusual_min_amount" t.unusual_min_amount
        ; (if Float.(t.unusual_multiple > 1.)
           then None
           else Some [%message "unusual_multiple must exceed 1"])
        ; (if Time_ns.Span.(t.dormant_after > zero && t.mule_max_account_age > zero)
           then None
           else Some [%message "spans must be positive"])
        ]
    in
    if List.is_empty errors
    then Ok t
    else
      Error (Error.create_s [%message "invalid rules config" ~_:(errors : Sexp.t list)])
  ;;

  let load path =
    Or_error.try_with (fun () -> Sexp.load_sexp_conv_exn path t_of_sexp)
    |> Or_error.bind ~f:validate
    |> Or_error.tag_s ~tag:[%message "loading rules config" path]
  ;;
end

(* Text forms of minor units for reasons and evidence: "8500.00". *)
let money minor = sprintf "%d.%02d" (minor / 100) (minor % 100)
let span_days span = sprintf "%.1fd" (Time_ns.Span.to_day span)
let country_list l = List.map l ~f:Country.to_string |> String.concat ~sep:","

let create (typology : Typology.t) ~severity check =
  (* The check returns the reason and the evidence together, so the facts in the reason
     are the facts in the evidence. Rule.create takes them as two functions; the second
     call recomputes, which is cheap and keeps both pure. *)
  Rule.create
    ~name:(Typology.name typology)
    ~severity
    ~code:(Typology.code typology)
    ~evidence:(fun ctx ->
      match check ctx with
      | None -> []
      | Some (_, evidence) -> evidence)
    (fun ctx -> Option.map (check ctx) ~f:fst)
;;

let structuring (c : Config.t) =
  let p = c.profile in
  let floor =
    p.reporting_threshold - (p.reporting_threshold * p.below_threshold_bps / 10_000)
  in
  create Structuring ~severity:Review (fun (ctx : Context.t) ->
    if ctx.amount >= floor && ctx.amount < p.reporting_threshold
    then (
      let n = ctx.customer.below_n_7d + 1 in
      if n >= c.structuring_min_count
      then
        Some
          ( sprintf
              "%d amounts in [%s, %s) within 7d, this one %s; together %s"
              n
              (money floor)
              (money p.reporting_threshold)
              (money ctx.amount)
              (money (ctx.customer.below_sum_7d + ctx.amount))
          , [ "count_7d", Int.to_string n
            ; "sum_7d", money (ctx.customer.below_sum_7d + ctx.amount)
            ; "threshold", money p.reporting_threshold
            ; "amount", money ctx.amount
            ] )
      else None)
    else None)
;;

let smurfing (c : Config.t) =
  let threshold = c.profile.reporting_threshold in
  create Smurfing ~severity:Review (fun (ctx : Context.t) ->
    let payers = ctx.customer.senders_24h + 1 in
    let total = ctx.customer.in_sum_24h + ctx.amount in
    if (not (Context.is_outbound ctx))
       && ctx.amount < threshold
       && payers >= c.smurfing_min_payers
       && total >= threshold
    then
      Some
        ( sprintf
            "%d payers in 24h sent %s in amounts under %s"
            payers
            (money total)
            (money threshold)
        , [ "payers_24h", Int.to_string payers
          ; "inbound_24h", money total
          ; "threshold", money threshold
          ] )
    else None)
;;

let rapid_movement (c : Config.t) =
  create Rapid_movement ~severity:Review (fun (ctx : Context.t) ->
    let inbound = ctx.customer.in_sum_24h in
    let outbound = ctx.customer.out_sum_24h + ctx.amount in
    (* Pass-through: what leaves matches what came in. Paying out much more than came in
       is spending from an existing balance, and a small payment after the money already
       left is the same episode, not a new one (the payment itself must be a quarter of
       the inflow). *)
    if Context.is_outbound ctx
       && inbound >= c.rapid_min_inbound
       && outbound * 10_000 >= inbound * c.rapid_share_bps
       && outbound * 10_000 <= inbound * (20_000 - c.rapid_share_bps)
       && ctx.amount * 4 >= inbound
    then (
      let since_inbound =
        Option.map ctx.customer.last_inbound ~f:(fun t ->
          Time_ns.Span.to_min (Time_ns.diff ctx.customer.now t))
      in
      Some
        ( sprintf
            "outbound %s in 24h against inbound %s (%.0f%%), last inbound %s ago"
            (money outbound)
            (money inbound)
            (100. *. Float.of_int outbound /. Float.of_int inbound)
            (Option.value_map since_inbound ~default:"?" ~f:(sprintf "%.0fmin"))
        , [ "inbound_24h", money inbound
          ; "outbound_24h", money outbound
          ; "share_bps", Int.to_string (outbound * 10_000 / inbound)
          ] ))
    else None)
;;

let mule_fan_in (c : Config.t) =
  create Mule_fan_in ~severity:Review (fun (ctx : Context.t) ->
    if not (Context.is_outbound ctx)
    then None
    else (
      let payers = ctx.customer.senders_7d in
      let payees = ctx.customer.beneficiaries_24h + 1 in
      let young =
        match Context.account_age ctx with
        | Some age -> Time_ns.Span.(age < c.mule_max_account_age)
        | None -> false
      in
      let drains =
        (ctx.customer.out_sum_24h + ctx.amount) * 2 >= ctx.customer.in_sum_7d
      in
      let collector = ctx.counterparty.payers_7d + 1 in
      (* A payee the bank first saw within two weeks: billers, merchants and landlords
         paid by many customers have a long history. Card payments never count. *)
      let new_payee =
        match ctx.counterparty.first_seen with
        | None -> true
        | Some first -> Time_ns.Span.(Time_ns.diff ctx.customer.now first < of_day 14.)
      in
      let card = Event.Rail.equal ctx.event.rail Card in
      if (not card)
         && payers >= c.mule_min_payers
         && payees <= c.mule_max_payees
         && (young || drains)
         && ctx.amount * 4 >= ctx.customer.in_sum_7d
      then
        Some
          ( sprintf
              "%d payers in 7d, paying out to %d payee(s) in 24h%s"
              payers
              payees
              (if young then ", young account" else ", over half of the inflow")
          , [ "payers_7d", Int.to_string payers
            ; "payees_24h", Int.to_string payees
            ; "inbound_7d", money ctx.customer.in_sum_7d
            ; ( "account_age"
              , Option.value_map (Context.account_age ctx) ~default:"unknown" ~f:span_days
              )
            ] )
      else if (not card) && new_payee && collector >= c.mule_collector_min_customers
      then
        Some
          ( sprintf "payee collects from %d of the bank's customers in 7d" collector
          , [ "payee_customers_7d", Int.to_string collector
            ; "payee_received_7d", money (ctx.counterparty.paid_sum_7d + ctx.amount)
            ] )
      else None))
;;

let dormant_reactivation (c : Config.t) =
  create Dormant_reactivation ~severity:Review (fun (ctx : Context.t) ->
    match ctx.customer.last_seen with
    | Some last when ctx.amount >= c.dormant_min_amount ->
      let idle = Time_ns.diff ctx.customer.now last in
      if Time_ns.Span.(idle >= c.dormant_after)
      then
        Some
          ( sprintf "%s after %s without activity" (money ctx.amount) (span_days idle)
          , [ "idle", span_days idle; "amount", money ctx.amount ] )
      else None
    | Some _ | None -> None)
;;

let high_risk_corridor (c : Config.t) =
  create High_risk_corridor ~severity:Review (fun (ctx : Context.t) ->
    match ctx.event.counterparty_country with
    | Some country when List.mem c.high_risk_countries country ~equal:Country.equal ->
      let first = Option.equal Bool.equal ctx.customer.country_seen (Some false) in
      if first || ctx.amount >= c.corridor_min_amount
      then
        Some
          ( sprintf
              "%s %s %s%s"
              (money ctx.amount)
              (if Context.is_outbound ctx then "to" else "from")
              (Country.to_string country)
              (if first then ", first time" else "")
          , [ "country", Country.to_string country
            ; "amount", money ctx.amount
            ; "first_time", Bool.to_string first
            ] )
      else None
    | Some _ | None -> None)
;;

let card_testing (c : Config.t) =
  create Card_testing ~severity:Block (fun (ctx : Context.t) ->
    match ctx.event.rail with
    | Card ->
      let small =
        ctx.customer.card_small_n_10m
        + if ctx.amount < c.profile.small_card_amount then 1 else 0
      in
      let declines = ctx.customer.declined_n_10m + if ctx.event.declined then 1 else 0 in
      if small >= c.card_testing_min_small || declines >= c.card_testing_min_declines
      then
        Some
          ( sprintf "%d small and %d declined card attempts in 10-15min" small declines
          , [ "small_10m", Int.to_string small; "declined_10m", Int.to_string declines ]
          )
      else None
    | Sepa
    | Sepa_instant
    | Uk_faster_payments
    | Uk_chaps
    | Us_ach
    | Us_wire
    | Intraledger
    | Crypto -> None)
;;

let atm_cash (c : Config.t) =
  create Atm_cash ~severity:Review (fun (ctx : Context.t) ->
    if Event.is_cash ctx.event && not ctx.event.declined
    then (
      let n = ctx.customer.cash_n_24h + 1 in
      let sum = ctx.customer.cash_sum_24h + ctx.amount in
      if n > c.atm_max_count_24h || sum > c.atm_max_sum_24h
      then
        Some
          ( sprintf "%d ATM operations totalling %s in 24h" n (money sum)
          , [ "count_24h", Int.to_string n
            ; "sum_24h", money sum
            ; ( "atm_country"
              , Option.value_map
                  ctx.event.counterparty_country
                  ~default:"unknown"
                  ~f:Country.to_string )
            ] )
      else None)
    else None)
;;

let sanctions_adjacent (c : Config.t) =
  create Sanctions_adjacent ~severity:Review (fun (ctx : Context.t) ->
    let listed = List.mem c.sanctions_adjacent_countries ~equal:Country.equal in
    let hits =
      List.filter_opt
        [ Option.filter ctx.event.counterparty_country ~f:listed
          |> Option.map ~f:(fun c -> "counterparty_country", c)
        ; Option.filter ctx.event.ip_country ~f:listed
          |> Option.map ~f:(fun c -> "ip_country", c)
        ]
    in
    if List.is_empty hits
    then None
    else
      Some
        ( sprintf "%s touches %s" (money ctx.amount) (List.map hits ~f:snd |> country_list)
        , List.map hits ~f:(fun (k, c) -> k, Country.to_string c) ))
;;

let threshold_report (c : Config.t) =
  let threshold = c.profile.reporting_threshold in
  create Threshold_report ~severity:Allow (fun (ctx : Context.t) ->
    if Event.is_cash ctx.event && not ctx.event.declined
    then (
      let sum = ctx.customer.cash_sum_24h + ctx.amount in
      if sum >= threshold
      then
        Some
          ( sprintf
              "cash %s in 24h reaches the reporting threshold %s"
              (money sum)
              (money threshold)
          , [ "cash_24h", money sum; "threshold", money threshold ] )
      else None)
    else None)
;;

let unusual_amount (c : Config.t) =
  create Unusual_amount ~severity:Review (fun (ctx : Context.t) ->
    match ctx.customer.out_p95 with
    | Some p95
      when Context.is_outbound ctx
           && ctx.amount >= c.unusual_min_amount
           && ctx.customer.events >= c.unusual_min_history ->
      let amount = Float.of_int ctx.amount /. 100. in
      if Float.(amount >= c.unusual_multiple * p95)
      then
        Some
          ( sprintf
              "%s is %.1fx the customer's p95 of %.2f"
              (money ctx.amount)
              (amount /. p95)
              p95
          , [ "amount", money ctx.amount
            ; "p95", sprintf "%.2f" p95
            ; "multiple", sprintf "%.1f" (amount /. p95)
            ] )
      else None
    | Some _ | None -> None)
;;

let rule (c : Config.t) (t : Typology.t) =
  match t with
  | Structuring -> structuring c
  | Smurfing -> smurfing c
  | Rapid_movement -> rapid_movement c
  | Mule_fan_in -> mule_fan_in c
  | Dormant_reactivation -> dormant_reactivation c
  | High_risk_corridor -> high_risk_corridor c
  | Card_testing -> card_testing c
  | Atm_cash -> atm_cash c
  | Sanctions_adjacent -> sanctions_adjacent c
  | Threshold_report -> threshold_report c
  | Unusual_amount -> unusual_amount c
;;

let all (c : Config.t) =
  List.filter_map Typology.all ~f:(fun t ->
    if List.mem c.disabled t ~equal:Typology.equal
    then None
    else (
      let r = rule c t in
      Some
        (if List.mem c.shadow t ~equal:Typology.equal then Rule.cap r ~max:Allow else r)))
;;

let evaluate c ctx =
  let decision = Risk_rules.Engine.evaluate (all c) ctx in
  Risk_rules.Decision.of_hits
    (List.map decision.hits ~f:(fun (hit : Risk_rules.Hit.t) ->
       match Typology.of_name hit.rule with
       | Some t -> { hit with typology = Typology.family t }
       | None -> hit))
;;

let requires_threshold_report (d : Risk_rules.Decision.t) =
  List.exists d.hits ~f:(fun h -> String.equal h.rule (Typology.name Threshold_report))
;;
