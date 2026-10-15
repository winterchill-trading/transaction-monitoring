open! Core
open Transaction_monitoring

let ns = Time_ns.to_int_ns_since_epoch

(* Each event at the time the profile applies it: never earlier than the one before. *)
let clamped history =
  let _, applied =
    List.fold_map history ~init:Int.min_value ~f:(fun last (e : Event.t) ->
      let at = Int.max last (ns e.occurred_at) in
      at, (at, e))
  in
  applied
;;

let customer_view (p : Profile.Params.t) ~history (e : Event.t) : Profile.Customer.View.t =
  let applied = clamped history in
  let last_at = List.last applied |> Option.map ~f:fst in
  let now_ns = Int.max (ns e.occurred_at) (Option.value last_at ~default:Int.min_value) in
  let now = Time_ns.of_int_ns_since_epoch now_ns in
  let in_window ~width_min ~buckets (at, _) =
    let w = Float.to_int (width_min *. 60e9) in
    at / w > (now_ns / w) - buckets && at / w <= now_ns / w
  in
  let h1 = in_window ~width_min:5. ~buckets:12 in
  let h10m = in_window ~width_min:5. ~buckets:3 in
  let h24 = in_window ~width_min:60. ~buckets:24 in
  let d7 = in_window ~width_min:360. ~buckets:28 in
  let d30 = in_window ~width_min:1440. ~buckets:30 in
  let ok (_, (e : Event.t)) = not e.declined in
  let out (_, (e : Event.t)) = Event.Direction.equal e.direction Outbound in
  let amount (_, e) = Event.base_minor e in
  let count f = List.count applied ~f in
  let sum f = List.sum (module Int) (List.filter applied ~f) ~f:amount in
  let out_ok x = ok x && out x
  and in_ok x = ok x && not (out x) in
  let below x =
    let a = amount x in
    ok x
    && a >= p.reporting_threshold - (p.reporting_threshold * p.below_threshold_bps / 10_000)
    && a < p.reporting_threshold
  in
  let round x = ok x && amount x > 0 && amount x % p.round_unit = 0 in
  let cash (x : _ * Event.t) = ok x && Event.is_cash (snd x) in
  let card (_, (e : Event.t)) = Event.Rail.equal e.rail Card in
  let outgoing = List.filter applied ~f:out_ok |> List.map ~f:(fun x -> Float.of_int (amount x) /. 100.) in
  let quantile q =
    let sketch = P2_quantile.create ~p:q in
    List.iter outgoing ~f:(P2_quantile.add sketch);
    P2_quantile.estimate sketch
  in
  let logs = List.map outgoing ~f:Float.log1p in
  let n = List.length logs in
  let mean = if n = 0 then None else Some (List.sum (module Float) logs ~f:Fn.id /. Float.of_int n) in
  let sd =
    match mean with
    | Some m when n >= 2 ->
      Some
        (Float.sqrt
           (List.sum (module Float) logs ~f:(fun y -> (y -. m) *. (y -. m)) /. Float.of_int (n - 1)))
    | _ -> None
  in
  let distinct ~since ~inbound =
    List.filter applied ~f:(fun (at, (x : Event.t)) ->
      at >= ns since && Bool.equal inbound (Event.Direction.equal x.direction Inbound))
    |> List.map ~f:(fun (_, (x : Event.t)) -> Ids.Counterparty_id.to_string x.counterparty)
    |> List.dedup_and_sort ~compare:String.compare
    |> List.length
  in
  let countries =
    List.concat_map applied ~f:(fun (_, (x : Event.t)) ->
      List.filter_opt [ x.counterparty_country; x.ip_country ])
    |> List.dedup_and_sort ~compare:Risk_rules.Country.compare
  in
  let devices = List.filter_map applied ~f:(fun (_, (x : Event.t)) -> x.device_id) in
  let time n = Time_ns.of_int_ns_since_epoch n in
  let ago span = Time_ns.sub now span in
  { now
  ; events = List.length applied
  ; first_seen = List.hd applied |> Option.map ~f:(fun (at, _) -> time at)
  ; last_seen = Option.map last_at ~f:time
  ; last_inbound =
      List.filter applied ~f:in_ok |> List.last |> Option.map ~f:(fun (at, _) -> time at)
  ; out_n_1h = count (fun x -> h1 x && out_ok x)
  ; out_sum_1h = sum (fun x -> h1 x && out_ok x)
  ; in_n_1h = count (fun x -> h1 x && in_ok x)
  ; in_sum_1h = sum (fun x -> h1 x && in_ok x)
  ; out_n_24h = count (fun x -> h24 x && out_ok x)
  ; out_sum_24h = sum (fun x -> h24 x && out_ok x)
  ; in_n_24h = count (fun x -> h24 x && in_ok x)
  ; in_sum_24h = sum (fun x -> h24 x && in_ok x)
  ; out_n_7d = count (fun x -> d7 x && out_ok x)
  ; out_sum_7d = sum (fun x -> d7 x && out_ok x)
  ; in_n_7d = count (fun x -> d7 x && in_ok x)
  ; in_sum_7d = sum (fun x -> d7 x && in_ok x)
  ; out_n_30d = count (fun x -> d30 x && out_ok x)
  ; out_sum_30d = sum (fun x -> d30 x && out_ok x)
  ; in_n_30d = count (fun x -> d30 x && in_ok x)
  ; in_sum_30d = sum (fun x -> d30 x && in_ok x)
  ; round_n_30d = count (fun x -> d30 x && round x)
  ; below_n_24h = count (fun x -> h24 x && below x)
  ; below_sum_24h = sum (fun x -> h24 x && below x)
  ; below_n_7d = count (fun x -> d7 x && below x)
  ; below_sum_7d = sum (fun x -> d7 x && below x)
  ; cash_n_24h = count (fun x -> h24 x && cash x)
  ; cash_sum_24h = sum (fun x -> h24 x && cash x)
  ; card_n_10m = count (fun x -> h10m x && card x)
  ; card_small_n_10m = count (fun x -> h10m x && card x && amount x < p.small_card_amount)
  ; declined_n_10m = count (fun x -> h10m x && not (ok x))
  ; declined_n_1h = count (fun x -> h1 x && not (ok x))
  ; out_p50 = quantile 0.5
  ; out_p95 = quantile 0.95
  ; out_log_mean = mean
  ; out_log_sd = sd
  ; counterparty_first_seen =
      List.find applied ~f:(fun (_, (x : Event.t)) ->
        Ids.Counterparty_id.equal x.counterparty e.counterparty)
      |> Option.map ~f:(fun (at, _) -> time at)
  ; senders_24h = distinct ~since:(ago Time_ns.Span.day) ~inbound:true
  ; senders_7d = distinct ~since:(ago (Time_ns.Span.of_day 7.)) ~inbound:true
  ; beneficiaries_24h = distinct ~since:(ago Time_ns.Span.day) ~inbound:false
  ; country_seen =
      Option.map e.counterparty_country ~f:(fun c ->
        List.mem countries c ~equal:Risk_rules.Country.equal)
  ; countries_seen = List.length countries
  ; device_seen = Option.map e.device_id ~f:(fun d -> List.mem devices d ~equal:String.equal)
  }
;;
