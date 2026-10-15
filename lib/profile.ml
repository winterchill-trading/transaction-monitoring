open! Core

let never = Int.min_value
let ns = Time_ns.to_int_ns_since_epoch
let time_opt n = if n = never then None else Some (Time_ns.of_int_ns_since_epoch n)

module Params = struct
  type t =
    { reporting_threshold : int
    ; below_threshold_bps : int
    ; round_unit : int
    ; small_card_amount : int
    }
  [@@deriving sexp, compare, equal, bin_io]

  let default =
    { reporting_threshold = 1_000_000
    ; below_threshold_bps = 1_500
    ; round_unit = 10_000
    ; small_card_amount = 200
    }
  ;;

  let below_floor t =
    t.reporting_threshold - (t.reporting_threshold * t.below_threshold_bps / 10_000)
  ;;
end

(* Counter layout shared by the customer windows. The 7-day and 30-day windows keep only
   the first [long_fields]; the short windows keep all of them. *)
module F = struct
  let out_n = 0
  let out_sum = 1
  let in_n = 2
  let in_sum = 3
  let round_n = 4
  let below_n = 5
  let below_sum = 6
  let long_fields = 7
  let cash_n = 7
  let cash_sum = 8
  let card_n = 9
  let card_small_n = 10
  let declined_n = 11
  let all_fields = 12
end

(* Major units as a float, for quantiles and model features only (never for money). The
   reporting currency is assumed to have two decimals for these statistics; thresholds
   compare integer minor units. *)
let major minor = Float.of_int minor /. 100.

module Customer = struct
  module Stable = struct
    module Snapshot = struct
      module V1 = struct
        type t =
          { customer : Ids.Customer_id.Stable.V1.t
          ; params : Params.t
          ; mutable events : int
          ; mutable first_seen : int
          ; mutable last_seen : int
          ; mutable last_in : int
          ; w1h : Window.Stable.V1.t
          ; w24h : Window.Stable.V1.t
          ; w7d : Window.Stable.V1.t
          ; w30d : Window.Stable.V1.t
          ; out_p50 : P2_quantile.Stable.V1.t
          ; out_p95 : P2_quantile.Stable.V1.t
          ; mutable log_n : int
          ; mutable log_mean : float
          ; mutable log_m2 : float
          ; counterparties : Recent.Stable.V1.t
          ; devices : Recent.Stable.V1.t
          ; countries : Country_set.Stable.V1.t
          }
        [@@deriving bin_io, sexp, compare, equal]
      end
    end
  end

  type t = Stable.Snapshot.V1.t [@@deriving sexp_of]
  type snapshot = Stable.Snapshot.V1.t [@@deriving sexp_of, compare, equal]

  open Stable.Snapshot.V1

  let create params customer =
    { customer
    ; params
    ; events = 0
    ; first_seen = never
    ; last_seen = never
    ; last_in = never
    ; w1h = Window.create ~width:(Time_ns.Span.of_min 5.) ~buckets:12 ~fields:F.all_fields
    ; w24h = Window.create ~width:Time_ns.Span.hour ~buckets:24 ~fields:F.all_fields
    ; w7d = Window.create ~width:(Time_ns.Span.of_hr 6.) ~buckets:28 ~fields:F.long_fields
    ; w30d = Window.create ~width:Time_ns.Span.day ~buckets:30 ~fields:F.long_fields
    ; out_p50 = P2_quantile.create ~p:0.5
    ; out_p95 = P2_quantile.create ~p:0.95
    ; log_n = 0
    ; log_mean = 0.
    ; log_m2 = 0.
    ; counterparties = Recent.create ~capacity:64
    ; devices = Recent.create ~capacity:8
    ; countries = Country_set.create ()
    }
  ;;

  let customer t = t.customer
  let params t = t.params
  let events t = t.events

  let copy t =
    { t with
      w1h = Window.copy t.w1h
    ; w24h = Window.copy t.w24h
    ; w7d = Window.copy t.w7d
    ; w30d = Window.copy t.w30d
    ; out_p50 = P2_quantile.copy t.out_p50
    ; out_p95 = P2_quantile.copy t.out_p95
    ; counterparties = Recent.copy t.counterparties
    ; devices = Recent.copy t.devices
    ; countries = Country_set.copy t.countries
    }
  ;;

  let snapshot = copy
  let of_snapshot = copy
  let clamp_ns t at = Int.max (ns at) t.last_seen
  let clamp t at = Time_ns.of_int_ns_since_epoch (clamp_ns t at)

  let update t (e : Event.t) =
    let at_ns = clamp_ns t e.occurred_at in
    let at = Time_ns.of_int_ns_since_epoch at_ns in
    let amount = Event.base_minor e in
    let p = t.params in
    let v = Array.create ~len:F.all_fields 0 in
    let inbound =
      match e.direction with
      | Inbound -> true
      | Outbound -> false
    in
    if not e.declined
    then (
      if inbound
      then (
        v.(F.in_n) <- 1;
        v.(F.in_sum) <- amount;
        t.last_in <- at_ns)
      else (
        v.(F.out_n) <- 1;
        v.(F.out_sum) <- amount;
        let x = major amount in
        P2_quantile.add t.out_p50 x;
        P2_quantile.add t.out_p95 x;
        (* Welford's running mean and variance of log amounts *)
        let y = Float.log1p x in
        t.log_n <- t.log_n + 1;
        let delta = y -. t.log_mean in
        t.log_mean <- t.log_mean +. (delta /. Float.of_int t.log_n);
        t.log_m2 <- t.log_m2 +. (delta *. (y -. t.log_mean)));
      if amount > 0 && amount % p.round_unit = 0 then v.(F.round_n) <- 1;
      if amount >= Params.below_floor p && amount < p.reporting_threshold
      then (
        v.(F.below_n) <- 1;
        v.(F.below_sum) <- amount);
      if Event.is_cash e
      then (
        v.(F.cash_n) <- 1;
        v.(F.cash_sum) <- amount))
    else v.(F.declined_n) <- 1;
    (match e.rail with
     | Card ->
       v.(F.card_n) <- 1;
       if amount < p.small_card_amount then v.(F.card_small_n) <- 1
     | Sepa
     | Sepa_instant
     | Uk_faster_payments
     | Uk_chaps
     | Us_ach
     | Us_wire
     | Intraledger
     | Crypto -> ());
    Window.add_many t.w1h ~at v;
    Window.add_many t.w24h ~at v;
    Window.add_many t.w7d ~at v;
    Window.add_many t.w30d ~at v;
    Recent.touch
      t.counterparties
      (Ids.Counterparty_id.to_string e.counterparty)
      ~at
      ~inbound;
    Option.iter e.device_id ~f:(fun d -> Recent.touch t.devices d ~at ~inbound);
    Option.iter e.counterparty_country ~f:(Country_set.add t.countries);
    Option.iter e.ip_country ~f:(Country_set.add t.countries);
    if t.events = 0 then t.first_seen <- at_ns;
    t.events <- t.events + 1;
    t.last_seen <- at_ns
  ;;

  module View = struct
    type t =
      { now : Time_ns.Alternate_sexp.t
      ; events : int
      ; first_seen : Time_ns.Alternate_sexp.t option
      ; last_seen : Time_ns.Alternate_sexp.t option
      ; last_inbound : Time_ns.Alternate_sexp.t option
      ; out_n_1h : int
      ; out_sum_1h : int
      ; in_n_1h : int
      ; in_sum_1h : int
      ; out_n_24h : int
      ; out_sum_24h : int
      ; in_n_24h : int
      ; in_sum_24h : int
      ; out_n_7d : int
      ; out_sum_7d : int
      ; in_n_7d : int
      ; in_sum_7d : int
      ; out_n_30d : int
      ; out_sum_30d : int
      ; in_n_30d : int
      ; in_sum_30d : int
      ; round_n_30d : int
      ; below_n_24h : int
      ; below_sum_24h : int
      ; below_n_7d : int
      ; below_sum_7d : int
      ; cash_n_24h : int
      ; cash_sum_24h : int
      ; card_n_10m : int
      ; card_small_n_10m : int
      ; declined_n_10m : int
      ; declined_n_1h : int
      ; out_p50 : float option
      ; out_p95 : float option
      ; out_log_mean : float option
      ; out_log_sd : float option
      ; counterparty_first_seen : Time_ns.Alternate_sexp.t option
      ; senders_24h : int
      ; senders_7d : int
      ; beneficiaries_24h : int
      ; country_seen : bool option
      ; countries_seen : int
      ; device_seen : bool option
      }
    [@@deriving sexp_of, compare, equal]
  end

  let view t (e : Event.t) : View.t =
    let now = clamp t e.occurred_at in
    let total w field = Window.total w ~now ~field in
    let last w field n = Window.sum w ~now ~field ~last:n in
    let since span = Time_ns.sub now span in
    { now
    ; events = t.events
    ; first_seen = time_opt t.first_seen
    ; last_seen = time_opt t.last_seen
    ; last_inbound = time_opt t.last_in
    ; out_n_1h = total t.w1h F.out_n
    ; out_sum_1h = total t.w1h F.out_sum
    ; in_n_1h = total t.w1h F.in_n
    ; in_sum_1h = total t.w1h F.in_sum
    ; out_n_24h = total t.w24h F.out_n
    ; out_sum_24h = total t.w24h F.out_sum
    ; in_n_24h = total t.w24h F.in_n
    ; in_sum_24h = total t.w24h F.in_sum
    ; out_n_7d = total t.w7d F.out_n
    ; out_sum_7d = total t.w7d F.out_sum
    ; in_n_7d = total t.w7d F.in_n
    ; in_sum_7d = total t.w7d F.in_sum
    ; out_n_30d = total t.w30d F.out_n
    ; out_sum_30d = total t.w30d F.out_sum
    ; in_n_30d = total t.w30d F.in_n
    ; in_sum_30d = total t.w30d F.in_sum
    ; round_n_30d = total t.w30d F.round_n
    ; below_n_24h = total t.w24h F.below_n
    ; below_sum_24h = total t.w24h F.below_sum
    ; below_n_7d = total t.w7d F.below_n
    ; below_sum_7d = total t.w7d F.below_sum
    ; cash_n_24h = total t.w24h F.cash_n
    ; cash_sum_24h = total t.w24h F.cash_sum
    ; card_n_10m = last t.w1h F.card_n 3
    ; card_small_n_10m = last t.w1h F.card_small_n 3
    ; declined_n_10m = last t.w1h F.declined_n 3
    ; declined_n_1h = total t.w1h F.declined_n
    ; out_p50 = P2_quantile.estimate t.out_p50
    ; out_p95 = P2_quantile.estimate t.out_p95
    ; out_log_mean = (if t.log_n = 0 then None else Some t.log_mean)
    ; out_log_sd =
        (if t.log_n < 2
         then None
         else Some (Float.sqrt (t.log_m2 /. Float.of_int (t.log_n - 1))))
    ; counterparty_first_seen =
        Recent.first_seen t.counterparties (Ids.Counterparty_id.to_string e.counterparty)
    ; senders_24h =
        Recent.count_since t.counterparties ~since:(since Time_ns.Span.day) ~inbound:true
    ; senders_7d =
        Recent.count_since
          t.counterparties
          ~since:(since (Time_ns.Span.of_day 7.))
          ~inbound:true
    ; beneficiaries_24h =
        Recent.count_since t.counterparties ~since:(since Time_ns.Span.day) ~inbound:false
    ; country_seen = Option.map e.counterparty_country ~f:(Country_set.mem t.countries)
    ; countries_seen = Country_set.cardinal t.countries
    ; device_seen = Option.map e.device_id ~f:(Recent.mem t.devices)
    }
  ;;
end

module Counterparty = struct
  module G = struct
    let paid_n = 0
    let paid_sum = 1
    let sent_n = 2
    let sent_sum = 3
    let fields = 4
  end

  module Stable = struct
    module Snapshot = struct
      module V1 = struct
        type t =
          { counterparty : Ids.Counterparty_id.Stable.V1.t
          ; mutable events : int
          ; mutable first_seen : int
          ; mutable last_seen : int
          ; w24h : Window.Stable.V1.t
          ; w7d : Window.Stable.V1.t
          ; customers : Recent.Stable.V1.t
          }
        [@@deriving bin_io, sexp, compare, equal]
      end
    end
  end

  type t = Stable.Snapshot.V1.t [@@deriving sexp_of]
  type snapshot = Stable.Snapshot.V1.t [@@deriving sexp_of, compare, equal]

  open Stable.Snapshot.V1

  let create counterparty =
    { counterparty
    ; events = 0
    ; first_seen = never
    ; last_seen = never
    ; w24h = Window.create ~width:Time_ns.Span.hour ~buckets:24 ~fields:G.fields
    ; w7d = Window.create ~width:(Time_ns.Span.of_hr 6.) ~buckets:28 ~fields:G.fields
    ; customers = Recent.create ~capacity:32
    }
  ;;

  let counterparty t = t.counterparty
  let events t = t.events

  let copy t =
    { t with
      w24h = Window.copy t.w24h
    ; w7d = Window.copy t.w7d
    ; customers = Recent.copy t.customers
    }
  ;;

  let snapshot = copy
  let of_snapshot = copy

  let update t (e : Event.t) =
    let at_ns = Int.max (ns e.occurred_at) t.last_seen in
    let at = Time_ns.of_int_ns_since_epoch at_ns in
    (* "inbound" for the counterparty is money paid to it, i.e. a customer's outbound *)
    let paid =
      match e.direction with
      | Outbound -> true
      | Inbound -> false
    in
    if not e.declined
    then (
      let amount = Event.base_minor e in
      let v = Array.create ~len:G.fields 0 in
      if paid
      then (
        v.(G.paid_n) <- 1;
        v.(G.paid_sum) <- amount)
      else (
        v.(G.sent_n) <- 1;
        v.(G.sent_sum) <- amount);
      Window.add_many t.w24h ~at v;
      Window.add_many t.w7d ~at v;
      Recent.touch t.customers (Ids.Customer_id.to_string e.customer) ~at ~inbound:paid);
    if t.events = 0 then t.first_seen <- at_ns;
    t.events <- t.events + 1;
    t.last_seen <- at_ns
  ;;

  module View = struct
    type t =
      { events : int
      ; first_seen : Time_ns.Alternate_sexp.t option
      ; paid_n_24h : int
      ; paid_sum_24h : int
      ; paid_n_7d : int
      ; paid_sum_7d : int
      ; sent_n_24h : int
      ; sent_sum_24h : int
      ; sent_n_7d : int
      ; sent_sum_7d : int
      ; payers_7d : int
      ; payees_7d : int
      }
    [@@deriving sexp_of, compare, equal]
  end

  let view t (e : Event.t) : View.t =
    let now = Time_ns.of_int_ns_since_epoch (Int.max (ns e.occurred_at) t.last_seen) in
    let total w field = Window.total w ~now ~field in
    let week_ago = Time_ns.sub now (Time_ns.Span.of_day 7.) in
    { events = t.events
    ; first_seen = time_opt t.first_seen
    ; paid_n_24h = total t.w24h G.paid_n
    ; paid_sum_24h = total t.w24h G.paid_sum
    ; paid_n_7d = total t.w7d G.paid_n
    ; paid_sum_7d = total t.w7d G.paid_sum
    ; sent_n_24h = total t.w24h G.sent_n
    ; sent_sum_24h = total t.w24h G.sent_sum
    ; sent_n_7d = total t.w7d G.sent_n
    ; sent_sum_7d = total t.w7d G.sent_sum
    ; payers_7d = Recent.count_since t.customers ~since:week_ago ~inbound:true
    ; payees_7d = Recent.count_since t.customers ~since:week_ago ~inbound:false
    }
  ;;
end
