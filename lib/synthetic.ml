open! Core

module Config = struct
  type t =
    { customers : int
    ; days : int
    ; seed : int
    ; start : Time_ns.t
    ; bad_share : float
    }
  [@@deriving sexp_of]

  let default =
    { customers = 5_000
    ; days = 75
    ; seed = 42
    ; start = Time_ns.of_string_with_utc_offset "2026-01-05T00:00:00Z"
    ; bad_share = 0.08
    }
  ;;
end

type labelled =
  { event : Event.t
  ; label : Typology.t option
  }

module Rng = struct
  let float t = Random.State.float t 1.
  let int t n = Random.State.int t n
  let range t lo hi = lo + Random.State.int t (hi - lo + 1)
  let chance t p = Float.(float t < p)
  let pick t a = a.(int t (Array.length a))

  let normal t =
    (* Box-Muller *)
    let u1 = Float.max 1e-12 (float t)
    and u2 = float t in
    Float.sqrt (-2. *. Float.log u1) *. Float.cos (2. *. Float.pi *. u2)
  ;;

  (* Log-normal with the given median, in cents. *)
  let amount t ~median ~sigma =
    Int.max 1 (Float.iround_nearest_exn (median *. 100. *. Float.exp (sigma *. normal t)))
  ;;

  let poisson t lambda =
    let l = Float.exp (-.lambda) in
    let rec go k p = if Float.(p <= l) then k else go (k + 1) (p *. float t) in
    go 0 (float t)
  ;;
end

let country = Risk_rules.Country.of_string
let residences = [| "DE"; "FR"; "NL"; "ES"; "IT"; "BE"; "AT" |]
let abroad = [| "GB"; "US"; "CH"; "PT"; "GR"; "TH"; "TR"; "AE"; "MA"; "MX" |]
let remittance_countries = [| "TR"; "MA"; "PH"; "IN"; "NG"; "UA"; "RS"; "VN" |]

(* MCCs for the merchant pool: groceries, restaurants, fuel, online retail, transport,
   streaming, pharmacies, clothing, electronics, hotels *)
let mccs = [| 5411; 5812; 5541; 5999; 4111; 4899; 5912; 5651; 5732; 7011 |]

type persona =
  | Salaried
  | Student
  | Business
  | Expat

(* Plain-data description of one generated event, turned into an [Event.t] at the end. *)
type draft =
  { minute : float (** since [start] *)
  ; customer : int
  ; counterparty : string
  ; cp_country : string option
  ; rail : Event.Rail.t
  ; direction : Event.Direction.t
  ; cents : int
  ; currency : Money.Currency.t
  ; channel : Event.Card_channel.t option
  ; mcc : int option
  ; declined : bool
  ; device : string option
  ; ip : string option
  ; label : Typology.t option
  }

type customer =
  { idx : int
  ; persona : persona
  ; residence : string
  ; opened_day : float (** relative to day 0; negative = before *)
  ; band : Event.Risk_band.t
  ; devices : string array
  ; salary : int
  ; employer : string
  ; landlord : string
  ; rent : int
  ; friends : (string * string) array (** id, country *)
  ; favourite_merchants : int array
  ; home_atms : int array
  }

let merchants = 600
let atms = 300

let merchant_country rng i =
  (* 80% of merchants domestic-looking EU, the rest abroad; derived from the index so a
     merchant always has the same country *)
  ignore rng;
  if i % 5 = 0
  then abroad.(i % Array.length abroad)
  else residences.(i % Array.length residences)
;;

let make_customer rng idx =
  let persona =
    let r = Rng.float rng in
    if Float.(r < 0.66)
    then Salaried
    else if Float.(r < 0.78)
    then Student
    else if Float.(r < 0.88)
    then Business
    else Expat
  in
  let residence = Rng.pick rng residences in
  { idx
  ; persona
  ; residence
  ; opened_day = -.Float.of_int (Rng.range rng 20 1500)
  ; band =
      (let r = Rng.float rng in
       if Float.(r < 0.75) then Low else if Float.(r < 0.95) then Medium else High)
  ; devices = Array.init (Rng.range rng 1 2) ~f:(fun d -> sprintf "dev-%05d-%d" idx d)
  ; salary =
      (match persona with
       | Student -> Rng.amount rng ~median:700. ~sigma:0.3
       | Salaried | Expat -> Rng.amount rng ~median:2_600. ~sigma:0.35
       | Business -> Rng.amount rng ~median:4_000. ~sigma:0.5)
  ; employer = sprintf "cp-emp-%03d" (Rng.int rng 250)
  ; landlord = sprintf "cp-landlord-%05d" idx
  ; rent = 10_000 * Rng.range rng 4 16 (* rents are round: 400.00 to 1600.00 *)
  ; friends =
      Array.init (Rng.range rng 3 12) ~f:(fun k ->
        ( sprintf "cp-p2p-%05d-%d" idx k
        , if Rng.chance rng 0.15 then Rng.pick rng abroad else residence ))
  ; favourite_merchants = Array.init 8 ~f:(fun _ -> Rng.int rng merchants)
  ; home_atms = Array.init 3 ~f:(fun _ -> Rng.int rng atms)
  }
;;

let base_draft c ~minute =
  { minute
  ; customer = c.idx
  ; counterparty = ""
  ; cp_country = None
  ; rail = Sepa
  ; direction = Outbound
  ; cents = 0
  ; currency = Money.Currency.eur
  ; channel = None
  ; mcc = None
  ; declined = false
  ; device = None
  ; ip = None
  ; label = None
  }
;;

let day_minute rng day ~from_hour ~to_hour =
  (Float.of_int day *. 1440.)
  +. (60. *. (from_hour +. (Rng.float rng *. (to_hour -. from_hour))))
;;

let card rng c ~minute ?(abroad_country = None) ?label ?(declined = false) ?cents () =
  let m =
    if Rng.chance rng 0.7
    then Rng.pick rng c.favourite_merchants
    else Rng.int rng merchants
  in
  let online = Rng.chance rng 0.35 in
  let cents = Option.value cents ~default:(Rng.amount rng ~median:28. ~sigma:0.9) in
  let mcountry =
    match abroad_country with
    | Some a -> a
    | None -> merchant_country rng m
  in
  { (base_draft c ~minute) with
    counterparty = sprintf "cp-merch-%03d" m
  ; cp_country = Some mcountry
  ; rail = Card
  ; cents
  ; channel = Some (if online then Online else In_person)
  ; mcc = Some mccs.(m % Array.length mccs)
  ; declined = declined || Rng.chance rng 0.015
  ; device = (if online then Some (Rng.pick rng c.devices) else None)
  ; ip = (if online then Some c.residence else None)
  ; label
  }
;;

let atm
  rng
  c
  ~minute
  ?(country = None)
  ?label
  ?(direction = Event.Direction.Outbound)
  cents
  =
  let a = Rng.pick rng c.home_atms in
  { (base_draft c ~minute) with
    counterparty = sprintf "cp-atm-%03d" a
  ; cp_country = Some (Option.value country ~default:c.residence)
  ; rail = Card
  ; direction
  ; cents
  ; channel = Some Atm
  ; mcc = Some 6011
  ; label
  }
;;

let transfer
  c
  ~minute
  ?(rail = Event.Rail.Sepa)
  ?(direction = Event.Direction.Outbound)
  ?country
  ?label
  ~counterparty
  cents
  =
  { (base_draft c ~minute) with
    counterparty
  ; cp_country = Some (Option.value country ~default:c.residence)
  ; rail
  ; direction
  ; cents
  ; device =
      (match direction with
       | Outbound -> Some c.devices.(0)
       | Inbound -> None)
  ; ip =
      (match direction with
       | Outbound -> Some c.residence
       | Inbound -> None)
  ; label
  }
;;

(* Everyday life for one customer over the simulated days. *)
let normal_activity rng c ~days ~quiet_until =
  let out = ref [] in
  let add d = out := d :: !out in
  let salary_day = Rng.range rng 0 4 in
  let traveller =
    Rng.chance
      rng
      (match c.persona with
       | Expat -> 0.4
       | _ -> 0.08)
  in
  let trip_start = Rng.range rng 5 (Int.max 6 (days - 10)) in
  let trip_country = Rng.pick rng abroad in
  for day = quiet_until to days - 1 do
    let in_trip = traveller && day >= trip_start && day < trip_start + 7 in
    let m from_hour to_hour = day_minute rng day ~from_hour ~to_hour in
    (* monthly flows, on fixed days of a 30-day cycle *)
    if day % 30 = salary_day
    then (
      match c.persona with
      | Business -> ()
      | Salaried | Student | Expat ->
        add
          (transfer
             c
             ~minute:(m 6. 9.)
             ~direction:Inbound
             ~counterparty:c.employer
             c.salary));
    if day % 30 = (salary_day + 2) % 30
       && not
            (match c.persona with
             | Business -> true
             | _ -> false)
    then add (transfer c ~minute:(m 8. 20.) ~counterparty:c.landlord c.rent);
    if day % 30 = (salary_day + 5) % 30
    then (
      add
        (transfer
           c
           ~minute:(m 8. 12.)
           ~rail:Sepa
           ~counterparty:(sprintf "cp-util-%02d" (c.idx % 40))
           (Rng.amount rng ~median:85. ~sigma:0.3));
      add
        (transfer
           c
           ~minute:(m 8. 12.)
           ~counterparty:(sprintf "cp-telco-%02d" (c.idx % 7))
           (Rng.amount rng ~median:35. ~sigma:0.2));
      if Rng.chance rng 0.4
      then
        add
          (transfer
             c
             ~minute:(m 18. 22.)
             ~rail:Intraledger
             ~counterparty:(sprintf "cp-savings-%05d" c.idx)
             (10_000 * Rng.range rng 1 10)));
    (match c.persona with
     | Expat when day % 30 = (salary_day + 3) % 30 ->
       let country = Rng.pick rng remittance_countries in
       add
         (transfer
            c
            ~minute:(m 10. 20.)
            ~rail:Us_wire
            ~country
            ~counterparty:(sprintf "cp-family-%05d" c.idx)
            (Rng.amount rng ~median:300. ~sigma:0.5))
     | Business ->
       (* many different clients paying invoices, suppliers paid, the odd large invoice *)
       for _ = 1 to Rng.poisson rng 1.6 do
         let client = sprintf "cp-client-%05d-%d" c.idx (Rng.int rng 60) in
         let cents =
           if Rng.chance rng 0.04
           then Rng.range rng 850_000 990_000
           else Rng.amount rng ~median:450. ~sigma:0.9
         in
         add (transfer c ~minute:(m 8. 18.) ~direction:Inbound ~counterparty:client cents)
       done;
       if Rng.chance rng 0.35
       then
         add
           (transfer
              c
              ~minute:(m 9. 17.)
              ~counterparty:(sprintf "cp-supplier-%03d" (Rng.int rng 120))
              (Rng.amount rng ~median:900. ~sigma:0.8));
       if Rng.chance rng 0.01
       then
         add
           (atm
              rng
              c
              ~minute:(m 10. 16.)
              ~direction:Inbound
              (Rng.range rng 30 120 * 10_000))
     | Salaried | Student | Expat -> ());
    (* card spending *)
    let spend =
      match c.persona with
      | Business -> 2.5
      | Student -> 1.2
      | Salaried | Expat -> 1.6
    in
    for _ = 1 to Rng.poisson rng (if in_trip then spend *. 1.6 else spend) do
      add
        (card
           rng
           c
           ~minute:(m 7. 23.)
           ~abroad_country:(if in_trip then Some trip_country else None)
           ())
    done;
    (* cash *)
    if Rng.chance rng (if in_trip then 0.5 else 0.12)
    then
      add
        (atm
           rng
           c
           ~minute:(m 8. 22.)
           ~country:(if in_trip then Some trip_country else None)
           (10_000 * Rng.range rng 1 (if in_trip then 5 else 3)));
    (* friends *)
    if Rng.chance rng 0.12
    then (
      let id, country = Rng.pick rng c.friends in
      let cents =
        if Rng.chance rng 0.4
        then 1_000 * Rng.range rng 1 20
        else Rng.amount rng ~median:45. ~sigma:0.8
      in
      add
        (transfer
           c
           ~minute:(m 9. 23.)
           ~rail:(if Rng.chance rng 0.5 then Sepa_instant else Sepa)
           ~country
           ~counterparty:id
           cents));
    if Rng.chance rng 0.08
    then (
      let id, country = Rng.pick rng c.friends in
      add
        (transfer
           c
           ~minute:(m 9. 23.)
           ~direction:Inbound
           ~country
           ~counterparty:id
           (Rng.amount rng ~median:40. ~sigma:0.8)));
    (* the rare big purchase: a car, a deposit; a look-alike for unusual amounts *)
    if Rng.chance rng 0.0006
    then
      add
        (transfer
           c
           ~minute:(m 10. 16.)
           ~counterparty:(sprintf "cp-dealer-%02d" (Rng.int rng 30))
           (Rng.range rng 500_000 2_500_000))
  done;
  !out
;;

let typologies : Typology.t array =
  [| Structuring
   ; Smurfing
   ; Rapid_movement
   ; Mule_fan_in
   ; Dormant_reactivation
   ; High_risk_corridor
   ; Card_testing
   ; Atm_cash
   ; Sanctions_adjacent
  |]
;;

(* One episode of [typology]; [subtle] episodes stay under the default rule thresholds. *)
let episode rng c (typology : Typology.t) ~day ~subtle =
  let label = Some typology in
  let at d h = (Float.of_int d *. 1440.) +. (h *. 60.) +. (Rng.float rng *. 50.) in
  let h () = 8. +. (Rng.float rng *. 12.) in
  match typology with
  | Structuring ->
    let n = if subtle then 2 else Rng.range rng 3 6 in
    let gap = if subtle then 5 else 1 in
    List.init n ~f:(fun i ->
      let cents = Rng.range rng 860_000 995_000 in
      if Rng.chance rng 0.5
      then atm rng c ~minute:(at (day + (i * gap)) (h ())) ~direction:Inbound ?label cents
      else
        transfer
          c
          ~minute:(at (day + (i * gap)) (h ()))
          ~direction:Inbound
          ?label
          ~counterparty:(sprintf "cp-struct-%05d" c.idx)
          cents)
  | Smurfing ->
    let n = if subtle then 3 else Rng.range rng 5 12 in
    List.init n ~f:(fun i ->
      transfer
        c
        ~minute:(at day (8. +. Float.of_int i))
        ~direction:Inbound
        ?label
        ~counterparty:(sprintf "cp-smurf-%05d-%d" c.idx i)
        (Rng.range rng 150_000 450_000))
  | Rapid_movement ->
    let inbound = Rng.range rng 500_000 4_000_000 in
    let share =
      if subtle then 0.55 +. (Rng.float rng *. 0.2) else 0.85 +. (Rng.float rng *. 0.15)
    in
    let start = at day (h ()) in
    [ transfer
        c
        ~minute:start
        ~direction:Inbound
        ?label
        ~counterparty:(sprintf "cp-src-%04d" (Rng.int rng 500))
        inbound
    ; transfer
        c
        ~minute:(start +. 10. +. (Rng.float rng *. 120.))
        ~rail:(if Rng.chance rng 0.5 then Crypto else Us_wire)
        ~country:(Rng.pick rng abroad)
        ?label
        ~counterparty:(sprintf "cp-exit-%02d" (Rng.int rng 40))
        (Float.iround_nearest_exn (Float.of_int inbound *. share))
    ]
  | Mule_fan_in ->
    let payers = if subtle then 4 else Rng.range rng 5 14 in
    let ins =
      List.init payers ~f:(fun i ->
        transfer
          c
          ~minute:(at (day + (i / 4)) (h ()))
          ~direction:Inbound
          ~rail:Sepa_instant
          ?label
          ~counterparty:(sprintf "cp-victim-%05d-%d" c.idx i)
          (Rng.amount rng ~median:700. ~sigma:0.6))
    in
    let total = List.sum (module Int) ins ~f:(fun d -> d.cents) in
    let last_in = (payers - 1) / 4 in
    let collector = sprintf "cp-collector-%02d" (Rng.int rng 12) in
    let out =
      transfer
        c
        ~minute:(at (day + last_in) 21.)
        ~rail:(if Rng.chance rng 0.4 then Crypto else Sepa_instant)
        ~country:(Rng.pick rng [| "LT"; "EE"; "GB"; "AE" |])
        ?label
        ~counterparty:collector
        (total * 85 / 100)
    in
    let cash =
      if Rng.chance rng 0.4
      then [ atm rng c ~minute:(at (day + last_in + 1) (h ())) ?label (total * 10 / 100) ]
      else []
    in
    ins @ (out :: cash)
  | Dormant_reactivation ->
    let inbound = Rng.range rng (if subtle then 200_000 else 600_000) 3_000_000 in
    let start = at day (h ()) in
    [ transfer
        c
        ~minute:start
        ~direction:Inbound
        ?label
        ~counterparty:(sprintf "cp-wake-%05d" c.idx)
        inbound
    ; transfer
        c
        ~minute:(start +. 600.)
        ~rail:Us_wire
        ~country:(Rng.pick rng abroad)
        ?label
        ~counterparty:(sprintf "cp-wake-out-%05d" c.idx)
        (inbound * 9 / 10)
    ]
  | High_risk_corridor ->
    let country = Rng.pick rng [| "IR"; "IR"; "MM"; "KP" |] in
    List.init (Rng.range rng 1 3) ~f:(fun i ->
      transfer
        c
        ~minute:(at (day + (i * 3)) (h ()))
        ~rail:Us_wire
        ~country
        ?label
        ~counterparty:(sprintf "cp-hr-%03d" (Rng.int rng 60))
        (Rng.amount rng ~median:(if subtle then 250. else 2_000.) ~sigma:0.6))
  | Card_testing ->
    let n = if subtle then 3 else Rng.range rng 6 18 in
    let start = at day (h ()) in
    List.init n ~f:(fun i ->
      { (card
           rng
           c
           ~minute:(start +. (Float.of_int i *. 0.5))
           ?label
           ~cents:(Rng.range rng 50 199)
           ~declined:(Rng.chance rng 0.45)
           ())
        with
        channel = Some Online
      ; counterparty = sprintf "cp-merch-%03d" (Rng.int rng merchants)
      ; ip = Some (Rng.pick rng abroad)
      ; device = Some (sprintf "dev-x-%04d" (Rng.int rng 300))
      })
  | Atm_cash ->
    let n = if subtle then 4 else Rng.range rng 7 14 in
    let country = if Rng.chance rng 0.5 then Some (Rng.pick rng abroad) else None in
    List.init n ~f:(fun i ->
      { (atm
           rng
           c
           ~minute:(at day (8. +. Float.of_int i))
           ~country
           ?label
           (10_000 * Rng.range rng 2 5))
        with
        counterparty = sprintf "cp-atm-%03d" (Rng.int rng atms)
      })
  | Sanctions_adjacent ->
    let country = Rng.pick rng [| "RU"; "BY" |] in
    List.init (Rng.range rng 1 3) ~f:(fun i ->
      let d =
        transfer
          c
          ~minute:(at (day + (i * 4)) (h ()))
          ~rail:Us_wire
          ~country
          ?label
          ~counterparty:(sprintf "cp-sa-%03d" (Rng.int rng 40))
          (Rng.amount rng ~median:3_000. ~sigma:0.8)
      in
      if subtle then { d with cp_country = Some "AM"; ip = Some country } else d)
  | Threshold_report | Unusual_amount -> []
;;

let to_event (cfg : Config.t) customers (d : draft) ~id : labelled =
  let c = customers.(d.customer) in
  let at = Time_ns.add cfg.start (Time_ns.Span.of_min d.minute) in
  let amount = Money.Amount.of_minor_units_int d.currency d.cents in
  let customer_id = sprintf "c%05d" c.idx in
  let event : Event.t =
    { id = Ids.Event_id.of_string (sprintf "e%08d" id)
    ; customer = Ids.Customer_id.of_string customer_id
    ; account = Ledger.Account_id.of_string (sprintf "customer:%s/eur" customer_id)
    ; counterparty = Ids.Counterparty_id.of_string d.counterparty
    ; counterparty_country = Option.map d.cp_country ~f:country
    ; rail = d.rail
    ; direction = d.direction
    ; amount
    ; base_amount = amount
    ; occurred_at = at
    ; received_at = Time_ns.add at (Time_ns.Span.of_sec 2.)
    ; mcc = Option.map d.mcc ~f:Risk_rules.Mcc.of_int_exn
    ; card_channel = d.channel
    ; declined = d.declined
    ; device_id = d.device
    ; ip_country = Option.map d.ip ~f:country
    ; residence = Some (country c.residence)
    ; account_opened_at = Some (Time_ns.add cfg.start (Time_ns.Span.of_day c.opened_day))
    ; risk_band = Some c.band
    }
  in
  { event; label = d.label }
;;

let generate (cfg : Config.t) =
  let rng = Random.State.make [| cfg.seed |] in
  let customers = Array.init cfg.customers ~f:(make_customer rng) in
  let drafts =
    Array.to_list customers
    |> List.concat_map ~f:(fun c ->
      let bad = Rng.chance rng cfg.bad_share in
      let typology = Rng.pick rng typologies in
      let subtle = Rng.chance rng 0.33 in
      let day = Rng.range rng 10 (cfg.days - 5) in
      match bad, typology with
      | false, _ ->
        (* benign look-alike: an account silent for months that wakes up with small
           payments *)
        if Rng.chance rng 0.02
        then (
          let wake = Rng.range rng 10 (cfg.days - 5) in
          let old =
            { (transfer
                 c
                 ~minute:(-.Float.of_int (Rng.range rng 200 400) *. 1440.)
                 ~counterparty:c.landlord
                 5_000)
              with
              label = None
            }
          in
          old :: normal_activity rng c ~days:cfg.days ~quiet_until:wake)
        else normal_activity rng c ~days:cfg.days ~quiet_until:0
      | true, Dormant_reactivation ->
        let c = { c with opened_day = -.Float.of_int (Rng.range rng 400 1500) } in
        customers.(c.idx) <- c;
        let old =
          transfer
            c
            ~minute:(-.Float.of_int (Rng.range rng 200 360) *. 1440.)
            ~counterparty:c.landlord
            4_000
        in
        old
        :: (episode rng c Dormant_reactivation ~day ~subtle
            @ normal_activity rng c ~days:cfg.days ~quiet_until:(day + 1))
      | true, Mule_fan_in ->
        (* mules open accounts shortly before they are used *)
        let c = { c with opened_day = Float.of_int (day - Rng.range rng 5 40) } in
        customers.(c.idx) <- c;
        episode rng c Mule_fan_in ~day ~subtle
        @ (normal_activity
             rng
             c
             ~days:cfg.days
             ~quiet_until:(Int.max 0 (Float.to_int c.opened_day))
           |> List.filter ~f:(fun _ -> Rng.chance rng 0.3))
      | true, typology ->
        episode rng c typology ~day ~subtle
        @ normal_activity rng c ~days:cfg.days ~quiet_until:0)
  in
  List.filter drafts ~f:(fun d -> d.cents > 0)
  |> List.stable_sort ~compare:(fun a b -> Float.compare a.minute b.minute)
  |> List.mapi ~f:(fun id d -> to_event cfg customers d ~id)
;;

let to_json_line { event; label } =
  match Event.to_json event with
  | `Assoc fields ->
    Yojson.Safe.to_string
      (`Assoc
        (fields
         @ [ ( "label"
             , Option.value_map label ~default:`Null ~f:(fun t ->
                 `String (Typology.name t)) )
           ]))
  | json -> Yojson.Safe.to_string json
;;

let of_json_line line =
  let open Or_error.Let_syntax in
  let%bind json =
    Or_error.try_with (fun () -> Yojson.Safe.from_string line)
    |> Or_error.tag ~tag:"bad JSON"
  in
  let%bind event = Event.of_json json in
  let%map label =
    match json with
    | `Assoc fields ->
      (match List.Assoc.find fields ~equal:String.equal "label" with
       | None | Some `Null -> Ok None
       | Some (`String s) ->
         (match Typology.of_name s with
          | Some t -> Ok (Some t)
          | None -> Or_error.error_s [%message "unknown label" s])
       | Some _ -> Or_error.error_s [%message "label must be a string"])
    | _ -> Ok None
  in
  { event; label }
;;
