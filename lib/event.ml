open! Core

(* A small helper for the enumerations below: one table gives both directions of the
   text form, so [of_string] can never disagree with [to_string]. *)
module Text_enum (T : sig
    type t [@@deriving enumerate]

    val name : string
    val to_string : t -> string
  end) =
struct
  let table = List.map T.all ~f:(fun x -> T.to_string x, x)

  let of_string s =
    match List.Assoc.find table ~equal:String.equal (String.lowercase s) with
    | Some x -> Ok x
    | None -> Or_error.error_s [%message "unknown value" ~kind:T.name s]
  ;;
end

module Rail = struct
  module Stable = struct
    module V1 = struct
      type t =
        | Sepa
        | Sepa_instant
        | Uk_faster_payments
        | Uk_chaps
        | Us_ach
        | Us_wire
        | Card
        | Intraledger
        | Crypto
      [@@deriving bin_io, sexp, compare, equal, hash, enumerate, quickcheck]
    end
  end

  include Stable.V1

  let to_string = function
    | Sepa -> "sepa"
    | Sepa_instant -> "sepa_instant"
    | Uk_faster_payments -> "uk_faster_payments"
    | Uk_chaps -> "uk_chaps"
    | Us_ach -> "us_ach"
    | Us_wire -> "us_wire"
    | Card -> "card"
    | Intraledger -> "intraledger"
    | Crypto -> "crypto"
  ;;

  include Text_enum (struct
      include Stable.V1

      let name = "rail"
      let to_string = to_string
    end)
end

module Direction = struct
  module Stable = struct
    module V1 = struct
      type t =
        | Inbound
        | Outbound
      [@@deriving bin_io, sexp, compare, equal, hash, enumerate, quickcheck]
    end
  end

  include Stable.V1

  let to_string = function
    | Inbound -> "in"
    | Outbound -> "out"
  ;;

  include Text_enum (struct
      include Stable.V1

      let name = "direction"
      let to_string = to_string
    end)
end

module Card_channel = struct
  module Stable = struct
    module V1 = struct
      type t =
        | In_person
        | Online
        | Atm
      [@@deriving bin_io, sexp, compare, equal, hash, enumerate, quickcheck]
    end
  end

  include Stable.V1

  let to_string = function
    | In_person -> "in_person"
    | Online -> "online"
    | Atm -> "atm"
  ;;

  include Text_enum (struct
      include Stable.V1

      let name = "card_channel"
      let to_string = to_string
    end)
end

module Risk_band = struct
  module Stable = struct
    module V1 = struct
      type t =
        | Low
        | Medium
        | High
      [@@deriving bin_io, sexp, compare, equal, hash, enumerate, quickcheck]
    end
  end

  include Stable.V1

  let to_string = function
    | Low -> "low"
    | Medium -> "medium"
    | High -> "high"
  ;;

  include Text_enum (struct
      include Stable.V1

      let name = "risk_band"
      let to_string = to_string
    end)
end

module Stable = struct
  module V1 = struct
    type t =
      { id : Ids.Event_id.Stable.V1.t
      ; customer : Ids.Customer_id.Stable.V1.t
      ; account : Ledger.Account_id.Stable.V1.t
      ; counterparty : Ids.Counterparty_id.Stable.V1.t
      ; counterparty_country : Risk_rules.Country.Stable.V1.t option
      ; rail : Rail.Stable.V1.t
      ; direction : Direction.Stable.V1.t
      ; amount : Money.Amount.Stable.V1.t
      ; base_amount : Money.Amount.Stable.V1.t
      ; occurred_at : Time_ns.Stable.Alternate_sexp.V1.t
      ; received_at : Time_ns.Stable.Alternate_sexp.V1.t
      ; mcc : Risk_rules.Mcc.Stable.V1.t option
      ; card_channel : Card_channel.Stable.V1.t option
      ; declined : bool
      ; device_id : string option
      ; ip_country : Risk_rules.Country.Stable.V1.t option
      ; residence : Risk_rules.Country.Stable.V1.t option
      ; account_opened_at : Time_ns.Stable.Alternate_sexp.V1.t option
      ; risk_band : Risk_band.Stable.V1.t option
      }
    [@@deriving bin_io, sexp, compare, equal]
  end
end

include Stable.V1

let base_minor_or_error t =
  match Money.Amount.minor_units_int t.base_amount with
  | Some n when n >= 0 -> Ok n
  | Some _ ->
    Or_error.error_s [%message "negative base amount" ~event:(t.id : Ids.Event_id.t)]
  | None ->
    Or_error.error_s
      [%message "base amount does not fit in an int" ~event:(t.id : Ids.Event_id.t)]
;;

let base_minor t = Or_error.ok_exn (base_minor_or_error t)

let is_cash t =
  match t.card_channel with
  | Some Atm -> true
  | Some (In_person | Online) | None -> false
;;

let validate t =
  let open Or_error.Let_syntax in
  let%bind (_ : int) = base_minor_or_error t in
  match t.rail, t.card_channel with
  | Card, _ | _, None -> Ok t
  | _, Some _ ->
    Or_error.error_s
      [%message "card channel on a non-card event" ~event:(t.id : Ids.Event_id.t)]
;;

(* JSON *)

let time_to_string time = Time_ns.to_string_iso8601_basic time ~zone:Time_float.Zone.utc

let opt f = function
  | None -> `Null
  | Some x -> f x
;;

let str s = `String s

let to_json t : Yojson.Safe.t =
  `Assoc
    [ "id", str (Ids.Event_id.to_string t.id)
    ; "customer", str (Ids.Customer_id.to_string t.customer)
    ; "account", str (Ledger.Account_id.to_string t.account)
    ; "counterparty", str (Ids.Counterparty_id.to_string t.counterparty)
    ; ( "counterparty_country"
      , opt (fun c -> str (Risk_rules.Country.to_string c)) t.counterparty_country )
    ; "rail", str (Rail.to_string t.rail)
    ; "direction", str (Direction.to_string t.direction)
    ; "amount", str (Money.Amount.to_string t.amount)
    ; "base_amount", str (Money.Amount.to_string t.base_amount)
    ; "occurred_at", str (time_to_string t.occurred_at)
    ; "received_at", str (time_to_string t.received_at)
    ; "mcc", opt (fun m -> `Int (Risk_rules.Mcc.to_int m)) t.mcc
    ; "card_channel", opt (fun c -> str (Card_channel.to_string c)) t.card_channel
    ; "declined", `Bool t.declined
    ; "device_id", opt str t.device_id
    ; "ip_country", opt (fun c -> str (Risk_rules.Country.to_string c)) t.ip_country
    ; "residence", opt (fun c -> str (Risk_rules.Country.to_string c)) t.residence
    ; "account_opened_at", opt (fun x -> str (time_to_string x)) t.account_opened_at
    ; "risk_band", opt (fun b -> str (Risk_band.to_string b)) t.risk_band
    ]
;;

let field json name =
  match json with
  | `Assoc fields ->
    (match List.Assoc.find fields ~equal:String.equal name with
     | None | Some `Null -> None
     | Some v -> Some v)
  | _ -> None
;;

let required json name f =
  match field json name with
  | None -> Or_error.error_s [%message "missing field" name]
  | Some v -> Or_error.tag_s (f v) ~tag:[%message "in field" name]
;;

let optional json name f =
  match field json name with
  | None -> Ok None
  | Some v -> Or_error.tag_s (f v) ~tag:[%message "in field" name] |> Or_error.map ~f:Option.some
;;

let string_of = function
  | `String s -> Ok s
  | j -> Or_error.error_s [%message "expected a string" ~got:(Yojson.Safe.to_string j)]
;;

let parse f j = Or_error.bind (string_of j) ~f:(fun s -> Or_error.try_with (fun () -> f s))
let parse_or_error f j = Or_error.bind (string_of j) ~f

(* RFC 3339 with a zone ("2026-01-01T10:00:00Z", "+00:00") or a bare UTC time. *)
let time_of_string s =
  Or_error.try_with (fun () -> Time_ns.of_string_with_utc_offset s)
  |> Or_error.tag_s ~tag:[%message "bad time" s]
;;

let of_json json =
  let open Or_error.Let_syntax in
  let%bind id = required json "id" (parse Ids.Event_id.of_string) in
  (let%bind customer = required json "customer" (parse Ids.Customer_id.of_string) in
   let%bind account = required json "account" (parse Ledger.Account_id.of_string) in
   let%bind counterparty =
     required json "counterparty" (parse Ids.Counterparty_id.of_string)
   in
   let%bind counterparty_country =
     optional json "counterparty_country" (parse Risk_rules.Country.of_string)
   in
   let%bind rail = required json "rail" (parse_or_error Rail.of_string) in
   let%bind direction = required json "direction" (parse_or_error Direction.of_string) in
   let%bind amount =
     required json "amount" (parse_or_error Money.Amount.of_string_or_error)
   in
   let%bind base_amount =
     required json "base_amount" (parse_or_error Money.Amount.of_string_or_error)
   in
   let%bind occurred_at = required json "occurred_at" (parse_or_error time_of_string) in
   let%bind received_at =
     match field json "received_at" with
     | None -> Ok occurred_at
     | Some j -> parse_or_error time_of_string j
   in
   let%bind mcc =
     optional json "mcc" (function
       | `Int n -> Risk_rules.Mcc.of_int_or_error n
       | j -> Or_error.error_s [%message "expected an int" ~got:(Yojson.Safe.to_string j)])
   in
   let%bind card_channel =
     optional json "card_channel" (parse_or_error Card_channel.of_string)
   in
   let%bind declined =
     optional json "declined" (function
       | `Bool b -> Ok b
       | j -> Or_error.error_s [%message "expected a bool" ~got:(Yojson.Safe.to_string j)])
   in
   let%bind device_id = optional json "device_id" string_of in
   let%bind ip_country = optional json "ip_country" (parse Risk_rules.Country.of_string) in
   let%bind residence = optional json "residence" (parse Risk_rules.Country.of_string) in
   let%bind account_opened_at =
     optional json "account_opened_at" (parse_or_error time_of_string)
   in
   let%bind risk_band = optional json "risk_band" (parse_or_error Risk_band.of_string) in
   validate
     { id
     ; customer
     ; account
     ; counterparty
     ; counterparty_country
     ; rail
     ; direction
     ; amount
     ; base_amount
     ; occurred_at
     ; received_at
     ; mcc
     ; card_channel
     ; declined = Option.value declined ~default:false
     ; device_id
     ; ip_country
     ; residence
     ; account_opened_at
     ; risk_band
     })
  |> Or_error.tag_s ~tag:[%message "event" ~_:(id : Ids.Event_id.t)]
;;

let to_json_line t = Yojson.Safe.to_string (to_json t)

let of_json_line line =
  match Yojson.Safe.from_string line with
  | json -> of_json json
  | exception Yojson.Json_error msg -> Or_error.error_s [%message "bad JSON" msg]
;;
