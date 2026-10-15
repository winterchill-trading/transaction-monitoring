open! Core

module Status = struct
  module Stable = struct
    module V1 = struct
      type t =
        | Open
        | Assigned
        | Closed
      [@@deriving bin_io, sexp, compare, equal, hash, enumerate]
    end
  end

  include Stable.V1

  let to_string = function
    | Open -> "open"
    | Assigned -> "assigned"
    | Closed -> "closed"
  ;;
end

module Verdict = struct
  module Stable = struct
    module V1 = struct
      type t =
        | True_positive
        | False_positive
      [@@deriving bin_io, sexp, compare, equal, hash, enumerate]
    end
  end

  include Stable.V1
end

module Resolution = struct
  module Stable = struct
    module V1 = struct
      type t =
        { verdict : Verdict.Stable.V1.t
        ; note : string
        ; by : Ids.Analyst_id.Stable.V1.t
        ; at : Time_ns.Stable.Alternate_sexp.V1.t
        }
      [@@deriving bin_io, sexp, compare, equal]
    end
  end

  include Stable.V1
end

module Stable = struct
  module V1 = struct
    type t =
      { id : Ids.Alert_id.Stable.V1.t
      ; event_id : Ids.Event_id.Stable.V1.t
      ; customer : Ids.Customer_id.Stable.V1.t
      ; outcome : Decision.Outcome.Stable.V1.t
      ; explanations : Explanation.Stable.V1.t list
      ; score : float option
      ; opened_at : Time_ns.Stable.Alternate_sexp.V1.t
      ; status : Status.Stable.V1.t
      ; assignee : Ids.Analyst_id.Stable.V1.t option
      ; sar_candidate : bool
      ; resolution : Resolution.Stable.V1.t option
      ; version : int
      ; updated_at : Time_ns.Stable.Alternate_sexp.V1.t
      }
    [@@deriving bin_io, sexp, compare, equal]
  end
end

include Stable.V1

module Event = struct
  module Stable = struct
    module V1 = struct
      type t =
        | Opened of { at : Time_ns.Stable.Alternate_sexp.V1.t }
        | Assigned of
            { analyst : Ids.Analyst_id.Stable.V1.t
            ; previous : Ids.Analyst_id.Stable.V1.t option
            ; at : Time_ns.Stable.Alternate_sexp.V1.t
            }
        | Sar_flagged of
            { by : Ids.Analyst_id.Stable.V1.t
            ; reason : string
            ; at : Time_ns.Stable.Alternate_sexp.V1.t
            }
        | Sar_unflagged of
            { by : Ids.Analyst_id.Stable.V1.t
            ; reason : string
            ; at : Time_ns.Stable.Alternate_sexp.V1.t
            }
        | Closed of Resolution.Stable.V1.t
        | Reopened of
            { by : Ids.Analyst_id.Stable.V1.t
            ; reason : string
            ; at : Time_ns.Stable.Alternate_sexp.V1.t
            }
      [@@deriving bin_io, sexp, compare, equal]
    end
  end

  include Stable.V1
end

module Command = struct
  type t =
    | Assign of
        { analyst : Ids.Analyst_id.t
        ; at : Time_ns.t
        }
    | Flag_sar of
        { by : Ids.Analyst_id.t
        ; reason : string
        ; at : Time_ns.t
        }
    | Unflag_sar of
        { by : Ids.Analyst_id.t
        ; reason : string
        ; at : Time_ns.t
        }
    | Close of
        { by : Ids.Analyst_id.t
        ; verdict : Verdict.t
        ; note : string
        ; at : Time_ns.t
        }
    | Reopen of
        { by : Ids.Analyst_id.t
        ; reason : string
        ; at : Time_ns.t
        }
  [@@deriving sexp_of]
end

let id_for event_id = Ids.Alert_id.of_string ("alert-" ^ Ids.Event_id.to_string event_id)

let open_ (d : Decision.t) =
  match d.outcome with
  | Allow -> None
  | Hold | Block ->
    Some
      ( { id = id_for d.event_id
        ; event_id = d.event_id
        ; customer = d.customer
        ; outcome = d.outcome
        ; explanations = d.explanations
        ; score = d.score
        ; opened_at = d.decided_at
        ; status = Open
        ; assignee = None
        ; sar_candidate = false
        ; resolution = None
        ; version = 1
        ; updated_at = d.decided_at
        }
      , [ Event.Opened { at = d.decided_at } ] )
;;

let refuse t message =
  Or_error.error_s
    [%message
      message ~alert:(t.id : Ids.Alert_id.t) ~status:(Status.to_string t.status : string)]
;;

let is_assignee t analyst = Option.equal Ids.Analyst_id.equal t.assignee (Some analyst)
let blank s = String.is_empty (String.strip s)

let step t ~at (event : Event.t) f =
  Ok ({ (f t) with version = t.version + 1; updated_at = at }, [ event ])
;;

let apply t (command : Command.t) =
  match command with
  | Assign { analyst; at } ->
    (match t.status with
     | Closed -> refuse t "a closed alert cannot be assigned; reopen it first"
     | Open | Assigned ->
       if is_assignee t analyst
       then Ok (t, [])
       else
         step
           t
           ~at
           (Assigned { analyst; previous = t.assignee; at })
           (fun t -> { t with status = Assigned; assignee = Some analyst }))
  | Flag_sar { by; reason; at } ->
    (match t.status with
     | Open | Closed -> refuse t "only an assigned alert can be flagged"
     | Assigned ->
       if not (is_assignee t by)
       then refuse t "only the assignee can flag a SAR candidate"
       else if blank reason
       then refuse t "a SAR candidate flag needs a reason"
       else if t.sar_candidate
       then Ok (t, [])
       else
         step
           t
           ~at
           (Sar_flagged { by; reason; at })
           (fun t -> { t with sar_candidate = true }))
  | Unflag_sar { by; reason; at } ->
    (match t.status with
     | Open | Closed -> refuse t "only an assigned alert can be unflagged"
     | Assigned ->
       if not (is_assignee t by)
       then refuse t "only the assignee can withdraw a SAR candidate flag"
       else if blank reason
       then refuse t "withdrawing a SAR candidate flag needs a reason"
       else if not t.sar_candidate
       then Ok (t, [])
       else
         step
           t
           ~at
           (Sar_unflagged { by; reason; at })
           (fun t -> { t with sar_candidate = false }))
  | Close { by; verdict; note; at } ->
    (match t.status with
     | Open -> refuse t "assign the alert before closing it"
     | Closed -> refuse t "already closed"
     | Assigned ->
       if not (is_assignee t by)
       then refuse t "only the assignee can close the alert"
       else if blank note
       then refuse t "closing needs a note"
       else (
         match verdict with
         | False_positive when t.sar_candidate ->
           refuse t "a SAR candidate cannot be closed as a false positive"
         | True_positive | False_positive ->
           let resolution = { Resolution.verdict; note; by; at } in
           step t ~at (Closed resolution) (fun t ->
             { t with status = Closed; resolution = Some resolution })))
  | Reopen { by; reason; at } ->
    (match t.status, t.resolution with
     | (Open | Assigned), _ -> refuse t "only a closed alert can be reopened"
     | Closed, Some r when Ids.Analyst_id.equal r.by by ->
       refuse t "the analyst who closed the alert cannot reopen it"
     | Closed, _ when blank reason -> refuse t "reopening needs a reason"
     | Closed, _ ->
       step
         t
         ~at
         (Reopened { by; reason; at })
         (fun t -> { t with status = Assigned; assignee = Some by; resolution = None }))
;;
