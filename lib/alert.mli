(** An alert opened by a [Hold] or [Block] decision, and its case lifecycle, as a pure
    state machine: every command returns the new alert and the events it caused, or an
    error and nothing changes.

    {v
      Open --assign--> Assigned --close--> Closed
                        |  ^                  |
                        |  +---reassign       +--reopen (another analyst)--> Assigned
                        +--flag/unflag SAR candidate
    v}

    Rules: only the assignee flags, unflags or closes; a SAR candidate cannot be closed as
    a false positive until the flag is withdrawn with a reason; a closed alert is reopened
    only by an analyst other than the one who closed it (four eyes). Repeating a command
    that changes nothing (assigning the current assignee) returns no events. Alerts are
    never shown to the customer. *)

open! Core

module Status : sig
  module Stable : sig
    module V1 : sig
      type t =
        | Open
        | Assigned
        | Closed
      [@@deriving bin_io, sexp, compare, equal, hash]
    end
  end

  type t = Stable.V1.t =
    | Open
    | Assigned
    | Closed
  [@@deriving sexp, compare, equal, hash, enumerate]

  val to_string : t -> string
end

module Verdict : sig
  module Stable : sig
    module V1 : sig
      type t =
        | True_positive
        | False_positive
      [@@deriving bin_io, sexp, compare, equal, hash]
    end
  end

  type t = Stable.V1.t =
    | True_positive
    | False_positive
  [@@deriving sexp, compare, equal, hash, enumerate]
end

module Resolution : sig
  module Stable : sig
    module V1 : sig
      type t =
        { verdict : Verdict.Stable.V1.t
        ; note : string
        ; by : Ids.Analyst_id.Stable.V1.t
        ; at : Time_ns.Stable.Alternate_sexp.V1.t
        }
      [@@deriving bin_io, sexp, compare, equal]
    end
  end

  type t = Stable.V1.t =
    { verdict : Verdict.t
    ; note : string
    ; by : Ids.Analyst_id.t
    ; at : Time_ns.t
    }
  [@@deriving sexp_of, compare, equal]
end

module Stable : sig
  module V1 : sig
    type t =
      { id : Ids.Alert_id.Stable.V1.t
      ; event_id : Ids.Event_id.Stable.V1.t
      ; customer : Ids.Customer_id.Stable.V1.t
      ; outcome : Decision.Outcome.Stable.V1.t (** of the decision that opened it *)
      ; explanations : Explanation.Stable.V1.t list
      ; score : float option
      ; opened_at : Time_ns.Stable.Alternate_sexp.V1.t
      ; status : Status.Stable.V1.t
      ; assignee : Ids.Analyst_id.Stable.V1.t option
      ; sar_candidate : bool
      ; resolution : Resolution.Stable.V1.t option
      ; version : int (** events applied so far; for optimistic concurrency in stores *)
      ; updated_at : Time_ns.Stable.Alternate_sexp.V1.t
      }
    [@@deriving bin_io, sexp, compare, equal]
  end

  (** V1 plus the model's per-feature explanation copied from the decision. *)
  module V2 : sig
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
      ; attribution : Risk_models.Explanation.Stable.V1.t option
      }
    [@@deriving bin_io, sexp, compare, equal]

    val of_v1 : V1.t -> t
  end
end

type t = Stable.V2.t =
  { id : Ids.Alert_id.t
  ; event_id : Ids.Event_id.t
  ; customer : Ids.Customer_id.t
  ; outcome : Decision.Outcome.t
  ; explanations : Explanation.t list
  ; score : float option
  ; opened_at : Time_ns.t
  ; status : Status.t
  ; assignee : Ids.Analyst_id.t option
  ; sar_candidate : bool
  ; resolution : Resolution.t option
  ; version : int
  ; updated_at : Time_ns.t
  ; attribution : Risk_models.Explanation.t option
  (** the decision's top feature contributions, for the reviewer *)
  }
[@@deriving sexp_of, compare, equal]

module Event : sig
  module Stable : sig
    module V1 : sig
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

  type t = Stable.V1.t =
    | Opened of { at : Time_ns.t }
    | Assigned of
        { analyst : Ids.Analyst_id.t
        ; previous : Ids.Analyst_id.t option
        ; at : Time_ns.t
        }
    | Sar_flagged of
        { by : Ids.Analyst_id.t
        ; reason : string
        ; at : Time_ns.t
        }
    | Sar_unflagged of
        { by : Ids.Analyst_id.t
        ; reason : string
        ; at : Time_ns.t
        }
    | Closed of Resolution.t
    | Reopened of
        { by : Ids.Analyst_id.t
        ; reason : string
        ; at : Time_ns.t
        }
  [@@deriving sexp_of, compare, equal]
end

module Command : sig
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

(** The alert id for a decision: ["alert-" ^ event id], so a redelivered event can never
    open a second alert. *)
val id_for : Ids.Event_id.t -> Ids.Alert_id.t

(** An [Open] alert for a [Hold] or [Block] decision; [None] for [Allow]. *)
val open_ : Decision.t -> (t * Event.t list) option

val apply : t -> Command.t -> (t * Event.t list) Or_error.t
