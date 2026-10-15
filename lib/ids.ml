open! Core

module Customer_id =
  Ledger.Validated_id.Make
    (struct
      let module_name = "Transaction_monitoring.Customer_id"
    end)
    ()

module Event_id =
  Ledger.Validated_id.Make
    (struct
      let module_name = "Transaction_monitoring.Event_id"
    end)
    ()

module Counterparty_id =
  Ledger.Validated_id.Make
    (struct
      let module_name = "Transaction_monitoring.Counterparty_id"
    end)
    ()

module Alert_id =
  Ledger.Validated_id.Make
    (struct
      let module_name = "Transaction_monitoring.Alert_id"
    end)
    ()

module Analyst_id =
  Ledger.Validated_id.Make
    (struct
      let module_name = "Transaction_monitoring.Analyst_id"
    end)
    ()
