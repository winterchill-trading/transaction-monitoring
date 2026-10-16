open! Core
open! Async
open Transaction_monitoring

(* The consumer case: an application with its own migrations, transaction-monitoring and
   sanctions-screening in one database. Each library migrates inside its own schema, all
   three start at version 1, and nothing clashes, in either order and on a restart. *)
let%expect_test "two libraries and the host migrate into one database" =
  let open Pg_async in
  let module Sanctions = Sanctions_screening_pg.Store_pg in
  let%bind url = Test_db.fresh_url () in
  let config = Config.of_url url |> Or_error.ok_exn in
  let%bind conn = Connection.connect config >>| Pg_error.ok_exn in
  let%bind host =
    Migrate.apply
      conn
      [ { version = 1; name = "init"; sql = "CREATE TABLE accounts (id text)" } ]
    >>| Result.map_error ~f:Migrate.Error.to_error
    >>| Or_error.ok_exn
  in
  let%bind () = Connection.close conn in
  let%bind sanctions = Sanctions.connect ~url () >>| Or_error.ok_exn in
  let%bind monitoring = Store_pg.connect ~database_url:url () >>| Or_error.ok_exn in
  let%bind () = Sanctions.close sanctions in
  let%bind () = Store_pg.close monitoring in
  (* Restart in the other order: nothing left to apply. *)
  let%bind monitoring' = Store_pg.connect ~database_url:url () >>| Or_error.ok_exn in
  let%bind sanctions' = Sanctions.connect ~url () >>| Or_error.ok_exn in
  let pool = Pool.create ~max_size:1 config in
  let%bind rows =
    Pool.fetch_all
      pool
      "SELECT 'public' AS s, version, name FROM public.schema_migrations UNION ALL \
       SELECT 'sanctions_screening', version, name FROM \
       sanctions_screening.schema_migrations UNION ALL SELECT 'transaction_monitoring', \
       version, name FROM transaction_monitoring.schema_migrations ORDER BY 1, 2"
      ~row:
        Row.Let_syntax.(
          let%map_open s = column "s" string
          and v = column "version" int
          and n = column "name" string in
          s, v, n)
    >>| Pg_error.to_or_error
    >>| Or_error.ok_exn
  in
  print_s
    [%message
      ""
        (host : int list)
        ~sanctions:(Sanctions.applied_migrations sanctions : int list)
        ~monitoring:(Store_pg.applied_migrations monitoring : int list)
        ~after_restart:
          (( Store_pg.applied_migrations monitoring'
           , Sanctions.applied_migrations sanctions' )
           : int list * int list)
        (rows : (string * int * string) list)];
  let%bind () = Pool.close pool in
  let%bind () = Sanctions.close sanctions' in
  let%map () = Store_pg.close monitoring' in
  [%expect
    {|
    ((host (1)) (sanctions (1)) (monitoring (1 2 3)) (after_restart (() ()))
     (rows
      ((public 1 init) (sanctions_screening 1 init)
       (transaction_monitoring 1 init) (transaction_monitoring 2 explanations)
       (transaction_monitoring 3 revocations))))
    |}]
;;
