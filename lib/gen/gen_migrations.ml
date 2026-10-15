(* Embeds migrations/<version>_<name>.sql into the binary, so the image needs no
   migrations directory at run time. Prints an OCaml list of (version, name, sql). *)

let () =
  let files =
    Array.to_list Sys.argv
    |> List.tl
    |> List.filter (fun f -> Filename.check_suffix f ".sql")
    |> List.sort compare
  in
  print_string "let all = [\n";
  List.iter
    (fun path ->
      let base = Filename.chop_suffix (Filename.basename path) ".sql" in
      let version, name =
        match String.index_opt base '_' with
        | Some i ->
          ( int_of_string (String.sub base 0 i)
          , String.sub base (i + 1) (String.length base - i - 1) )
        | None -> failwith ("migration file not named <version>_<name>.sql: " ^ path)
      in
      let ic = open_in_bin path in
      let sql = really_input_string ic (in_channel_length ic) in
      close_in ic;
      Printf.printf "  %d, %S, %S;\n" version name sql)
    files;
  print_string "]\n"
;;
