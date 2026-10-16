(* Embeds one file into the library as a string, so the bundled model is available in
   process without a file on disk. Prints [let contents = "..."]. *)

let () =
  let path = Sys.argv.(1) in
  let ic = open_in_bin path in
  let contents = really_input_string ic (in_channel_length ic) in
  close_in ic;
  Printf.printf "let contents = %S\n" contents
;;
