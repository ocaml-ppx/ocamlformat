open Parser_extended

let get_arg () =
  if Array.length Sys.argv < 2 then (
    Printf.eprintf "Not enough argument\n" ;
    exit 2 )
  else Sys.argv.(1)

let parse_and_print parsef printf lexbuf =
  printf Format.std_formatter (parsef lexbuf)

let get_ppf fname =
  match Filename.extension fname with
  | ".mli" ->
      (parse_and_print Parse.interface Printast.interface, "interface")
  | _ ->
      ( parse_and_print Parse.implementation Printast.implementation
      , "implementation" )

let () =
  let inputf = get_arg () in
  let ppf, parser_name = get_ppf inputf in
  Printf.printf "Reading %S as %s\n" inputf parser_name ;
  In_channel.with_open_text inputf (fun ic ->
      let lexbuf = Lexing.from_channel ic in
      ppf lexbuf )
