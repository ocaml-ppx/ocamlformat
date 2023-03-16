open! Stdio
open Ocamlformat_lib
module E = Extended_ast

let get_arg () =
  if Array.length Sys.argv < 2 then (
    Printf.eprintf "Not enough argument\n" ;
    exit 2 )
  else Sys.argv.(1)

let () =
  let inputf = get_arg () in
  let syntax = Option.value ~default:Syntax.Use_file (Syntax.of_fname inputf) in
  let E.Any kind = E.of_syntax syntax in
  Printf.printf "Reading %S\n" inputf;
  let content = In_channel.read_all inputf in
  let ast = E.Parse.ast kind ~preserve_beginend:true ~input_name:inputf content in
  E.Printast.ast kind Format.std_formatter ast
