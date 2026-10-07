open! Stdio
open Ocamlformat_lib

let () =
  let std = ref false and input = ref None in
  let opts = [("-std", Arg.Set std, "Use the standard parser")] in
  let usage = "printast [-std] <file>" in
  Arg.parse opts (fun inp -> input := Some inp) usage ;
  let inputf =
    match !input with
    | Some inp -> inp
    | None ->
        Printf.eprintf "Not enough argument\n" ;
        exit 2
  in
  let syntax =
    Option.value ~default:Syntax.Use_file (Syntax.of_fname inputf)
  in
  Printf.printf "Reading %S\n" inputf ;
  let content = In_channel.read_all inputf in
  let conf = Conf.default in
  let (Extended_ast.Kind.Any kind) = Extended_ast.Kind.of_syntax syntax in
  let parsed =
    Extended_ast.parse kind conf ~input_name:inputf ~source:content
  in
  if !std then (
    match Extended_ast.get_std parsed with
    | Some (Std_value (std_fg, std_v)) ->
        Std_ast.Printast.ast std_fg Format.std_formatter std_v
    | None ->
        Printf.eprintf "No standard parser for this fragment type\n" ;
        exit 1 )
  else Extended_ast.Printast.ast Format.std_formatter parsed
