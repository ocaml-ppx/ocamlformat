open! Stdio
open Ocamlformat_lib

let ocaml_version = Ocaml_version.sys_version

let extended_ast ppf syntax ~input_name content =
  let open Extended_ast in
  let (Any kind) = of_syntax syntax in
  Parse.ast kind ~ocaml_version ~preserve_beginend:true ~input_name content
  |> Printast.ast kind ppf

let std_ast ppf syntax ~input_name content =
  let open Std_ast in
  let (Any kind) = of_syntax syntax in
  Parse.ast kind ~ocaml_version ~input_name content |> Printast.ast kind ppf

let get_arg () =
  let std = ref false and input = ref None in
  let opts = [("-std", Arg.Set std, "Use the standard parser")] in
  let usage = "printast [-std] <file>" in
  Arg.parse opts (fun inp -> input := Some inp) usage ;
  let input =
    match !input with
    | Some inp -> inp
    | None ->
        Printf.eprintf "Not enough argument\n" ;
        exit 2
  and parse_and_print = if !std then std_ast else extended_ast in
  (parse_and_print, input)

let () =
  let parse_and_print, inputf = get_arg () in
  let syntax =
    Option.value ~default:Syntax.Use_file (Syntax.of_fname inputf)
  in
  Printf.printf "Reading %S\n" inputf ;
  let content = In_channel.read_all inputf in
  parse_and_print Format.std_formatter syntax ~input_name:inputf content
