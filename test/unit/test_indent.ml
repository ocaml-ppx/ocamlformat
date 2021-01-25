open Base
open Ocamlformat_lib

module Partial_ast = struct
  let get_indent line = String.(length line - length (lstrip line))

  let get_inputs input =
    let lines = String.split_lines input in
    let nb_lines = List.length lines in
    match List.last lines with
    | Some last -> (Some (get_indent last, last), nb_lines, "", nb_lines)
    | None -> (None, 0, "", nb_lines)

  let prepare_output input indent =
    let spaces = String.make indent ' ' in
    input ^ "\n" ^ spaces ^ "^"

  let tests_indent_line =
    let test name ~input ~expected =
      let test_name = "Partial_ast.indent_line: " ^ name in
      ( test_name
      , `Quick
      , fun () ->
          let prev, i, line, nb_lines = get_inputs input in
          let indent =
            Indent.Partial_ast.indent_line ?prev ~i ~line nb_lines
          in
          let output = prepare_output input indent in
          Alcotest.(check string) test_name expected output )
    in
    [ test "" ~input:{|
let foo () =
  if bar then|}
        ~expected:{|
let foo () =
  if bar then
    ^|} ]

  let tests = tests_indent_line
end

let tests = Partial_ast.tests
