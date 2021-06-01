open Ocamlformat_lib

let read_file f = Stdio.In_channel.with_file f ~f:Stdio.In_channel.input_all

let partial_let = read_file "../passing/tests/partial.ml"

module Partial_ast = struct
  let tests_indent_range =
    let test name ~input:source ~range ~expected =
      let test_name = "Partial_ast.indent_range: " ^ name in
      ( test_name
      , `Quick
      , fun () ->
          let indent = Indent.Partial_ast.indent_range ~source ~range in
          let output =
            Test_translation_unit.reindent ~source ~range indent
          in
          Alcotest.(check string) test_name expected output )
    in
    [ test "empty" ~input:"" ~range:(1, 1) ~expected:""
    ; test "multiline let" ~range:(1, 5) ~input:{|let f =
let x =
y
in
2|}
        ~expected:{|let f =
  let x =
    y
  in
  2|}
    ; test "after in" ~range:(1, 12)
        ~input:
          {|let f =
let x =
   let y =
     foooooooooooooooooooo
    in
   let x = fooooooooooooo in
  let k =
           fooooooo
in
   foooooooooooooooooooooooooo
        foooooooooooooooooooo foooooooooooooooo fooooooooo
in
|}
        ~expected:
          {|let f =
  let x =
    let y =
      foooooooooooooooooooo
    in
    let x = fooooooooooooo in
    let k =
      fooooooo
    in
    foooooooooooooooooooooooooo
      foooooooooooooooooooo foooooooooooooooo fooooooooo
  in|}
    ; test "partial let" ~range:(2, 14) ~input:partial_let
        ~expected:
          {|   let () =
     ffff;
     hhhhhh;
     fff;
     let (quot, _rem) =
       let quot_rem n k =
         let (d, m) = (n / k, n mod k) in
         if d < 0 && m > 0 then (d+1, m-k)
         else (d, m)
       in
       let quot n k = fst (quot_rem n k) in
       let rem n k = snd (quot_rem n k) in

       quot, rem|}
    ]

  let tests = tests_indent_range
end

let tests = Partial_ast.tests
