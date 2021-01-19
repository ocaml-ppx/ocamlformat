let test_split_use_file =
  let make_test name ~input ~expected =
    let test_name = "Split.use_file: " ^ name in
    let test_fun () =
      let actual =
        Ocamlformat_lib.Split_parser.Split.fragment Use_file input
      in
      Alcotest.(check (list string)) test_name expected actual
    in
    (test_name, `Quick, test_fun)
  in
  [ make_test "empty" ~input:"" ~expected:[]
  ; make_test "multi empty" ~input:"\n\n" ~expected:[]
  ; make_test "invalid let" ~input:"let x" ~expected:["let x"]
  ; make_test "valid let" ~input:"let x = 2" ~expected:["let x = 2"]
  ; make_test "2 let 1l split" ~input:"let x = x\nlet y = y"
      ~expected:["let x = x"; "let y = y"]
  ; make_test "2 let 2l split" ~input:"let x = x\n\nlet y = y"
      ~expected:["let x = x"; "let y = y"]
  ; make_test "2 let mix split" ~input:"let x =\n\nx\nlet y = y"
      ~expected:["let x =\n\nx"; "let y = y"] ]

let test_split_interface =
  let make_test name ~input ~expected =
    let test_name = "Split.interface: " ^ name in
    let test_fun () =
      let actual =
        Ocamlformat_lib.Split_parser.Split.fragment Signature input
      in
      Alcotest.(check (list string)) test_name expected actual
    in
    (test_name, `Quick, test_fun)
  in
  [ make_test "empty" ~input:"" ~expected:[]
  ; make_test "multi empty" ~input:"\n\n" ~expected:[] ]

let lex =
  let open Ocamlformat_lib.Split_parser.Cmt_lexer in
  let pp fs = function
    | Cmt x -> Fmt.pf fs "Cmt %s" x
    | Other x -> Fmt.pf fs "Other %s" x
  in
  Alcotest.testable pp ( = )

let test_lex_comments =
  let make_test name ~input ~expected =
    let test_name = "Cmt_lexer.lex_comments: " ^ name in
    let test_fun () =
      let actual =
        Ocamlformat_lib.Split_parser.Cmt_lexer.lex_comments input
      in
      Alcotest.(check (list lex)) test_name expected actual
    in
    (test_name, `Quick, test_fun)
  in
  [ make_test "empty" ~input:"" ~expected:[]
  ; make_test "cmt before" ~input:"(* fooooooooooooo *)\nbar\n"
      ~expected:[Cmt "(* fooooooooooooo *)"; Other "\nbar\n"]
  ; make_test "cmt multi before" ~input:"(* foooooo\n fooooooooooooo *)\nbar"
      ~expected:[Cmt "(* foooooo\n fooooooooooooo *)"; Other "\nbar"]
  ; make_test "cmt after" ~input:"foo\n(* bar *)"
      ~expected:[Other "foo\n"; Cmt "(* bar *)"]
  ; make_test "cmt multi after" ~input:"fooooo\n(* baaaaaar\n  baaaaaar *)"
      ~expected:[Other "fooooo\n"; Cmt "(* baaaaaar\n  baaaaaar *)"]
  ; make_test "2 cmts" ~input:"(* foo *)(* bar *)"
      ~expected:[Cmt "(* foo *)"; Cmt "(* bar *)"]
  ; make_test "2 cmts break" ~input:"(* foo *)\n(* bar *)"
      ~expected:[Cmt "(* foo *)"; Other "\n"; Cmt "(* bar *)"] ]

let test_split_on_linebreaks =
  let make_test name ~input ~expected =
    let test_name = "Split.split_on_linebreaks: " ^ name in
    let test_fun () =
      let actual =
        Ocamlformat_lib.Split_parser.Split.split_on_linebreaks input
      in
      Alcotest.(check (list string)) test_name expected actual
    in
    (test_name, `Quick, test_fun)
  in
  [ make_test "empty" ~input:[] ~expected:[]
  ; make_test "cmt before"
      ~input:[Cmt "(* fooooooooooooo *)"; Other "\nbar\n"]
      ~expected:["(* fooooooooooooo *)"; "bar"; ""]
  ; make_test "cmt after"
      ~input:[Other "foo\n"; Cmt "(* bar *)"]
      ~expected:["foo"; "(* bar *)"]
  ; make_test "2 cmts"
      ~input:[Cmt "(* foo *)"; Cmt "(* bar *)"]
      ~expected:["(* foo *)(* bar *)"]
  ; make_test "2 cmts break"
      ~input:[Cmt "(* foo *)"; Other "\n"; Cmt "(* bar *)"]
      ~expected:["(* foo *)"; "(* bar *)"] ]

let tests =
  test_split_use_file @ test_split_interface @ test_lex_comments
  @ test_split_on_linebreaks
