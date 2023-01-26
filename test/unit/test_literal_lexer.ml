open Base
open Ocamlformat_lib

let single_quote = '\''

let double_quote = '"'

let backslash = '\\'

let space = ' '

let tab = '\t'

let newline = '\n'

let tests_string =
  let test_opt name s mode ~expected =
    ( "string: " ^ name
    , `Quick
    , fun () ->
        let got = Literal_lexer.string mode s in
        Alcotest.check Alcotest.(option string) Caml.__LOC__ expected got )
  in
  let test_one name s mode ~expected =
    test_opt name s mode ~expected:(Some expected)
  in
  let test name s ~expected_preserve ~expected_normalize =
    [ test_one (name ^ " (preserve)") s `Preserve ~expected:expected_preserve
    ; test_one (name ^ " (normalize)") s `Normalize
        ~expected:expected_normalize ]
  in
  List.concat
    [ [test_opt "string: not a string" {|hello|} `Preserve ~expected:None]
    ; test "simple" {|"hello"|} ~expected_preserve:"hello"
        ~expected_normalize:"hello"
    ; test "numeric escapes" {|"\123 \xff \o234"|}
        ~expected_preserve:{|\123 \xff \o234|}
        ~expected_normalize:{|\123 \xff \o234|}
    ; test "raw tab"
        (String.of_char_list [double_quote; tab; double_quote])
        ~expected_preserve:(String.of_char_list [tab])
        ~expected_normalize:(String.of_char_list [backslash; 't'])
    ; test "backslash space"
        (String.of_char_list [double_quote; backslash; space; double_quote])
        ~expected_preserve:(String.of_char_list [backslash; space])
        ~expected_normalize:(String.of_char_list [space])
    ; test "backslash n"
        (String.of_char_list [double_quote; backslash; 'n'; double_quote])
        ~expected_preserve:(String.of_char_list [backslash; 'n'])
        ~expected_normalize:(String.of_char_list [newline]) ]

let tests_char =
  let test_opt name s ~expected =
    ( "char: " ^ name
    , `Quick
    , fun () ->
        let got = Literal_lexer.char s in
        Alcotest.check
          (Alcotest.option Alcotest.string)
          Caml.__LOC__ expected got )
  in
  let test name s ~expected = test_opt name s ~expected:(Some expected) in
  [ test_opt "not a character literal" {|c|} ~expected:None
  ; test "escaped newline"
      (String.of_char_list [single_quote; newline; single_quote])
      ~expected:(String.of_char_list [backslash; 'n'])
  ; test "letter" {|'c'|} ~expected:"c"
  ; test "escaped backslash" {|'\\'|} ~expected:{|\\|}
  ; test "escaped single quote" {|'\''|} ~expected:{|\'|}
  ; test "escaped double quote" {|'\"'|} ~expected:{|\"|}
  ; test "backslash n" {|'\n'|} ~expected:{|\n|}
  ; test "backslash t" {|'\t'|} ~expected:{|\t|}
  ; test "backslash b" {|'\b'|} ~expected:{|\b|}
  ; test "backslash r" {|'\r'|} ~expected:{|\r|}
  ; test "backslash space" {|'\ '|} ~expected:{|\ |}
  ; test "decimal escape" {|'\123'|} ~expected:{|\123|}
  ; test "octal escape" {|'\o356'|} ~expected:{|\o356|}
  ; test "hex escape" {|'\xde'|} ~expected:{|\xde|} ]

let tests = tests_string @ tests_char
