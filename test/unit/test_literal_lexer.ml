open Base
open Ocamlformat_lib

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
        Alcotest.check Alcotest.(option string) Stdlib.__LOC__ expected got
    )
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

let tests = tests_string
