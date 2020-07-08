open! Base
open Ocamlformat_lib

let test_string_id ~f name ~pass ~fail =
  let test symbol ~expected =
    let test_name = Stdlib.Format.sprintf "%s %S" name symbol in
    ( symbol
    , `Quick
    , fun () ->
        let got = f symbol in
        Alcotest.(check bool) test_name got expected )
  in
  let test_true = List.map ~f:(test ~expected:true) in
  let test_false = List.map ~f:(test ~expected:false) in
  test_true pass @ test_false fail

let test_is_prefix =
  test_string_id "is_prefix" ~f:Ast.String_id.is_prefix ~pass:["!"; "?"; "~"]
    ~fail:
      [ "*."
      ; "+"
      ; "!="
      ; "land"
      ; "lor"
      ; "||"
      ; "@"
      ; "::"
      ; ":="
      ; "let+"
      ; "and+"
      ; ".()"
      ; ".*()"
      ; ".*(;..)"
      ; ".[]"
      ; ".*[]"
      ; ".*[;..]"
      ; ".{}"
      ; ".*{}"
      ; ".*{;..}"
      ; ".{}<-"
      ; ".*{}<-"
      ; ".*{;..}<-"
      ; "let"
      ; "let+abc"
      ; "and" ]

let test_is_infix =
  test_string_id "is_infix" ~f:Ast.String_id.is_infix
    ~pass:
      ["*."; "+"; "!="; "land"; "lor"; "||"; "@"; "::"; ":="; "let+"; "and+"]
    ~fail:
      [ "let"
      ; "let+abc"
      ; "and"
      ; ".()"
      ; ".*()"
      ; ".*(;..)"
      ; ".[]"
      ; ".*[]"
      ; ".*[;..]"
      ; ".{}"
      ; ".*{}"
      ; ".*{;..}"
      ; ".{}<-"
      ; ".*{}<-"
      ; ".*{;..}<-" ]

let test_is_symbol =
  test_string_id "is_symbol" ~f:Ast.String_id.is_symbol
    ~pass:
      [ "*."
      ; "+"
      ; "!="
      ; "land"
      ; "lor"
      ; "||"
      ; "@"
      ; "::"
      ; ":="
      ; "let+"
      ; "and+"
      ; ".()"
      ; ".*()"
      ; ".*(;..)"
      ; ".[]"
      ; ".*[]"
      ; ".*[;..]"
      ; ".{}"
      ; ".*{}"
      ; ".*{;..}"
      ; ".{}<-"
      ; ".*{}<-"
      ; ".*{;..}<-" ]
    ~fail:["let"; "let+abc"; "and"]

let test_is_hash_getter =
  test_string_id "is_hash_getter" ~f:Ast.String_id.is_hash_getter
    ~pass:["#"; "#."; "##"; "#++."; "#---#"]
    ~fail:
      [ "let"
      ; "let+abc"
      ; "and"
      ; "*."
      ; "+"
      ; "!="
      ; "land"
      ; "lor"
      ; "||"
      ; "@"
      ; "::"
      ; ":="
      ; "let+"
      ; "and+"
      ; ".()"
      ; ".*()"
      ; ".*(;..)"
      ; ".[]"
      ; ".*[]"
      ; ".*[;..]"
      ; ".{}"
      ; ".*{}"
      ; ".*{;..}"
      ; ".{}<-"
      ; ".*{}<-"
      ; ".*{;..}<-" ]

let test_is_monadic_binding =
  test_string_id "is_monadic_binding" ~f:Ast.String_id.is_monadic_binding
    ~pass:["let+"; "and+"]
    ~fail:
      [ "*."
      ; "+"
      ; "!="
      ; "let"
      ; "let+abc"
      ; "and"
      ; "and#"
      ; "land"
      ; "lor"
      ; "||"
      ; "@"
      ; "::"
      ; ":="
      ; ".()"
      ; ".*()"
      ; ".*(;..)"
      ; ".[]"
      ; ".*[]"
      ; ".*[;..]"
      ; ".{}"
      ; ".*{}"
      ; ".*{;..}"
      ; ".{}<-"
      ; ".*{}<-"
      ; ".*{;..}<-" ]

let tests =
  List.concat
    [ test_is_prefix
    ; test_is_infix
    ; test_is_symbol
    ; test_is_hash_getter
    ; test_is_monadic_binding ]
