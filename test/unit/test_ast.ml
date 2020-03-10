open! Base
open Ocamlformat_lib

let test_is_symbol_id =
  let test symbol ~expected =
    ( symbol
    , `Quick
    , fun () ->
        let got = Ast.is_symbol_id symbol in
        Alcotest.(check bool) "expected" got expected )
  in
  let test_true = List.map ~f:(test ~expected:true) in
  let test_false = List.map ~f:(test ~expected:false) in
  test_true
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
  @ test_false ["let"; "let+abc"; "and"]

let tests = List.concat [test_is_symbol_id]
