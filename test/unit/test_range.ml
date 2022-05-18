open Base
open Ocamlformat

let pp =
  let open Stdlib.Format in
  let pp_pair f fs (x, y) = fprintf fs "(%a, %a)" f x f y in
  let pp_none fs () = Stdlib.Format.fprintf fs "None" in
  pp_print_option ~none:pp_none (pp_pair pp_print_int)

let tests_make =
  let test ?range source ~expected =
    let test_name = Stdlib.Format.asprintf "make: %S %a" source pp range in
    ( test_name
    , `Quick
    , fun () ->
        let output = Range.(get @@ make source ?range) in
        Alcotest.(check (pair int int)) test_name expected output )
  in
  [ test "" ~expected:(1, 0)
  ; test "" ~range:(0, 0) ~expected:(1, 0)
  ; test "" ~range:(1, 0) ~expected:(1, 0)
  ; test "" ~range:(-1, -1) ~expected:(1, 0)
  ; test "\n" ~expected:(1, 1)
  ; test "\n" ~range:(0, 0) ~expected:(1, 1)
  ; test "\n" ~range:(1, 0) ~expected:(1, 1)
  ; test "\n" ~range:(-1, -1) ~expected:(1, 1)
  ; test "\n" ~range:(1, 2) ~expected:(1, 1)
  ; test "xxxx\nxxxxx" ~expected:(1, 2)
  ; test "xxxx\nxxxxx" ~range:(1, 1000) ~expected:(1, 2)
  ; test "xxxx\nxxxxx" ~range:(1, 2) ~expected:(1, 2)
  ; test "\n\n" ~range:(0, 0) ~expected:(1, 2)
  ; test "\n\n" ~range:(1, 2) ~expected:(1, 2)
  ; test "xxx\nxxxx\nxxxx" ~expected:(1, 3)
  ; test "xxx\nxxxx\nxxxx" ~range:(1, 3) ~expected:(1, 3)
  ; test "xxx\nxxxx\nxxxx" ~range:(1, 1) ~expected:(1, 1)
  ; test "xxx\nxxxx\nxxxx" ~range:(2, 3) ~expected:(2, 3)
  ; test "xxx\nxxxx\nxxxx" ~range:(3, 3) ~expected:(3, 3) ]

let tests = tests_make
