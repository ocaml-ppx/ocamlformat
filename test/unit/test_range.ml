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
        let range = Range.make source ?range in
        let l, h = Range.get range in
        let output = (l, h, Range.is_whole range) in
        Alcotest.(check (triple int int bool)) test_name expected output )
  in
  [ test "" ~expected:(1, 0, true)
  ; test "" ~range:(0, 0) ~expected:(1, 0, true)
  ; test "" ~range:(1, 0) ~expected:(1, 0, true)
  ; test "" ~range:(-1, -1) ~expected:(1, 0, true)
  ; test "" ~range:(1, 1) ~expected:(1, 1, true)
  ; test "\n" ~expected:(1, 1, true)
  ; test "\n" ~range:(0, 0) ~expected:(1, 1, true)
  ; test "\n" ~range:(1, 0) ~expected:(1, 1, true)
  ; test "\n" ~range:(-1, -1) ~expected:(1, 1, true)
  ; test "\n" ~range:(1, 2) ~expected:(1, 2, true)
  ; test "xxxx\nxxxxx" ~expected:(1, 2, true)
  ; test "xxxx\nxxxxx" ~range:(1, 1000) ~expected:(1, 2, true)
  ; test "xxxx\nxxxxx" ~range:(1, 2) ~expected:(1, 2, true)
  ; test "\n\n" ~expected:(1, 2, true)
  ; test "\n\n" ~range:(0, 0) ~expected:(1, 2, true)
  ; test "\n\n" ~range:(1, 2) ~expected:(1, 2, true)
  ; test "\nxxxx\nxxxx" ~expected:(1, 3, true)
  ; test "xxx\nxxxx\n" ~expected:(1, 2, true)
  ; test "xxx\nxxxx\nxxxx" ~expected:(1, 3, true)
  ; test "xxx\nxxxx\nxxxx" ~range:(1, 3) ~expected:(1, 3, true)
  ; test "xxx\nxxxx\nxxxx" ~range:(1, 4) ~expected:(1, 4, true)
  ; test "xxx\nxxxx\nxxxx" ~range:(1, 1) ~expected:(1, 1, false)
  ; test "xxx\nxxxx\nxxxx" ~range:(2, 3) ~expected:(2, 3, false)
  ; test "xxx\nxxxx\nxxxx" ~range:(3, 3) ~expected:(3, 3, false)
  ; test "xxx\nxxxx\nxxxx" ~range:(4, 4) ~expected:(4, 4, false) ]

let tests = tests_make
