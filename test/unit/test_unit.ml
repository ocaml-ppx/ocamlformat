open Parse_wyc

module Position = struct
  open Lexing

  let repr p = (p.pos_lnum, p.pos_cnum - p.pos_bol)
end

module Location = struct
  open Location

  let repr l = (Position.repr l.loc_start, Position.repr l.loc_end)
end

module Locations = struct
  let test_impl =
    let test name input expected =
      let test_name = "impl " ^ name in
      ( test_name,
        `Quick,
        fun () ->
          let lexbuf = Lexing.from_string input in
          let actual = List.map Location.repr (implementation lexbuf) in
          let ty = Alcotest.(list (pair (pair int int) (pair int int))) in
          Alcotest.check ty test_name expected actual )
    in
    let valid_test =
      {|
let fooooooooooooooo =
  let fooooooooooooo =
    let foooooooooooooo =
      foooooooooooooo
    in
    fooooo, fooooooo
  in
  fooooooooooooooooo;
  foooooooooooooo
|}
    in
    let invalid_after_eq_test = {|let fooooooooooooooo =|} in
    let invalid_after_in_test =
      {| (* line 1 *)
let fooooooooooooooooooooo = (* line 2 *)
  let foooooooooooooooooooo =
    let foooooooooooooooo =
      foooooooo
    in
    foooooo
  in (* line 8 *)
|}
    in
    let invalid_seq_modules_test =
      {|
module M = struct
  let foooooo = foooo

  let foooooooooooooo = (* line 5 *)

  let foooooooooooo = fooooo
end
|}
    in
    let not_closed_module_test =
      {|
module M = struct
  let foo = foo
  let foo =
|}
    in
    let not_closed_module_test_2 = {|
module M = struct
  let foo = foo in
|} in
    let not_closed_sig = {|
module K : sig
  type t
|} in
    let not_closed_begin = {| let x = if x then begin a |} in
    let not_closed_if = {| let x = if k |} in
    let not_closed_if_2 = {| let x = if k then |} in
    let invalid_if = {| let x = if k then else |} in
    let invalid_if_2 = {| let x = if k then x else |} in
    let not_closed_class = {| class c = object |} in
    let not_closed_class_2 = {| class c |} in
    let not_closed_class_3 = {| class c = |} in
    let not_closed_class_4 = {| class |} in
    let binop = {| x + |} in
    [
      test "empty" "" [];
      test "valid" valid_test [];
      test "invalid after eq" invalid_after_eq_test [ ((1, 0), (1, 22)) ];
      test "invalid after in" invalid_after_in_test [ ((2, 0), (8, 4)) ];
      test "invalid seq modules" invalid_seq_modules_test [ ((2, 11), (7, 28)) ];
      test "not closed module" not_closed_module_test [ ((2, 11), (4, 11)) ];
      test "not closed module 2" not_closed_module_test_2 [ ((2, 11), (3, 18)) ];
      test "not closed sig" not_closed_sig [ ((2, 11), (3, 8)) ];
      test "not closed begin" not_closed_begin [ ((1, 1), (1, 26)) ];
      test "not closed if" not_closed_if [ ((1, 1), (1, 13)) ];
      test "not closed if 2" not_closed_if_2 [ ((1, 1), (1, 18)) ];
      test "invalid if" invalid_if [ ((1, 1), (1, 18)) ];
      test "invalid if 2" invalid_if_2 [ ((1, 1), (1, 25)) ];
      test "not closed class" not_closed_class [ ((1, 11), (1, 17)) ];
      test "not closed class 2" not_closed_class_2 [ ((1, 1), (1, 8)) ];
      test "not closed class 3" not_closed_class_3 [ ((1, 1), (1, 10)) ];
      test "not closed class 4" not_closed_class_4 [ ((1, 1), (1, 6)) ];
      test "binop" binop [ ((1, 1), (1, 4)) ];
    ]

  let tests = test_impl
end

let tests = [ ("locations", Locations.tests) ]

let () = Alcotest.run "Parse_wyc" tests
