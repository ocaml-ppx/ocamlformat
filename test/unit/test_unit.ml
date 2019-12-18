open Parse_wyc

module Position = struct
  open Lexing

  let pp fs p =
    Format.fprintf fs "line %i, column %i" p.pos_lnum (p.pos_cnum - p.pos_bol)
end

module Location = struct
  open Location

  let pp fs l =
    Format.fprintf fs "start: (%a), end: (%a)" Position.pp l.loc_start
      Position.pp l.loc_end

  let to_string l = Format.asprintf "%a" pp l
end

module Locations = struct
  let test_impl =
    let test name input expected =
      let test_name = "impl " ^ name in
      ( test_name,
        `Quick,
        fun () ->
          let lexbuf = Lexing.from_string input in
          let actual = List.map Location.to_string (implementation lexbuf) in
          Alcotest.(check (list string)) test_name expected actual )
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
    [
      test "empty" "" [];
      test "valid" valid_test [];
      test "invalid after eq" invalid_after_eq_test
        [ "start: (line 1, column 0), end: (line 1, column 22)" ];
      test "invalid after in" invalid_after_in_test
        [ "start: (line 2, column 0), end: (line 8, column 4)" ];
      test "invalid seq modules" invalid_seq_modules_test
        [ "start: (line 2, column 11), end: (line 7, column 28)" ];
      test "not closed module" not_closed_module_test
        [ "start: (line 2, column 11), end: (line 4, column 11)" ];
    ]

  let tests = test_impl
end

let tests = [ ("locations", Locations.tests) ]

let () = Alcotest.run "Parse_wyc" tests
