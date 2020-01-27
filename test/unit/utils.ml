module Position = struct
  open Lexing

  let repr p = (p.pos_lnum, p.pos_cnum - p.pos_bol)
end

module Location = struct
  open Location

  let repr l = (Position.repr l.loc_start, Position.repr l.loc_end)
end

let check_content parse ~name ~input ~expected =
  let lexbuf = Lexing.from_string input in
  let actual = List.map Location.repr (parse lexbuf) in
  let ty = Alcotest.(list (pair (pair int int) (pair int int))) in
  Alcotest.check ty name expected actual

let check_use_file = check_content Parse_wyc.use_file

let check_impl = check_content Parse_wyc.implementation

let check_intf = check_content Parse_wyc.interface
