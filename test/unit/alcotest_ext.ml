let pos_equal p1 p2 =
  let open Lexing in
  p1.pos_lnum = p2.pos_lnum && p1.pos_bol = p2.pos_bol
  && p1.pos_cnum = p2.pos_cnum

let loc_equal l1 l2 =
  let open Location in
  pos_equal l1.loc_start l2.loc_start && pos_equal l1.loc_end l2.loc_end

let location = Alcotest.testable Location.print_loc loc_equal
