open Base
open Ocamlformat_lib

module Test_location = struct
  let test_compare_width_decreasing =
    let open Migrate_ast in
    let test name a b expected =
      let test_name = "compare_width_decreasing: " ^ name in
      ( test_name
      , `Quick
      , fun () ->
          let got = Location.compare_width_decreasing a b in
          Alcotest.check Alcotest.int test_name expected got )
    in
    let make_pos pos_cnum =
      {Lexing.pos_fname= "fname"; pos_lnum= 0; pos_bol= 0; pos_cnum}
    in
    let make_loc s e loc_ghost =
      {Location.loc_start= make_pos s; loc_end= make_pos e; loc_ghost}
    in
    [ test "equal" (make_loc 0 0 false) (make_loc 0 0 false) 0
    ; test "different start" (make_loc 0 0 false) (make_loc 1 0 false) (-1)
    ; test "same start" (make_loc 0 0 false) (make_loc 0 1 false) 1
    ; test "same start, same end" (make_loc 0 0 true) (make_loc 0 0 false) 1
    ]

  let tests = test_compare_width_decreasing
end

module Test_conf = struct
  let test_check_version =
    let test name ~expected ~exe res =
      let test_name = "compare_version: " ^ name in
      ( test_name
      , `Quick
      , fun () ->
          let got = Conf.check_version ~expected ~exe in
          Alcotest.check Alcotest.(result unit string) test_name res got )
    in
    [ test "empty 'expected'" ~expected:"" ~exe:""
        (Error "malformed version number \"\".")
    ; test "invalid 'expected'" ~expected:"0.33..foo" ~exe:""
        (Error "malformed version number \"0.33..foo\".")
    ; test "invalid 'exe'" ~expected:"0.14.2" ~exe:"0.33..foo"
        (Error
           "expected ocamlformat version to be \"0.14.2\" but got \
            \"0.33..foo\".")
    ; test "unknown 'exe'" ~expected:"0.14.2" ~exe:"unknown"
        (Error
           "expected ocamlformat version to be \"0.14.2\" but got \
            \"unknown\".")
    ; test "equal" ~expected:"0.14.1" ~exe:"0.14.1" (Ok ())
    ; test "accepted" ~expected:"0.14.1" ~exe:"0.14.1-15-g273f6f6-dirty"
        (Ok ())
    ; test "too old" ~expected:"0.14.1" ~exe:"0.14.0"
        (Error
           "expected ocamlformat version to be at least \"0.14.1\" but got \
            \"0.14.0\". Please upgrade.")
    ; test "too old 2" ~expected:"0.14.1" ~exe:"0.14.0-15-g273f6f6-dirty"
        (Error
           "expected ocamlformat version to be at least \"0.14.1\" but got \
            \"0.14.0-15-g273f6f6-dirty\". Please upgrade.")
    ; test "too recent" ~expected:"0.14.0" ~exe:"0.14.1"
        (Error
           "expected ocamlformat version to be at most \"0.14.0\" but got \
            \"0.14.1\". Please downgrade.")
    ; test "too recent 2" ~expected:"0.14.0" ~exe:"0.14.1-15-g273f6f6-dirty"
        (Error
           "expected ocamlformat version to be at most \"0.14.0\" but got \
            \"0.14.1-15-g273f6f6-dirty\". Please downgrade.") ]

  let tests = test_check_version
end

module Test_noit = struct
  module Itv = struct
    module T = struct
      type t = int * int

      let sexp_of_t (s, e) =
        Base.Sexp.List [Base.Int.sexp_of_t s; Base.Int.sexp_of_t e]

      let compare = Poly.compare
    end

    include T
    include Comparator.Make (T)

    let contains (sa, ea) (sb, eb) = sa <= sb && eb <= ea

    let compare_width_decreasing (sa, ea) (sb, eb) =
      Poly.compare (sa, -ea) (sb, -eb)

    let pp ppf (s, e) = Caml.Format.fprintf ppf "(%d, %d)" s e

    let equal = Poly.equal
  end

  module T = Non_overlapping_interval_tree.Make (Itv)

  let itv_list = Alcotest.list (module Itv)

  let test_dump =
    let to_string t = Format_.asprintf "%a" Fmt.eval (T.dump t) in
    let test name l expected =
      let test_name = "dump: " ^ name in
      ( test_name
      , `Quick
      , fun () ->
          let t = T.of_list l in
          let got = to_string t in
          Alcotest.check Alcotest.string test_name expected got )
    in
    [ test "empty" [] ""
    ; test "singleton" [(1, 2)] "(1 2)"
    ; test "disjoint" [(1, 2); (3, 5)] "(1 2)\n(3 5)"
    ; test "same start" [(1, 2); (1, 3)] "(1 3)\n {(1 2) }"
    ; test "same end" [(2, 3); (1, 3)] "(1 3)\n {(2 3) }"
    ; test "inclusion"
        [(1, 8); (2, 7); (3, 6); (4, 5)]
        "(1 8)\n {(2 7)\n   {(3 6)\n     {(4 5) } } }" ]

  let test_roots =
    let test name l expected =
      let test_name = "roots: " ^ name in
      ( test_name
      , `Quick
      , fun () ->
          let t = T.of_list l in
          let got = T.roots t in
          Alcotest.check itv_list test_name expected got )
    in
    [ test "empty" [] []
    ; test "singleton" [(1, 2)] [(1, 2)]
    ; test "disjoint" [(1, 2); (3, 4)] [(1, 2); (3, 4)]
    ; test "inclusion" [(1, 2); (3, 6); (4, 5)] [(1, 2); (3, 6)] ]

  let test_children =
    let test name l i expected =
      let test_name = "children: " ^ name in
      ( test_name
      , `Quick
      , fun () ->
          let t = T.of_list l in
          let got = T.children t i in
          Alcotest.check itv_list test_name expected got )
    in
    [ test "empty" [] (1, 2) []
    ; test "no children" [(1, 2)] (1, 2) []
    ; test "not present" [(1, 2)] (3, 4) []
    ; test "one level" [(1, 2); (3, 6); (4, 5)] (3, 6) [(4, 5)]
    ; test "two levels" [(1, 2); (3, 8); (4, 7); (5, 6)] (3, 8) [(4, 7)]
    ; test "no partial results" [(1, 2); (3, 6); (4, 5)] (3, 5) [] ]

  let tests = test_dump @ test_roots @ test_children
end

let tests =
  [ ("Location", Test_location.tests)
  ; ("Conf", Test_conf.tests)
  ; ("non overlapping interval tree", Test_noit.tests)
  ; ("Ast", Test_ast.tests) ]

let () = Alcotest.run "ocamlformat" tests
