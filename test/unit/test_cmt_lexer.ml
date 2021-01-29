open Ocamlformat_lib.Cmt_lexer
open Ocamlformat_lib.Migrate_ast.Location

let lex =
  let pp fs = function
    | Cmt x -> Fmt.pf fs "(Cmt %a)" (pp_loc Fmt.string) x
    | S x -> Fmt.pf fs "(S %a)" (pp_loc Fmt.string) x
  in
  Alcotest.testable pp ( = )

let test_lex_comments =
  let make_test name ~input ~expected =
    let test_name = "lex_comments: " ^ name in
    let test_fun () =
      let actual = lex_comments input in
      Alcotest.(check (list lex)) test_name expected actual
    in
    (test_name, `Quick, test_fun)
  in
  let loc_ghost = false in
  let pos_fname = "_none_" in
  [ make_test "empty" ~input:"" ~expected:[]
  ; make_test "multi empty" ~input:"\n\n"
      ~expected:
        [ S
            { txt= "\n\n"
            ; loc=
                { loc_ghost= false
                ; loc_start=
                    { pos_fname= "_none_"
                    ; pos_lnum= 0
                    ; pos_bol= 0
                    ; pos_cnum= 0 }
                ; loc_end=
                    { pos_fname= "_none_"
                    ; pos_lnum= 2
                    ; pos_bol= 2
                    ; pos_cnum= 2 } } } ]
  ; make_test "cmt before" ~input:"(* fooooooooooooo *)\nbar\n"
      ~expected:
        [ Cmt
            { txt= "(* fooooooooooooo *)"
            ; loc=
                { loc_ghost
                ; loc_start= {pos_fname; pos_lnum= 0; pos_bol= 0; pos_cnum= 0}
                ; loc_end= {pos_fname; pos_lnum= 0; pos_bol= 0; pos_cnum= 20}
                } }
        ; S
            { txt= "\nbar\n"
            ; loc=
                { loc_ghost
                ; loc_start=
                    {pos_fname; pos_lnum= 0; pos_bol= 0; pos_cnum= 20}
                ; loc_end= {pos_fname; pos_lnum= 2; pos_bol= 25; pos_cnum= 25}
                } } ]
  ; make_test "cmt multi before" ~input:"(* foooooo\n fooooooooooooo *)\nbar"
      ~expected:
        [ Cmt
            { txt= "(* foooooo\n fooooooooooooo *)"
            ; loc=
                { loc_ghost
                ; loc_start= {pos_fname; pos_lnum= 0; pos_bol= 0; pos_cnum= 0}
                ; loc_end= {pos_fname; pos_lnum= 1; pos_bol= 11; pos_cnum= 29}
                } }
        ; S
            { txt= "\nbar"
            ; loc=
                { loc_ghost
                ; loc_start=
                    {pos_fname; pos_lnum= 1; pos_bol= 11; pos_cnum= 29}
                ; loc_end= {pos_fname; pos_lnum= 2; pos_bol= 30; pos_cnum= 33}
                } } ]
  ; make_test "cmt after" ~input:"foo\n(* bar *)"
      ~expected:
        [ S
            { txt= "foo\n"
            ; loc=
                { loc_ghost
                ; loc_start= {pos_fname; pos_lnum= 0; pos_bol= 0; pos_cnum= 0}
                ; loc_end= {pos_fname; pos_lnum= 1; pos_bol= 4; pos_cnum= 4}
                } }
        ; Cmt
            { txt= "(* bar *)"
            ; loc=
                { loc_ghost
                ; loc_start= {pos_fname; pos_lnum= 1; pos_bol= 4; pos_cnum= 4}
                ; loc_end= {pos_fname; pos_lnum= 1; pos_bol= 4; pos_cnum= 13}
                } } ]
  ; make_test "cmt multi after" ~input:"fooooo\n(* baaaaaar\n  baaaaaar *)"
      ~expected:
        [ S
            { txt= "fooooo\n"
            ; loc=
                { loc_ghost
                ; loc_start= {pos_fname; pos_lnum= 0; pos_bol= 0; pos_cnum= 0}
                ; loc_end= {pos_fname; pos_lnum= 1; pos_bol= 7; pos_cnum= 7}
                } }
        ; Cmt
            { txt= "(* baaaaaar\n  baaaaaar *)"
            ; loc=
                { loc_ghost
                ; loc_start= {pos_fname; pos_lnum= 1; pos_bol= 7; pos_cnum= 7}
                ; loc_end= {pos_fname; pos_lnum= 2; pos_bol= 19; pos_cnum= 32}
                } } ]
  ; make_test "2 cmts" ~input:"(* foo *)(* bar *)"
      ~expected:
        [ Cmt
            { txt= "(* foo *)"
            ; loc=
                { loc_ghost
                ; loc_start= {pos_fname; pos_lnum= 0; pos_bol= 0; pos_cnum= 0}
                ; loc_end= {pos_fname; pos_lnum= 0; pos_bol= 0; pos_cnum= 9}
                } }
        ; Cmt
            { txt= "(* bar *)"
            ; loc=
                { loc_ghost
                ; loc_start= {pos_fname; pos_lnum= 0; pos_bol= 0; pos_cnum= 9}
                ; loc_end= {pos_fname; pos_lnum= 0; pos_bol= 0; pos_cnum= 18}
                } } ]
  ; make_test "2 cmts break" ~input:"(* foo *)\n(* bar *)"
      ~expected:
        [ Cmt
            { txt= "(* foo *)"
            ; loc=
                { loc_ghost
                ; loc_start= {pos_fname; pos_lnum= 0; pos_bol= 0; pos_cnum= 0}
                ; loc_end= {pos_fname; pos_lnum= 0; pos_bol= 0; pos_cnum= 9}
                } }
        ; S
            { txt= "\n"
            ; loc=
                { loc_ghost
                ; loc_start= {pos_fname; pos_lnum= 0; pos_bol= 0; pos_cnum= 9}
                ; loc_end= {pos_fname; pos_lnum= 1; pos_bol= 10; pos_cnum= 10}
                } }
        ; Cmt
            { txt= "(* bar *)"
            ; loc=
                { loc_ghost
                ; loc_start=
                    {pos_fname; pos_lnum= 1; pos_bol= 10; pos_cnum= 10}
                ; loc_end= {pos_fname; pos_lnum= 1; pos_bol= 10; pos_cnum= 19}
                } } ]
  ; make_test "let after module"
      ~input:{|
module X = struct
  let x = [

  let y = bar
end

let f =
|}
      ~expected:
        [ S
            { txt=
                "\n\
                 module X = struct\n\
                \  let x = [\n\n\
                \  let y = bar\n\
                 end\n\n\
                 let f =\n"
            ; loc=
                { loc_ghost= false
                ; loc_start=
                    { pos_fname= "_none_"
                    ; pos_lnum= 0
                    ; pos_bol= 0
                    ; pos_cnum= 0 }
                ; loc_end=
                    { pos_fname= "_none_"
                    ; pos_lnum= 8
                    ; pos_bol= 59
                    ; pos_cnum= 59 } } } ]
  ; make_test "2 let 1l split" ~input:"let x = x\nlet y = y"
      ~expected:
        [ S
            { txt= "let x = x\nlet y = y"
            ; loc=
                { loc_ghost= false
                ; loc_start=
                    { pos_fname= "_none_"
                    ; pos_lnum= 0
                    ; pos_bol= 0
                    ; pos_cnum= 0 }
                ; loc_end=
                    { pos_fname= "_none_"
                    ; pos_lnum= 1
                    ; pos_bol= 10
                    ; pos_cnum= 19 } } } ]
  ; make_test "already formatted"
      ~input:
        {|let foooooo =
  let baaaaar =
    let woooooo = foooooo in
    let xooooo = bar + foo in
    woooooo
  in
  bar
|}
      ~expected:
        [ S
            { txt=
                "let foooooo =\n\
                \  let baaaaar =\n\
                \    let woooooo = foooooo in\n\
                \    let xooooo = bar + foo in\n\
                \    woooooo\n\
                \  in\n\
                \  bar\n"
            ; loc=
                { loc_ghost= false
                ; loc_start=
                    { pos_fname= "_none_"
                    ; pos_lnum= 0
                    ; pos_bol= 0
                    ; pos_cnum= 0 }
                ; loc_end=
                    { pos_fname= "_none_"
                    ; pos_lnum= 7
                    ; pos_bol= 112
                    ; pos_cnum= 112 } } } ]
  ; make_test "ill-indent"
      ~input:
        {|   let () =
 ffff;
     hhhhhh;
        fff;
 let (quot, _rem) =
   let quot_rem n k =
     let (d, m) = (n / k, n mod k) in
     if d < 0 && m > 0 then (d+1, m-k)
else (d, m)
    in
    let quot n k = fst (quot_rem n k) in
    let rem n k = snd (quot_rem n k) in

quot, rem
|}
      ~expected:
        [ S
            { txt=
                {|   let () =
 ffff;
     hhhhhh;
        fff;
 let (quot, _rem) =
   let quot_rem n k =
     let (d, m) = (n / k, n mod k) in
     if d < 0 && m > 0 then (d+1, m-k)
else (d, m)
    in
    let quot n k = fst (quot_rem n k) in
    let rem n k = snd (quot_rem n k) in

quot, rem
|}
            ; loc=
                { loc_ghost= false
                ; loc_start=
                    { pos_fname= "_none_"
                    ; pos_lnum= 0
                    ; pos_bol= 0
                    ; pos_cnum= 0 }
                ; loc_end=
                    { pos_fname= "_none_"
                    ; pos_lnum= 14
                    ; pos_bol= 275
                    ; pos_cnum= 275 } } } ]
  ; make_test "comment header"
      ~input:
        {|(**************************************************************************)
(**************************************************************************)

module Format = Format_

(** Format OCaml Ast *)

open Migrate_ast
|}
      ~expected:
        [ Cmt
            { txt=
                "(**************************************************************************)"
            ; loc=
                { loc_ghost= false
                ; loc_start=
                    { pos_fname= "_none_"
                    ; pos_lnum= 0
                    ; pos_bol= 0
                    ; pos_cnum= 0 }
                ; loc_end=
                    { pos_fname= "_none_"
                    ; pos_lnum= 0
                    ; pos_bol= 0
                    ; pos_cnum= 76 } } }
        ; S
            { txt= "\n"
            ; loc=
                { loc_ghost= false
                ; loc_start=
                    { pos_fname= "_none_"
                    ; pos_lnum= 0
                    ; pos_bol= 0
                    ; pos_cnum= 76 }
                ; loc_end=
                    { pos_fname= "_none_"
                    ; pos_lnum= 1
                    ; pos_bol= 77
                    ; pos_cnum= 77 } } }
        ; Cmt
            { txt=
                "(**************************************************************************)"
            ; loc=
                { loc_ghost= false
                ; loc_start=
                    { pos_fname= "_none_"
                    ; pos_lnum= 1
                    ; pos_bol= 77
                    ; pos_cnum= 77 }
                ; loc_end=
                    { pos_fname= "_none_"
                    ; pos_lnum= 1
                    ; pos_bol= 77
                    ; pos_cnum= 153 } } }
        ; S
            { txt= "\n\nmodule Format = Format_\n\n"
            ; loc=
                { loc_ghost= false
                ; loc_start=
                    { pos_fname= "_none_"
                    ; pos_lnum= 1
                    ; pos_bol= 77
                    ; pos_cnum= 153 }
                ; loc_end=
                    { pos_fname= "_none_"
                    ; pos_lnum= 5
                    ; pos_bol= 180
                    ; pos_cnum= 180 } } }
        ; Cmt
            { txt= "(** Format OCaml Ast *)"
            ; loc=
                { loc_ghost= false
                ; loc_start=
                    { pos_fname= "_none_"
                    ; pos_lnum= 5
                    ; pos_bol= 180
                    ; pos_cnum= 180 }
                ; loc_end=
                    { pos_fname= "_none_"
                    ; pos_lnum= 5
                    ; pos_bol= 180
                    ; pos_cnum= 203 } } }
        ; S
            { txt= "\n\nopen Migrate_ast\n"
            ; loc=
                { loc_ghost= false
                ; loc_start=
                    { pos_fname= "_none_"
                    ; pos_lnum= 5
                    ; pos_bol= 180
                    ; pos_cnum= 203 }
                ; loc_end=
                    { pos_fname= "_none_"
                    ; pos_lnum= 8
                    ; pos_bol= 222
                    ; pos_cnum= 222 } } } ] ]

let tests = test_lex_comments
