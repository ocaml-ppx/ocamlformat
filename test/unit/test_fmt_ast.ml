open Ocamlformat_stdlib
open Ocamlformat_lib

let check_updated_test source expected =
  let source1 = "" in
  let source2 =
    (* Shift all the locations down, in case the parser consults location
       information somehow. *)
    String.make 1000 '\n' ^ source
  in
  let conf = Ocamlformat_lib.Conf.default in
  let parse_struct ~input_name ~source =
    let (Extended_ast.Any parsed) =
      Extended_ast.parse Syntax.Structure conf ~input_name ~source
    in
    match parsed with Structure _ as r -> r | _ -> assert false
  in
  let unwrap_structure (parsed : _ Extended_ast.t) =
    match parsed with
    | Structure {ast; std; cmts} -> (ast, std, cmts)
    | _ -> .
  in
  let ast1 = parse_struct ~input_name:"source1" ~source:source1 in
  let _, ast1_std, ast1_cmts = unwrap_structure ast1 in
  let ast2_ast =
    let parsed = parse_struct ~input_name:"source2" ~source:source2 in
    let ast, _, _ = unwrap_structure parsed in
    let ghostify =
      { Ocamlformat_parser_extended.Ast_mapper.default_mapper with
        location= (fun _ loc -> {loc with loc_ghost= true}) }
    in
    ghostify.structure ghostify ast
  in
  let mixed =
    Extended_ast.Structure {ast= ast2_ast; std= ast1_std; cmts= ast1_cmts}
  in
  let with_buffer_formatter ~buffer_size k =
    let buffer = Buffer.create buffer_size in
    let fs = Format_.formatter_of_buffer buffer in
    Fmt.eval fs k ;
    Format_.pp_print_flush fs () ;
    if Buffer.length buffer > 0 then Format_.pp_print_newline fs () ;
    Buffer.contents buffer
  in
  let print parsed =
    let open Fmt in
    let debug = conf.opr_opts.debug.v in
    let body, _ = Fmt_ast.fmt_ast parsed ~debug conf in
    with_buffer_formatter ~buffer_size:1000
      ( set_margin conf.fmt_opts.margin.v
      $ set_max_indent conf.fmt_opts.max_indent.v
      $ body )
  in
  let printed_ast_replaced = String.strip (print mixed) in
  (* Ideally we'd improve two things about this test:

     - check the new string parses, to the same AST as the original one - use
     ppx_expect, so we have a nicer workflow and more readable errors *)
  Alcotest.check Alcotest.string
    ("updated AST: " ^ source)
    expected printed_ast_replaced

let updated_ast_tests =
  [ ( "updated AST"
    , `Quick
    , fun () ->
        (* We try to ensure that modified ASTs can be printed by ocamlformat,
           which can fail due to assumption about certain constructions
           having corresponding bits of syntax in the Source.t. *)
        (* exercise every expression construct *)
        check_updated_test "x" "x" ;
        check_updated_test "1_2" "1_2" ;
        check_updated_test "12l" "12l" ;
        check_updated_test "'a'" "'a'" ;
        check_updated_test {|'\n'|} {|'\n'|} ;
        check_updated_test {|"a\013"|} {|"a\r"|} ;
        check_updated_test "{|a|}" "{|a|}" ;
        check_updated_test "12e1" "12e1" ;
        check_updated_test "let rec x = 1 and y = 2 in ()"
          "let rec x = 1 and y = 2 in\n()" ;
        check_updated_test "let x = 1 and y = 2 in ()"
          "let x = 1 and y = 2 in\n()" ;
        check_updated_test "fun x y : a -> function 1 -> 1"
          "fun x y : a -> function 1 -> 1" ;
        check_updated_test "f a ~b ?c" "f a ~b ?c" ;
        check_updated_test "match () with () -> () | () -> ()"
          "match () with () -> () | () -> ()" ;
        check_updated_test "try () with () -> () | () -> ()"
          "try () with () -> () | () -> ()" ;
        check_updated_test "((), ())" "(), ()" ;
        check_updated_test "Some (); None" "Some ();\nNone" ;
        check_updated_test "`Some (); `None" "`Some ();\n`None" ;
        check_updated_test "{ a = 1; b : float = 2 }, { r with a }"
          "{ a = 1; b : float = 2 }, { r with a }" ;
        check_updated_test "a.x" "a.x" ;
        check_updated_test "a.x <- 1" "a.x <- 1" ;
        check_updated_test "[| 1; 2 |]" "[| 1; 2 |]" ;
        check_updated_test "[ 1; 2 ]" "[ 1; 2 ]" ;
        check_updated_test "if a then b else if c then d else e"
          "if a then b else if c then d else e" ;
        check_updated_test "a; b" "a;\nb" ;
        check_updated_test "while a do b done" "while a do\n  b\ndone" ;
        check_updated_test "for a = b to c do d done"
          "for a = b to c do\n  d\ndone" ;
        check_updated_test "(a : b)" "(a : b)" ;
        check_updated_test "(a : b :> c)" "(a : b :> c)" ;
        check_updated_test "a#b" "a#b" ;
        check_updated_test "x <- 2" "x <- 2" ;
        check_updated_test "{<x = 1>}" "{<x = 1>}" ;
        check_updated_test "let module M = struct end in ()"
          "let module M = struct end in\n()" ;
        check_updated_test "let exception E in ()" "let exception E in\n()" ;
        check_updated_test "assert ()" "assert ()" ;
        check_updated_test "lazy 1" "lazy 1" ;
        check_updated_test "object val x = 1 end" "object\n  val x = 1\nend" ;
        check_updated_test "(module M)" "(module M)" ;
        check_updated_test "(module M : S)" "(module M : S)" ;
        check_updated_test "let open M in 1" "let open M in\n1" ;
        check_updated_test "M.(1)" "M.(1)" ;
        check_updated_test "let+ x = 1 and+ y = 2 in ()"
          "let+ x = 1 and+ y = 2 in\n()" ;
        check_updated_test "[%extension 1]" "[%extension 1]" ;
        check_updated_test "function _ -> ." "function _ -> ." ;
        check_updated_test "_" "_" ;
        check_updated_test "begin () end" "begin\n  ()\nend" ;
        check_updated_test "a :: b" "a :: b" ;
        check_updated_test "a.!(b)" "a.!(b)" ;
        check_updated_test "a.!(b) <- c" "a.!(b) <- c" ;
        check_updated_test "!a" "!a" ;
        check_updated_test "a + b" "a + b" ) ]

let tests = updated_ast_tests
