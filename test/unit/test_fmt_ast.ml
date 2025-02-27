open Ocamlformat_stdlib
open Ocamlformat_lib

let updated_ast_tests =
  [ ( "updated AST"
    , `Quick
    , fun () ->
        (* We try to ensure that modified ASTs can be printed by ocamlformat,
           which can fail due to assumption about certain constructions
           having corresponding bits of syntax in the Source.t. *)
        let source1 = "" in
        let source2 =
          (* Shift all the locations down, in case the parser consults
             location information somehow. *)
          String.make 1000 '\n'
          ^ {outer|
let _ =
  (* exercise every expression construct *)
  x;
  1_2;
  12l;
  'a';
  '\n';
  "a\013";
  {|a|};
  12e1;
  (let rec x = 1 and y = 2 in ());
  (let x = 1 and y = 2 in ());
  (fun x y : a -> function 1 -> 1);
  f a ~b ?c;
  (match () with () -> () | () -> ());
  (try () with () -> () | () -> ());
  ((), ());
  (Some (); None);
  (`Some (); `None);
  ({ a = 1; b : float = 2 }, { r with a });
  a.x;
  a.x <- 1;
  [|1;2|];
  [1;2];
  (if a then b else if c then d else e);
  (a; b);
  (while a; do b; done);
  (for a = b to c do d done);
  (a : b);
  (a : b :> c);
  a#b;
  x <- 2;
  {< x = 1 >};
  (let module M = struct end in ());
  (let exception E in ());
  assert ();
  lazy 1;
  object val x = 1 end;
  (module M);
  (module M : S);
  (let open M in 1);
  M.(1);
  (let+ x = 1 and+ y = 2 in ());
  [%extension 1];
  (function _ -> .);
  _;
  begin () end;
  (a :: b);
  a.!(b);
  a.!(b) <- c;
  !a;
  a + b;
|outer}
        in
        let conf = Ocamlformat_lib.Conf.default in
        let ast ~input_name ~source =
          Ocamlformat_lib.Parse_with_comments.parse
            (Ocamlformat_lib.Parse_with_comments.parse_ast conf)
            Structure conf ~input_name ~source
        in
        let ast1 = ast ~input_name:"source1" ~source:source1 in
        let ast2 =
          let ast = ast ~input_name:"source2" ~source:source2 in
          let ghostify =
            { Ocamlformat_parser_extended.Ast_mapper.default_mapper with
              location= (fun _ loc -> {loc with loc_ghost= true}) }
          in
          {ast with ast= ghostify.structure ghostify ast.ast}
        in
        let ast_replaced = {ast1 with ast= ast2.ast} in
        let with_buffer_formatter ~buffer_size k =
          let buffer = Buffer.create buffer_size in
          let fs = Format_.formatter_of_buffer buffer in
          Fmt.eval fs k ;
          Format_.pp_print_flush fs () ;
          if Buffer.length buffer > 0 then Format_.pp_print_newline fs () ;
          Buffer.contents buffer
        in
        let print (ast : _ Parse_with_comments.with_comments) =
          let open Fmt in
          let debug = conf.opr_opts.debug.v in
          with_buffer_formatter ~buffer_size:1000
            ( set_margin conf.fmt_opts.margin.v
            $ set_max_indent conf.fmt_opts.max_indent.v
            $ Fmt_ast.fmt_ast Structure ~debug ast.source
                (Ocamlformat_lib.Cmts.init Structure ~debug ast.source
                   ast.ast ast.comments )
                conf ast.ast )
        in
        let printed_ast_replaced = String.strip (print ast_replaced) in
        let expected =
          String.strip
            {outer|
let _ =
  x;
  1_2;
  12l;
  'a';
  '\n';
  "a\r";
  {|a|};
  12e1;
  (let rec x = 1 and y = 2 in
   ());
  (let x = 1 and y = 2 in
   ());
  (fun x y : a -> function 1 -> 1);
  f a ~b ?c;
  (match () with () -> () | () -> ());
  (try () with () -> () | () -> ());
  ((), ());
  Some ();
  None;
  `Some ();
  `None;
  ({ a = 1; b : float = 2 }, { r with a });
  a.x;
  a.x <- 1;
  [| 1; 2 |];
  [ 1; 2 ];
  if a then b else if c then d else e;
  a;
  b;
  while a do
    b
  done;
  for a = b to c do
    d
  done;
  (a : b);
  (a : b :> c);
  a#b;
  x <- 2;
  {<x = 1>};
  (let module M = struct end in
  ());
  (let exception E in
  ());
  assert ();
  lazy 1;
  object
    val x = 1
  end;
  (module M);
  (module M : S);
  (let open M in
   1);
  M.(1);
  (let+ x = 1 and+ y = 2 in
   ());
  [%extension 1];
  (function _ -> .);
  _;
  ();
  a :: b;
  a.!(b);
  a.!(b) <- c;
  !a;
  a + b
|outer}
        in
        (* Ideally we'd improve two things about this test:

           - check the new string parses, to the same AST as the original one
           - use ppx_expect, so we have a nicer workflow and more readable
           errors *)
        if String.( <> ) expected printed_ast_replaced then (
          print_endline "got:" ;
          print_endline printed_ast_replaced ;
          failwith "different result" ) ) ]

let tests = updated_ast_tests
