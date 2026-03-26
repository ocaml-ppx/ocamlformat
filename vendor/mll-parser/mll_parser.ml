exception Parse_error of string * Lexing.position

let parse_string ~input_name source =
  let lexbuf = Lexing.from_string source in
  Location.init_info lexbuf input_name;
  Mll_lexer.comments := [];
  let result =
    try Mll_grammar.lexer_def Mll_lexer.main lexbuf
    with Mll_grammar.Error ->
      let pos = lexbuf.lex_curr_p in
      raise (Parse_error ("syntax error", pos))
  in
  let comments = Mll_lexer.reset_comments () in
  { result with Mll_ast.comments }
