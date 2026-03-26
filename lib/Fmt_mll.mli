type fmt_code =
     Conf.t
  -> offset:int
  -> set_margin:bool
  -> string
  -> (Fmt.t, [`Msg of string]) Result.t

val collect_ocaml_codes :
  Ocamlformat_mll_parser.Mll_ast.lexer_def
  -> (Ocamlformat_mll_parser.Mll_ast.ocaml_code * bool) list
(** Returns [(code, block_level)] pairs. [block_level] is true for
    headers and trailers, false for rule actions. *)

val fmt_lexer_def :
     Conf.t
  -> cmts:Cmts.t
  -> fmt_code:fmt_code
  -> fmt_code_structure:fmt_code
  -> Ocamlformat_mll_parser.Mll_ast.lexer_def
  -> Fmt.t
