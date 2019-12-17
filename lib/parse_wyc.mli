val implementation :
  Lexing.lexbuf -> Migrate_parsetree.OCaml_current.Ast.Parsetree.structure

val interface :
  Lexing.lexbuf -> Migrate_parsetree.OCaml_current.Ast.Parsetree.signature

val use_file :
  Lexing.lexbuf ->
  Migrate_parsetree.OCaml_current.Ast.Parsetree.toplevel_phrase list
