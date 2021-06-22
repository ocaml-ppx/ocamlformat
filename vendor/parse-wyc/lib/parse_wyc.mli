val structure : Lexing.lexbuf -> Parsetree.structure

val signature : Lexing.lexbuf -> Parsetree.signature

val use_file : Lexing.lexbuf -> Parsetree.toplevel_phrase list
