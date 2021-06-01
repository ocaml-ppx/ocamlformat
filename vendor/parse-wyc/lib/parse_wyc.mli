val structure : Lexing.lexbuf -> Ppxlib.Parsetree.structure

val signature : Lexing.lexbuf -> Ppxlib.Parsetree.signature

val use_file : Lexing.lexbuf -> Ppxlib.Parsetree.toplevel_phrase list
