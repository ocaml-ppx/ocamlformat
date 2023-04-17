type 'a t = 'a * [`Recovered | `Not_recovered]

val structure : Lexing.lexbuf -> Parsetree.structure t

val signature : Lexing.lexbuf -> Parsetree.signature t

val use_file : Lexing.lexbuf -> Parsetree.toplevel_phrase list t

val core_type : Lexing.lexbuf -> Parsetree.core_type t

val module_type : Lexing.lexbuf -> Parsetree.module_type t

val expression : Lexing.lexbuf -> Parsetree.expression t

module Parser = Parser
module Lexer = Lexer
