{
let target_version = "devversion"
}

let enabler = "; enable release renaming"

let disabler = "; disable release renaming"


let dev_version_name = "devversion"

rule enabled b = parse
| dev_version_name { b target_version ; enabled b lexbuf}
| disabler { b (Lexing.lexeme lexbuf) ; disabled b lexbuf}
| eof { () }
| _ { b (Lexing.lexeme lexbuf) ; enabled b lexbuf }

and disabled b = parse
| enabler { b (Lexing.lexeme lexbuf) ; enabled b lexbuf}
| eof { () }
| _ { b (Lexing.lexeme lexbuf) ; enabled b lexbuf }

{
}
