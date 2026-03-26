{
  let   x   =   1
}

let   newline  =   '\n' | "\r\n"
let   blank = [' '    '\t']
let ident = ['a'-'z'   'A'-'Z'    '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule    main   =   parse
  |  newline
    { new_line   lexbuf;    main    lexbuf }
  | blank+
    { main   lexbuf }
  | eof
    { () }
