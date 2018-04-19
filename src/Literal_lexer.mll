{
open Caml

(* To buffer string literals *)

let string_buffer = Buffer.create 256
let reset_string_buffer () = Buffer.reset string_buffer
let get_stored_string () = Buffer.contents string_buffer

let store_string_char c = Buffer.add_char string_buffer c
let store_string s = Buffer.add_string string_buffer s

}

let newline = ('\013'* '\010')
let hex_digit =
  ['0'-'9' 'A'-'F' 'a'-'f']

    (* the string sometime include its surrounding parens *)
rule string mode = parse
  | '\"'
      { reset_string_buffer ();
        string_aux mode lexbuf;
        get_stored_string () }
  | _ { assert false }


and string_aux mode = parse
    '\"'
      { () }
  | '\\' newline ([' ' '\t'] *)
      { string_aux mode lexbuf }
  | '\\' ['\\' '\'' '\"' 't' 'b' 'r' ' ']
      { store_string (Lexing.lexeme lexbuf);
        string_aux mode lexbuf }
  | "\\n"
      { begin match mode with
          | `Normalize_nl -> store_string "\n"
          | `Preserve -> store_string "\\n" end;
        string_aux mode lexbuf }
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9']
      { store_string (Lexing.lexeme lexbuf);
        string_aux mode lexbuf }
  | '\\' 'o' ['0'-'3'] ['0'-'7'] ['0'-'7']
      { store_string (Lexing.lexeme lexbuf);
        string_aux mode lexbuf }
  | '\\' 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F']
      { store_string (Lexing.lexeme lexbuf);
        string_aux mode lexbuf }
  | '\\' 'u' '{' hex_digit+ '}'
      { store_string (Lexing.lexeme lexbuf);
        string_aux mode lexbuf }
  | '\\' _
      { (*  Should be an error, but we are very lax.
            raise (Error (Illegal_escape (Lexing.lexeme lexbuf),
            Location.curr lexbuf))
        *)
        string_aux mode lexbuf
      }
  | newline
      { store_string (Lexing.lexeme lexbuf);
        string_aux mode lexbuf }
  | eof
      { assert false }
  | _
      { store_string_char (Lexing.lexeme_char lexbuf 0);
        string_aux mode lexbuf }

and char = parse
  | "\'" newline "\'"
      { "\n" }
  | "\'" [^ '\\' '\'' '\010' '\013'] "\'"
      { Lexing.lexeme lexbuf }
  | "\'\\" ['\\' '\'' '\"' 'n' 't' 'b' 'r' ' '] "\'"
      { Lexing.lexeme lexbuf }
  | "\'\\" ['0'-'9'] ['0'-'9'] ['0'-'9'] "\'"
      { Lexing.lexeme lexbuf }
  | "\'\\" 'o' ['0'-'3'] ['0'-'7'] ['0'-'7'] "\'"
      { Lexing.lexeme lexbuf }
  | "\'\\" 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] "\'"
      { Lexing.lexeme lexbuf }
  | "\'\\" _
      { assert false }
