(* Adapted from OCaml's lex/lexer.mll (OCaml 5.4) *)
(* Modifications: collect text into buffers, produce our tokens, track comments *)

{
open Lexing
open Mll_grammar

exception Lexer_error of string * position

let error lexbuf msg =
  raise (Lexer_error (msg, lexbuf.lex_curr_p))

let comments : Mll_ast.ocaml_code list ref = ref []

let reset_comments () =
  let c = List.rev !comments in
  comments := [];
  c

let add_comment lexbuf s =
  let loc =
    { Location.loc_start = lexbuf.lex_start_p
    ; loc_end = lexbuf.lex_curr_p
    ; loc_ghost = false }
  in
  comments := { Mll_ast.value = s; loc } :: !comments

(* Upstream helpers *)
let string_buff = Buffer.create 256
let reset_string_buffer () = Buffer.clear string_buff
let store_string_char c = Buffer.add_char string_buff c
let store_string_chars s = Buffer.add_string string_buff s
let get_stored_string () = Buffer.contents string_buff

let char_for_backslash = function
  | 'n' -> '\010' | 'r' -> '\013' | 'b' -> '\008' | 't' -> '\009' | c -> c

let decimal_code c d u =
  100 * (Char.code c - 48) + 10 * (Char.code d - 48) + (Char.code u - 48)

let char_for_octal_code c d u =
  Char.chr (64 * (Char.code c - 48) + 8 * (Char.code d - 48) + (Char.code u - 48))

let hex_digit_value d =
  let d = Char.code d in
  if d >= 97 then d - 87 else if d >= 65 then d - 55 else d - 48

let char_for_hexadecimal_code d u =
  Char.chr (16 * (hex_digit_value d) + (hex_digit_value u))

let incr_loc lexbuf delta =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with
    pos_lnum = pos.pos_lnum + 1;
    pos_bol = pos.pos_cnum - delta }
}

(* Character classes — from upstream *)
let identstart =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255']
let identbody =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']
let backslash_escapes =
  ['\\' '\'' '"' 'n' 't' 'b' 'r' ' ']
let lowercase = ['a'-'z' '_']
let ident = identstart identbody*
let extattrident = ident ('.' ident)*
let blank = [' ' '\009' '\012']
let uppercase = ['A'-'Z']
let ocaml_identstart = lowercase | uppercase
let identchar = ['A'-'Z' 'a'-'z' '_' '\'' '0'-'9']
let utf8 = ['\192'-'\255'] ['\128'-'\191']*
let identstart_ext = ocaml_identstart | utf8
let identchar_ext = identchar | utf8
let ocaml_ident = identstart_ext identchar_ext*

rule main = parse
  | [' ' '\013' '\009' '\012'] +
    { main lexbuf }
  | '\010'
    { incr_loc lexbuf 0; main lexbuf }
  | "(*"
    { let start_p = lexbuf.lex_start_p in
      let buf = Buffer.create 128 in
      Buffer.add_string buf "(*";
      comment buf 0 lexbuf;
      lexbuf.lex_start_p <- start_p;
      add_comment lexbuf (Buffer.contents buf);
      main lexbuf }
  | '_' { UNDERSCORE }
  | ident as s
    { match s with
      | "rule" -> RULE | "parse" -> PARSE | "shortest" -> SHORTEST
      | "and" -> AND | "eof" -> EOF_KW | "let" -> LET | "as" -> AS
      | "refill" ->
          let start_p = lexbuf.lex_start_p in
          let buf = Buffer.create 256 in
          skip_refill buf lexbuf;
          lexbuf.lex_start_p <- start_p;
          add_comment lexbuf ("(* refill " ^ Buffer.contents buf ^ " *)");
          main lexbuf
      | _ -> IDENT s }
  | '"'
    { reset_string_buffer();
      string lexbuf;
      STRING (Printf.sprintf "\"%s\"" (get_stored_string())) }
  (* Character literals — from upstream *)
  | "'" [^ '\\'] "'"
    { CHAR (Lexing.lexeme lexbuf) }
  | "'" '\\' backslash_escapes "'"
    { CHAR (Lexing.lexeme lexbuf) }
  | "'" '\\' (['0'-'9'] ['0'-'9'] ['0'-'9']) "'"
    { CHAR (Lexing.lexeme lexbuf) }
  | "'" '\\' 'o' (['0'-'3'] ['0'-'7'] ['0'-'7']) "'"
    { CHAR (Lexing.lexeme lexbuf) }
  | "'" '\\' 'x' (['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F']) "'"
    { CHAR (Lexing.lexeme lexbuf) }
  | '{'
    { let start_p = lexbuf.lex_start_p in
      let buf = Buffer.create 256 in
      Buffer.add_char buf '{';
      action buf [] lexbuf;
      lexbuf.lex_start_p <- start_p;
      OCAML_CODE (Buffer.contents buf) }
  | '|' { PIPE } | '=' { EQUAL } | '#' { HASH } | '*' { STAR }
  | '+' { PLUS } | '?' { QUESTION } | '(' { LPAREN } | ')' { RPAREN }
  | '[' { LBRACKET } | ']' { RBRACKET } | '^' { CARET } | '-' { DASH }
  | eof { EOF }
  | _ as c
    { error lexbuf (Printf.sprintf "unexpected character: %C" c) }

(* String parsing — from upstream *)
and string = parse
  | '"' { () }
  | '\\' ('\013'* '\010') ([' ' '\009'] * as spaces)
    { incr_loc lexbuf (String.length spaces); string lexbuf }
  | '\\' (backslash_escapes as c)
    { store_string_char (char_for_backslash c); string lexbuf }
  | '\\' (['0'-'9'] as c) (['0'-'9'] as d) (['0'-'9'] as u)
    { store_string_char (Char.chr (decimal_code c d u)); string lexbuf }
  | '\\' 'o' (['0'-'3'] as c) (['0'-'7'] as d) (['0'-'7'] as u)
    { store_string_char (char_for_octal_code c d u); string lexbuf }
  | '\\' 'x' (['0'-'9' 'a'-'f' 'A'-'F'] as d) (['0'-'9' 'a'-'f' 'A'-'F'] as u)
    { store_string_char (char_for_hexadecimal_code d u); string lexbuf }
  | '\\' (_ as c)
    { store_string_char '\\'; store_string_char c; string lexbuf }
  | eof { error lexbuf "unterminated string" }
  | '\013'* '\010' as s
    { store_string_chars s; incr_loc lexbuf 0; string lexbuf }
  | _ as c
    { store_string_char c; string lexbuf }

(* Quoted string — from upstream *)
and quoted_string buf delim = parse
  | '\013'* '\010'
    { incr_loc lexbuf 0; Buffer.add_char buf '\n';
      quoted_string buf delim lexbuf }
  | eof { error lexbuf "unterminated quoted string" }
  | '|' (lowercase* as delim') '}'
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      if delim <> delim' then quoted_string buf delim lexbuf }
  | _ as c
    { Buffer.add_char buf c; quoted_string buf delim lexbuf }

(* Comment — from upstream, with buf for text collection *)
and comment buf depth = parse
  | "(*"
    { Buffer.add_string buf "(*"; comment buf (depth + 1) lexbuf }
  | "*)"
    { Buffer.add_string buf "*)";
      if depth > 0 then comment buf (depth - 1) lexbuf }
  | '"'
    { Buffer.add_char buf '"';
      reset_string_buffer(); string_in_comment buf lexbuf;
      comment buf depth lexbuf }
  | '{' ('%' '%'? extattrident blank*)? (lowercase* as delim) "|"
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      quoted_string buf delim lexbuf;
      comment buf depth lexbuf }
  | "'"
    { Buffer.add_char buf '\'';
      skip_char buf lexbuf;
      comment buf depth lexbuf }
  | eof { error lexbuf "unterminated comment" }
  | '\010'
    { incr_loc lexbuf 0; Buffer.add_char buf '\n';
      comment buf depth lexbuf }
  | ocaml_ident as s
    { Buffer.add_string buf s; comment buf depth lexbuf }
  | _ as c
    { Buffer.add_char buf c; comment buf depth lexbuf }

(* String inside comment — skip to closing quote *)
and string_in_comment buf = parse
  | '"' { Buffer.add_char buf '"' }
  | '\\' '"'
    { Buffer.add_string buf "\\\""; string_in_comment buf lexbuf }
  | '\\' '\\'
    { Buffer.add_string buf "\\\\"; string_in_comment buf lexbuf }
  | eof { error lexbuf "unterminated string in comment" }
  | '\013'* '\010' as s
    { Buffer.add_string buf s; incr_loc lexbuf 0;
      string_in_comment buf lexbuf }
  | _ as c
    { Buffer.add_char buf c; string_in_comment buf lexbuf }

(* Action — from upstream, with buf + stack-based matching *)
and action buf stk = parse
  | '(' { Buffer.add_char buf '('; action buf ('(' :: stk) lexbuf }
  | '{' { Buffer.add_char buf '{'; action buf ('{' :: stk) lexbuf }
  | ')'
    { Buffer.add_char buf ')';
      match stk with
      | '(' :: stk' -> action buf stk' lexbuf
      | _ -> error lexbuf "unmatched ) in action" }
  | '}'
    { match stk with
      | [] -> Buffer.add_char buf '}'  (* closing brace — done *)
      | '{' :: stk' ->
          Buffer.add_char buf '}'; action buf stk' lexbuf
      | _ -> error lexbuf "unmatched } in action" }
  | '"'
    { Buffer.add_char buf '"';
      action_string buf lexbuf;
      action buf stk lexbuf }
  | '{' ('%' '%'? extattrident blank*)? (lowercase* as delim) "|"
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      quoted_string buf delim lexbuf;
      action buf stk lexbuf }
  | "'"
    { Buffer.add_char buf '\'';
      skip_char buf lexbuf;
      action buf stk lexbuf }
  | "(*"
    { Buffer.add_string buf "(*";
      comment buf 0 lexbuf;
      action buf stk lexbuf }
  | eof { error lexbuf "unterminated action" }
  | '\010'
    { incr_loc lexbuf 0; Buffer.add_char buf '\n';
      action buf stk lexbuf }
  | ocaml_ident as s
    { Buffer.add_string buf s; action buf stk lexbuf }
  | _ as c
    { Buffer.add_char buf c; action buf stk lexbuf }

(* String inside action *)
and action_string buf = parse
  | '"' { Buffer.add_char buf '"' }
  | '\\' '"'
    { Buffer.add_string buf "\\\""; action_string buf lexbuf }
  | '\\' '\\'
    { Buffer.add_string buf "\\\\"; action_string buf lexbuf }
  | '\\' '\'' { Buffer.add_string buf "\\\'" ; action_string buf lexbuf }
  | eof { error lexbuf "unterminated string in action" }
  | '\013'* '\010' as s
    { Buffer.add_string buf s; incr_loc lexbuf 0;
      action_string buf lexbuf }
  | _ as c
    { Buffer.add_char buf c; action_string buf lexbuf }

(* Skip char literal — from upstream *)
and skip_char buf = parse
  | '\\' ? ('\013'* '\010') "'"
    { Buffer.add_string buf (Lexing.lexeme lexbuf); incr_loc lexbuf 1 }
  | [^ '\\' '\'' '\010' '\013'] "'"
    { Buffer.add_string buf (Lexing.lexeme lexbuf) }
  | '\\' _ "'"
    { Buffer.add_string buf (Lexing.lexeme lexbuf) }
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
    { Buffer.add_string buf (Lexing.lexeme lexbuf) }
  | '\\' 'o' ['0'-'7'] ['0'-'7'] ['0'-'7'] "'"
    { Buffer.add_string buf (Lexing.lexeme lexbuf) }
  | '\\' 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] "'"
    { Buffer.add_string buf (Lexing.lexeme lexbuf) }
  | ""
    { () }  (* Not a char literal — just a stray quote *)

(* Skip refill { ... } block *)
and skip_refill buf = parse
  | blank+ { skip_refill buf lexbuf }
  | '{' { Buffer.add_char buf '{'; action buf [] lexbuf }
  | _ as c { Buffer.add_char buf c; skip_refill buf lexbuf }
  | eof { () }
