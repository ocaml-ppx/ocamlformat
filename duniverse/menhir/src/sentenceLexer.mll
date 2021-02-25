(******************************************************************************)
(*                                                                            *)
(*                                   Menhir                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*              Yann Régis-Gianas, PPS, Université Paris Diderot              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU General Public License version 2, as described in the    *)
(*  file LICENSE.                                                             *)
(*                                                                            *)
(******************************************************************************)

(* This lexer is used to read the sentences provided on the standard input
   channel when [--interpret] is enabled. *)

{

  open Lexing
  open SentenceParser

  (* A short-hand. *)

  let error2 lexbuf =
    Error.error (Positions.lexbuf lexbuf)

}

let newline   = ('\010' | '\013' | "\013\010")

let whitespace = [ ' ' '\t' ';' ]

let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']

let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']

let identchar = ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '0'-'9'] (* '\'' forbidden *)

let autocomment = "##" [^'\010''\013']* newline

let comment = "#" [^'\010''\013']* newline

let skip = newline whitespace* newline

rule lex = parse
  (* An identifier that begins with an lowercase letter is considered a
     non-terminal symbol. It should be a start symbol. *)
  | (lowercase identchar *) as lid
      { NONTERMINAL (lid, lexbuf.lex_start_p, lexbuf.lex_curr_p) }
  (* An identifier that begins with an uppercase letter is considered a
     terminal symbol. *)
  | (uppercase identchar *) as uid
      { TERMINAL (uid, lexbuf.lex_start_p, lexbuf.lex_curr_p) }
  (* Whitespace is ignored. *)
  | whitespace
      { lex lexbuf }
  (* The end of a line is translated to [EOL]. *)
  | newline
      { new_line lexbuf; EOL }
  (* An auto-generated comment is ignored. *)
  | autocomment
      { new_line lexbuf; lex lexbuf }
  (* A manually-written comment is preserved. *)
  | comment as c
      { new_line lexbuf; COMMENT c }
  (* The end of file is translated to [EOF]. *)
  | eof
      { EOF }
  (* A colon. *)
  | ':'
      { COLON }
  | _
      { error2 lexbuf "unexpected character.\n\
                       (I believe I am reading a sentence, but may be off.)" }
