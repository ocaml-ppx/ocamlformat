(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(* This lexer decodes a string that may contain escaped characters. *)

{

let char_for_backslash = function
  | 'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c   -> c

}

rule unescape buffer = parse
| '"'
    { (* The final double quote is skipped. *) }
| '\\' (['\\' '\'' '"' 'n' 't' 'b' 'r' ' '] as c)
    { Buffer.add_char buffer (char_for_backslash c);
      unescape buffer lexbuf }
| _ as c
    { Buffer.add_char buffer c;
      unescape buffer lexbuf }
