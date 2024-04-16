(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU Library General Public License version 2, with a    *)
(*   special exception on linking, as described in the file LICENSE.          *)
(*                                                                            *)
(******************************************************************************)

open Lexing

(**[init filename lexbuf] initializes the lexing buffer [lexbuf] so
   that the positions that are subsequently read from it refer to the
   file [filename]. It returns [lexbuf]. *)
val init: string -> lexbuf -> lexbuf

(**[read filename] reads the entire contents of the file [filename] and
   returns a pair of this content (a string) and a lexing buffer that
   has been initialized, based on this string. *)
val read: string -> string * lexbuf

(**[newline lexbuf] increments the line counter stored within [lexbuf]. It
   should be invoked by the lexer itself every time a newline character is
   consumed. This allows maintaining a current the line number in [lexbuf]. *)
val newline: lexbuf -> unit

(**[range (startpos, endpos)] prints a textual description of the range
   delimited by the start and end positions [startpos] and [endpos].
   This description is one line long and ends in a newline character.
   This description mentions the file name, the line number, and a range
   of characters on this line. The line number is correct only if [newline]
   has been correctly used, as described dabove. *)
val range: position * position -> string

(**[tabulate is_eof lexer] tabulates the lexer [lexer]: that is, it
   immediately runs this lexer all the way until an EOF token is found, stores
   the tokens in an array in memory, and returns a new lexer which (when
   invoked) reads tokens from this array. The function [lexer] is not allowed
   to raise an exception, and must produce a finite stream of tokens: that is,
   after a finite number of invocations, it must return a token that is
   identified by the function [is_eof] as an EOF token.

   Both the existing lexer [lexer] and the new lexer returned by [tabulate
   is_eof lexer] are functions of type [unit -> 'a], where the type ['a] is
   likely to be instantiated with a triple of a token and two positions, as
   per the revised lexer API described in the module {!Convert}. *)
val tabulate: ('a -> bool) -> (unit -> 'a) -> (unit -> 'a)
