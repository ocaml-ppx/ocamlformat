(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(* The front-end. This module performs a series of toplevel side effects. *)

(* ------------------------------------------------------------------------- *)

(* Reading a grammar from a file. *)

let load_grammar_from_contents filename contents =
  InputFile.new_input_file filename;
  InputFile.with_file_contents contents (fun () ->
    let open Lexing in
    let lexbuf = Lexing.from_string contents in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
    (* the grammar: *)
    { (Driver.grammar Lexer.main lexbuf)
      with Syntax.pg_filename = filename }
  )

let check_filename filename =
  let validExt = Settings.extension in
  if not (Filename.check_suffix filename validExt) then
    Error.error []
      "argument file names should end in %s. \"%s\" is not accepted."
      validExt filename

let load_grammar_from_file filename : Syntax.partial_grammar =
  check_filename filename;
  try
    let contents = IO.read_whole_file filename in
    load_grammar_from_contents filename contents
  with Sys_error msg ->
    Error.error [] "%s" msg
