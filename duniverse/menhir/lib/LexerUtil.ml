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
open Printf

let init filename lexbuf =
  lexbuf.lex_curr_p <- {
    pos_fname = filename;
    pos_lnum  = 1;
    pos_bol   = 0;
    pos_cnum  = 0
  };
  lexbuf

let read filename =
  let c = open_in filename in
  let text = really_input_string c (in_channel_length c) in
  close_in c;
  let lexbuf = Lexing.from_string text in
  text, init filename lexbuf

let newline lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with
    pos_lnum = pos.pos_lnum + 1;
    pos_bol = pos.pos_cnum;
  }

let is_dummy (pos1, pos2) =
  pos1 == dummy_pos || pos2 == dummy_pos

let range ((pos1, pos2) as range) =
  if is_dummy range then
    sprintf "At an unknown location:\n"
  else
    let file = pos1.pos_fname in
    let line = pos1.pos_lnum in
    let char1 = pos1.pos_cnum - pos1.pos_bol in
    let char2 = pos2.pos_cnum - pos1.pos_bol in (* yes, [pos1.pos_bol] *)
    sprintf "File \"%s\", line %d, characters %d-%d:\n"
      file line char1 char2
      (* use [char1 + 1] and [char2 + 1] if *not* using Caml mode *)

let tabulate (type a) (is_eof : a -> bool) (lexer : unit -> a) : unit -> a =
  (* Read tokens from the lexer until we hit an EOF token. *)
  let rec read tokens =
    let token = lexer() in
    let tokens = token :: tokens in
    if is_eof token then
      (* Once done, reverse the list and convert it to an array. *)
      tokens |> List.rev |> Array.of_list
    else
      read tokens
  in
  (* We now have an array of tokens. *)
  let tokens = read [] in
  (* Define a pseudo-lexer that reads from this array. *)
  let i = ref 0 in
  let lexer () =
    (* If this assertion is violated, then the parser is trying to read
       past an EOF token. This should not happen. *)
    assert (!i < Array.length tokens);
    let token = Array.unsafe_get tokens !i in
    i := !i + 1;
    token
  in
  lexer
