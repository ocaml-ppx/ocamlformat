(**********************************************************************
 *                                                                    *
 *                            OCamlFormat                             *
 *                                                                    *
 *  Copyright (c) 2017-present, Facebook, Inc.  All rights reserved.  *
 *                                                                    *
 *  This source code is licensed under the MIT license found in the   *
 *  LICENSE file in the root directory of this source tree.           *
 *                                                                    *
 **********************************************************************)

(** Concrete syntax, set by [init]. *)
let source = ref ""

let init s = source := s

let string_between (l1: Location.t) (l2: Location.t) =
  let pos = l1.loc_end.pos_cnum + 1 in
  let len = l2.loc_start.pos_cnum - l1.loc_end.pos_cnum - 1 in
  if len >= 0 then Some (String.sub !source ~pos ~len)
  else
    (* can happen e.g. if comment is within a parenthesized expression *)
    None

let normalize_string mode s =
  let lexbuf = Lexing.from_string s in
  let rec loop () =
    match Lexer.token lexbuf with Parser.STRING _ -> () | _ -> loop ()
  in
  loop () ;
  let start = Lexing.lexeme_start lexbuf in
  let stop = Lexing.lexeme_end lexbuf in
  let s = String.sub s start (stop - start) in
  Literal_lexer.string mode (Lexing.from_string s)

let normalize_char s =
  let lexbuf = Lexing.from_string s in
  let rec loop () =
    match Lexer.token lexbuf with Parser.CHAR _ -> () | _ -> loop ()
  in
  loop () ;
  let start = Lexing.lexeme_start lexbuf in
  let stop = Lexing.lexeme_end lexbuf in
  let s = String.sub s start (stop - start) in
  let s = Literal_lexer.char (Lexing.from_string s) in
  String.sub s 1 (String.length s - 2)

let string_at (l: Location.t) =
  let pos = l.loc_start.pos_cnum in
  let len = l.loc_end.pos_cnum - pos in
  String.sub !source ~pos ~len

let string_literal mode (l: Location.t) =
  let s = string_at l in
  assert (String.length s >= 2) ;
  normalize_string mode s

let char_literal (l: Location.t) =
  let s = string_at l in
  assert (String.length s >= 2) ;
  normalize_char s

let begins_line (l: Location.t) =
  let rec begins_line_ cnum =
    cnum = 0
    ||
    let cnum = cnum - 1 in
    match !source.[cnum] with
    | '\n' | '\r' -> true
    | c when Char.is_whitespace c -> begins_line_ cnum
    | _ -> false
  in
  begins_line_ l.loc_start.pos_cnum

let ends_line (l: Location.t) =
  let rec ends_line_ cnum =
    match !source.[cnum] with
    | '\n' | '\r' -> true
    | c when Char.is_whitespace c -> ends_line_ (cnum + 1)
    | _ -> false
  in
  ends_line_ l.loc_end.pos_cnum

let sub ~pos ~len = String.sub !source ~pos ~len
