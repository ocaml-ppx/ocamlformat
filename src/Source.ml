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

(** Concrete syntax. *)
type t = string

let create s = s

let string_between t ?(inclusive= false) (l1: Location.t) (l2: Location.t) =
  let inclusive_or_not = if inclusive then 0 else 1 in
  let pos = l1.loc_end.pos_cnum + inclusive_or_not in
  let len =
    l2.loc_start.pos_cnum - l1.loc_end.pos_cnum - inclusive_or_not
  in
  if len >= 0 then Some (String.sub t ~pos ~len)
  else
    (* can happen e.g. if comment is within a parenthesized expression *)
    None

let merge (l1: Location.t) ~(sub: Location.t) =
  let base = l1.loc_start.pos_cnum in
  { l1 with
    loc_start= {l1.loc_start with pos_cnum= base + sub.loc_start.pos_cnum}
  ; loc_end= {l1.loc_end with pos_cnum= base + sub.loc_end.pos_cnum} }

let lexbuf_from_loc t (l: Location.t) =
  let pos = l.loc_start.pos_cnum in
  let len = l.loc_end.pos_cnum - pos in
  let s = String.sub t ~pos ~len in
  Lexing.from_string s

let tokens_at t ?(filter= fun _ -> true) (l: Location.t) :
    (Parser.token * Location.t) list =
  let lexbuf = lexbuf_from_loc t l in
  let rec loop acc =
    match Lexer.token lexbuf with
    | Parser.EOF -> List.rev acc
    | tok ->
        if filter tok then
          let sub = Location.curr lexbuf in
          loop ((tok, merge l ~sub) :: acc)
        else loop acc
  in
  loop []

let string_literal t mode (l: Location.t) =
  let toks =
    tokens_at t
      ~filter:(function Parser.STRING (_, None) -> true | _ -> false)
      l
  in
  match toks with
  | [(Parser.STRING (_, None), loc)] ->
      Literal_lexer.string mode (lexbuf_from_loc t loc)
  | _ -> user_error "location does not contain a string literal" []

let char_literal t (l: Location.t) =
  let toks =
    tokens_at t ~filter:(function Parser.CHAR _ -> true | _ -> false) l
  in
  match toks with
  | [(Parser.CHAR _, loc)] -> Literal_lexer.char (lexbuf_from_loc t loc)
  | _ -> user_error "location does not contain a string literal" []

let begins_line t (l: Location.t) =
  let rec begins_line_ cnum =
    cnum = 0
    ||
    let cnum = cnum - 1 in
    match t.[cnum] with
    | '\n' | '\r' -> true
    | c when Char.is_whitespace c -> begins_line_ cnum
    | _ -> false
  in
  begins_line_ l.loc_start.pos_cnum

let ends_line t (l: Location.t) =
  let len = String.length t in
  let rec ends_line_ cnum =
    if cnum >= len then true
    else
      match t.[cnum] with
      | '\n' | '\r' -> true
      | c when Char.is_whitespace c -> ends_line_ (cnum + 1)
      | _ -> false
  in
  ends_line_ l.loc_end.pos_cnum
