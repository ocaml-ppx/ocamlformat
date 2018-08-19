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
open Migrate_ast

(** Concrete syntax. *)
type t = string

let create s = s

let string_between t (l1 : Location.t) (l2 : Location.t) =
  let pos = l1.loc_end.pos_cnum in
  let len = Position.distance l1.loc_end l2.loc_start in
  if len >= 0 then Some (String.sub t ~pos ~len)
  else
    (* can happen e.g. if comment is within a parenthesized expression *)
    None

let string_at t (l : Location.t) =
  let pos = l.loc_start.pos_cnum in
  let len = Location.width l in
  String.sub t ~pos ~len

let merge (l1 : Location.t) ~(sub : Location.t) =
  let base = l1.loc_start.pos_cnum in
  { l1 with
    loc_start= {l1.loc_start with pos_cnum= base + sub.loc_start.pos_cnum}
  ; loc_end= {l1.loc_end with pos_cnum= base + sub.loc_end.pos_cnum} }

let string_from_loc t (l : Location.t) =
  let pos = l.loc_start.pos_cnum in
  let len = Location.width l in
  String.sub t ~pos ~len

let lexbuf_from_loc t (l : Location.t) =
  let s = string_from_loc t l in
  Lexing.from_string s

let tokens_at t ?(filter = fun _ -> true) (l : Location.t) :
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

let find_after t f (loc : Location.t) =
  let loc = {loc with loc_start= loc.loc_end} in
  let pos = ref loc.loc_end.pos_cnum in
  let lexbuf =
    Lexing.from_function (fun bytes available ->
        let to_write = min (String.length t - !pos) available in
        Bytes.From_string.blit ~src:t ~src_pos:!pos ~dst:bytes ~dst_pos:0
          ~len:to_write ;
        pos := !pos + to_write ;
        to_write )
  in
  let rec loop () =
    match Lexer.token lexbuf with
    | Parser.EOF -> None
    | tok ->
        if f tok then
          let sub = Location.curr lexbuf in
          Some (merge loc ~sub)
        else loop ()
  in
  loop ()

let extend_loc_to_include_attributes t (loc : Location.t)
    (l : Parsetree.attributes) =
  let last_loc =
    List.fold l ~init:loc
      ~f:(fun (acc : Location.t)
         (({loc; _}, payload) : Parsetree.attribute)
         ->
        if loc.loc_ghost then acc
        else
          let loc =
            match payload with
            | PStr [] -> loc
            | PStr l -> (List.last_exn l).pstr_loc
            | PSig [] -> loc
            | PSig l -> (List.last_exn l).psig_loc
            | PTyp c -> c.ptyp_loc
            | PPat (p, None) -> p.ppat_loc
            | PPat (_, Some e) -> e.pexp_loc
          in
          if Location.compare_end loc acc <= 0 then acc else loc )
  in
  if phys_equal last_loc loc then loc
  else
    let loc =
      { loc with
        loc_end= {loc.loc_end with pos_cnum= last_loc.loc_end.pos_cnum} }
    in
    let count = ref 0 in
    let l =
      find_after t
        (function
          | RBRACKET ->
              if !count = 0 then true else ( Int.decr count ; false )
          (* It is not clear that an LBRACKET* will ever happen in practice,
             we're just being defensive here. *)
          | LBRACKET | LBRACKETBAR | LBRACKETLESS | LBRACKETGREATER
           |LBRACKETPERCENT | LBRACKETPERCENTPERCENT | LBRACKETAT
           |LBRACKETATAT | LBRACKETATATAT ->
              Int.incr count ; false
          | _ -> false)
        loc
    in
    match l with
    | None -> impossible "Invariant of the token stream"
    | Some e -> {loc with loc_end= e.loc_end}

let loc_between ~(from : Location.t) ~(upto : Location.t) : Location.t =
  {from with loc_start= from.loc_end; loc_end= upto.loc_start}

let tokens_between t ?(filter = fun _ -> true) ~(from : Location.t)
    ~(upto : Location.t) : (Parser.token * Location.t) list =
  tokens_at t ~filter (loc_between ~from ~upto)

let contains_IN_token_between t ~(from : Location.t) ~(upto : Location.t) =
  let filter = function Parser.IN -> true | _ -> false in
  Source_code_position.ascending from.loc_start upto.loc_start < 0
  && not (List.is_empty (tokens_between t ~from ~upto ~filter))

let is_long_pexp_open source {Parsetree.pexp_desc} =
  match pexp_desc with
  | Pexp_open (_, {loc}, {pexp_loc}) ->
      contains_IN_token_between source ~from:loc ~upto:pexp_loc
  | _ -> false

let string_literal t mode (l : Location.t) =
  (* the location of a [string] might include surrounding comments and
     attributes because of [reloc_{exp,pat}] and a [string] can be found in
     attributes payloads. {[ f ((* comments *) "c" [@attributes]) ]} *)
  let toks =
    tokens_at t
      ~filter:(function
        | Parser.STRING (_, None) -> true
        | Parser.LBRACKETAT | Parser.LBRACKETATAT | Parser.LBRACKETATATAT ->
            true
        | _ -> false)
      l
  in
  match toks with
  | [(Parser.STRING (_, None), loc)]
   |(Parser.STRING (_, None), loc)
    :: ( Parser.LBRACKETATATAT, _
       | Parser.LBRACKETATAT, _
       | Parser.LBRACKETAT, _ )
       :: _ ->
      Literal_lexer.string mode (lexbuf_from_loc t loc)
  | _ ->
      user_error "location does not contain a string literal"
        [("text", Sexp.Atom (string_from_loc t l))]

let char_literal t (l : Location.t) =
  (* the location of a [char] might include surrounding comments and
     attributes because of [reloc_{exp,pat}] and a [char] can be found in
     attributes payloads. {[ f ((* comments *) 'c' [@attributes]) ]} *)
  let toks =
    tokens_at t
      ~filter:(function
        | Parser.CHAR _ -> true
        | Parser.LBRACKETAT | Parser.LBRACKETATAT | Parser.LBRACKETATATAT ->
            true
        | _ -> false)
      l
  in
  match toks with
  | [(Parser.CHAR _, loc)]
   |(Parser.CHAR _, loc)
    :: ( Parser.LBRACKETATATAT, _
       | Parser.LBRACKETATAT, _
       | Parser.LBRACKETAT, _ )
       :: _ ->
      Literal_lexer.char (lexbuf_from_loc t loc)
  | _ ->
      user_error "location does not contain a char literal"
        [("text", Sexp.Atom (string_from_loc t l))]

let begins_line t (l : Location.t) =
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

let ends_line t (l : Location.t) =
  let len = String.length t in
  let rec ends_line_ cnum =
    if cnum >= len then true
    else
      match t.[cnum] with
      | '\n' | '\r' -> true
      | c when Char.is_whitespace c -> ends_line_ (cnum + 1)
      | _ -> false
  in
  if Location.compare Location.none l = 0 then false
  else ends_line_ l.loc_end.pos_cnum

let extension_using_sugar ~(name : string Location.loc)
    ~(payload : Parsetree.expression) =
  Source_code_position.ascending name.loc.loc_start
    payload.pexp_loc.loc_start
  > 0
