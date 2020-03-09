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

type t = string
(** Concrete syntax. *)

let create s = s

let position_before t (pos : Lexing.position) =
  let is_closing c =
    Char.equal c ')' || Char.equal c ']' || Char.equal c '}'
  in
  let maybe_check_alphanum c =
    if Char.is_alphanum t.[pos.pos_cnum] then false else Char.is_alphanum c
  in
  let f _ c =
    maybe_check_alphanum c || Char.is_whitespace c || is_closing c
  in
  let pos_cnum_opt = String.rfindi t ~pos:pos.pos_cnum ~f in
  Option.map pos_cnum_opt ~f:(fun x -> {pos with pos_cnum= x + 1})

let string_between t (p1 : Lexing.position) (p2 : Lexing.position) =
  let pos = p1.pos_cnum in
  let len = Position.distance p1 p2 in
  if
    len < 0 || pos < 0
    (* can happen e.g. if comment is within a parenthesized expression *)
  then None
  else if
    String.length t < pos + len
    (* can happen e.g. if source is not available *)
  then None
  else Some (String.sub t ~pos ~len)

(** Lines between [p1] and [p2]. Lines are represented their (offset, length)
    in the source. The first and last line (containing [p1] and [p2]) are
    ignored. *)
let lines_between t (p1 : Lexing.position) (p2 : Lexing.position) =
  let rec loop acc off lnum =
    if lnum >= p2.pos_lnum then acc (* ignore last line *)
    else
      match String.index_from t off '\n' with
      | None -> acc (* ignore last line *)
      | Some endl -> loop ((off, endl - off) :: acc) (endl + 1) (lnum + 1)
  in
  (* ignore first line *)
  match String.index_from t p1.pos_cnum '\n' with
  | None -> []
  | Some endl -> loop [] (endl + 1) (p1.pos_lnum + 1)

(** Returns the index of the first char that [f] match. Only consider [len]
    characters starting at [pos]. *)
let string_lcontains ~pos ~len s ~f =
  let end_ = pos + len in
  let rec loop i =
    if i >= end_ then None else if f s.[i] then Some i else loop (i + 1)
  in
  loop pos

let empty_line_between t p1 p2 =
  let non_whitespace c = not (Char.is_whitespace c) in
  let is_line_empty (off, len) =
    Option.is_none (string_lcontains ~pos:off ~len t ~f:non_whitespace)
  in
  Lexing.(p2.pos_lnum - p1.pos_lnum) > 1
  && List.exists (lines_between t p1 p2) ~f:is_line_empty

let string_at t loc_start loc_end =
  let pos = loc_start.Lexing.pos_cnum
  and len = Position.distance loc_start loc_end in
  if String.length t < pos + len || pos < 0 || len < 0 then ""
  else String.sub t ~pos ~len

let has_cmt_same_line_after t (loc : Location.t) =
  let loc_start = {loc.loc_end with pos_cnum= loc.loc_end.pos_cnum} in
  let loc_end = {loc.loc_end with pos_cnum= loc_start.pos_cnum + 20} in
  let str = string_at t loc_start loc_end in
  if String.is_empty str then false
  else if Char.equal str.[0] '\n' then false
  else
    let str = String.lstrip str in
    String.is_prefix str ~prefix:"(*"

let lexbuf_set_pos lexbuf pos =
  lexbuf.Lexing.lex_abs_pos <- pos.Lexing.pos_cnum ;
  lexbuf.lex_curr_p <- pos

let tokens_between t ?(filter = fun _ -> true) loc_start loc_end =
  let s = string_at t loc_start loc_end in
  let lexbuf = Lexing.from_string s in
  lexbuf_set_pos lexbuf loc_start ;
  let rec loop acc =
    match Lexer.token lexbuf with
    | Parser.EOF -> List.rev acc
    | tok ->
        if filter tok then loop ((tok, Location.curr lexbuf) :: acc)
        else loop acc
  in
  loop []

let tokens_at t ?filter (l : Location.t) =
  tokens_between t ?filter l.loc_start l.loc_end

let find_after t f (loc : Location.t) =
  let pos_start = loc.loc_end in
  let lexbuf =
    let pos = ref pos_start.pos_cnum in
    Lexing.from_function (fun bytes available ->
        let to_write = min (String.length t - !pos) available in
        Bytes.From_string.blit ~src:t ~src_pos:!pos ~dst:bytes ~dst_pos:0
          ~len:to_write ;
        pos := !pos + to_write ;
        to_write)
  in
  lexbuf_set_pos lexbuf pos_start ;
  let rec loop () =
    match Lexer.token lexbuf with
    | Parser.EOF -> None
    | tok -> if f tok then Some (Location.curr lexbuf) else loop ()
  in
  loop ()

let extend_loc_to_include_attributes t (loc : Location.t)
    (l : Parsetree.attributes) =
  if
    List.for_all l ~f:(fun ({attr_loc; _} : Parsetree.attribute) ->
        Location.compare Location.none attr_loc <> 0)
  then
    (* Starting with OCaml 4.08, attributes have locations *)
    let loc_end =
      List.fold l ~init:loc
        ~f:(fun acc ({attr_loc; _} : Parsetree.attribute) ->
          if Location.compare_end attr_loc acc <= 0 then acc else attr_loc)
    in
    if phys_equal loc_end loc then loc
    else
      { loc with
        Location.loc_end=
          {loc.loc_end with pos_cnum= loc_end.loc_end.pos_cnum} }
  else
    let last_loc =
      List.fold l ~init:loc
        ~f:(fun (acc : Location.t)
                ({attr_name= {loc; _}; attr_payload= payload; _} :
                  Parsetree.attribute)
                ->
          if loc.loc_ghost then acc
          else
            let loc =
              match payload with
              | PStr [] -> loc
              | PStr l -> (List.last_exn l).Parsetree.pstr_loc
              | PSig [] -> loc
              | PSig l -> (List.last_exn l).Parsetree.psig_loc
              | PTyp c -> c.ptyp_loc
              | PPat (p, None) -> p.ppat_loc
              | PPat (_, Some e) -> e.pexp_loc
            in
            if Location.compare_end loc acc <= 0 then acc else loc)
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
                if !count = 0 then true else (Int.decr count ; false)
            (* It is not clear that an LBRACKET* will ever happen in
               practice, we're just being defensive here. *)
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

let contains_token_between t ~(from : Location.t) ~(upto : Location.t) tok =
  let filter = Poly.( = ) tok in
  let from = from.loc_start and upto = upto.loc_start in
  Source_code_position.ascending from upto < 0
  && not (List.is_empty (tokens_between t ~filter from upto))

let is_long_pexp_open source {Parsetree.pexp_desc; _} =
  match pexp_desc with
  | Pexp_open ({popen_loc= from; _}, {pexp_loc= upto; _}) ->
      contains_token_between source ~from ~upto Parser.IN
  | _ -> false

let is_long_pmod_functor source Parsetree.{pmod_desc; pmod_loc= from; _} =
  match pmod_desc with
  | Pmod_functor (_, {pmod_loc= upto; _}) ->
      contains_token_between source ~from ~upto Parser.FUNCTOR
  | _ -> false

let lexbuf_from_loc t (l : Location.t) =
  let s = string_at t l.loc_start l.loc_end in
  Lexing.from_string s

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
      Some (Literal_lexer.string mode (lexbuf_from_loc t loc))
  | _ -> None

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
      Some (Literal_lexer.char (lexbuf_from_loc t loc))
  | _ -> None

let begins_line ?(ignore_spaces = true) t (l : Location.t) =
  let rec begins_line_ cnum =
    cnum = 0
    ||
    let cnum = cnum - 1 in
    match t.[cnum] with
    | '\n' | '\r' -> true
    | c when Char.is_whitespace c && ignore_spaces -> begins_line_ cnum
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

let typed_expression (typ : Parsetree.core_type)
    (expr : Parsetree.expression) =
  if Location.compare_start typ.ptyp_loc expr.pexp_loc < 0 then `Type_first
  else `Expr_first

let typed_pattern (typ : Parsetree.core_type) (pat : Parsetree.pattern) =
  if Location.compare_start typ.ptyp_loc pat.ppat_loc < 0 then `Type_first
  else `Pat_first

let loc_of_underscore t flds (ppat_loc : Location.t) =
  let end_last_field =
    match List.last flds with
    | Some (_, p) -> p.Parsetree.ppat_loc.loc_end
    | None -> ppat_loc.loc_start
  in
  let loc_underscore = {ppat_loc with loc_start= end_last_field} in
  let filter = function Parser.UNDERSCORE -> true | _ -> false in
  let tokens = tokens_at t ~filter loc_underscore in
  Option.map (List.hd tokens) ~f:snd
