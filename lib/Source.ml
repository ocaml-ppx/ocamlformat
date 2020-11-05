(**************************************************************************)
(*                                                                        *)
(*                              OCamlFormat                               *)
(*                                                                        *)
(*            Copyright (c) Facebook, Inc. and its affiliates.            *)
(*                                                                        *)
(*      This source code is licensed under the MIT license found in       *)
(*      the LICENSE file in the root directory of this source tree.       *)
(*                                                                        *)
(**************************************************************************)

open Migrate_ast

(** Concrete syntax. *)
type t = {text: string; tokens: (Parser.token * Location.t) array}

let create text =
  let lexbuf = Lexing.from_string text in
  let rec loop acc =
    match Lexer.token lexbuf with
    | Parser.EOF -> Array.of_list (List.rev acc)
    | tok -> loop ((tok, Location.of_lexbuf lexbuf) :: acc)
  in
  {text; tokens= loop []}

let position_before (t : t) (pos : Lexing.position) =
  let is_closing c =
    Char.equal c ')' || Char.equal c ']' || Char.equal c '}'
  in
  let maybe_check_alphanum c =
    if Char.is_alphanum t.text.[pos.pos_cnum] then false
    else Char.is_alphanum c
  in
  let f _ c =
    maybe_check_alphanum c || Char.is_whitespace c || is_closing c
  in
  let pos_cnum_opt = String.rfindi t.text ~pos:pos.pos_cnum ~f in
  Option.map pos_cnum_opt ~f:(fun x -> {pos with pos_cnum= x + 1})

let string_between (t : t) (p1 : Lexing.position) (p2 : Lexing.position) =
  let pos = p1.pos_cnum in
  let len = Position.distance p1 p2 in
  if
    len < 0 || pos < 0
    (* can happen e.g. if comment is within a parenthesized expression *)
  then None
  else if
    String.length t.text < pos + len
    (* can happen e.g. if source is not available *)
  then None
  else Some (String.sub t.text ~pos ~len)

(** Lines between [p1] and [p2]. Lines are represented their (offset, length)
    in the source. The first and last line (containing [p1] and [p2]) are
    ignored. *)
let lines_between (t : t) (p1 : Lexing.position) (p2 : Lexing.position) =
  let rec loop acc off lnum =
    if lnum >= p2.pos_lnum then acc (* ignore last line *)
    else
      match String.index_from t.text off '\n' with
      | None -> acc (* ignore last line *)
      | Some endl -> loop ((off, endl - off) :: acc) (endl + 1) (lnum + 1)
  in
  (* ignore first line *)
  match String.index_from t.text p1.pos_cnum '\n' with
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

let empty_line_between (t : t) p1 p2 =
  let non_whitespace c = not (Char.is_whitespace c) in
  let is_line_empty (off, len) =
    Option.is_none (string_lcontains ~pos:off ~len t.text ~f:non_whitespace)
  in
  Lexing.(p2.pos_lnum - p1.pos_lnum) > 1
  && List.exists (lines_between t p1 p2) ~f:is_line_empty

let sub (t : t) ~pos ~len =
  if String.length t.text < pos + len || pos < 0 || len < 0 then ""
  else String.sub t.text ~pos ~len

let string_at (t : t) loc_start loc_end =
  let pos = loc_start.Lexing.pos_cnum
  and len = Position.distance loc_start loc_end in
  sub t ~pos ~len

let has_cmt_same_line_after (t : t) (loc : Location.t) =
  let loc_start = {loc.loc_end with pos_cnum= loc.loc_end.pos_cnum} in
  let loc_end = {loc.loc_end with pos_cnum= loc_start.pos_cnum + 20} in
  let str = string_at t loc_start loc_end in
  if String.is_empty str then false
  else if Char.equal str.[0] '\n' then false
  else
    let str = String.lstrip str in
    String.is_prefix str ~prefix:"(*"

let find_first_token_after t pos =
  Array.binary_search t.tokens
    ~compare:(fun (_, elt) pos -> Position.compare elt.Location.loc_start pos)
    `First_greater_than_or_equal_to pos

let tokens_between (t : t) ~filter loc_start loc_end =
  match find_first_token_after t loc_start with
  | None -> []
  | Some i ->
      let rec loop i acc =
        if i >= Array.length t.tokens then List.rev acc
        else
          let ((tok, tok_loc) as x) = t.tokens.(i) in
          if Position.compare tok_loc.Location.loc_end loc_end > 0 then
            List.rev acc
          else
            let acc = if filter tok then x :: acc else acc in
            loop (i + 1) acc
      in
      loop i []

let tokens_at t ~filter (l : Location.t) : (Parser.token * Location.t) list =
  tokens_between t ~filter l.loc_start l.loc_end

let last_token_before t pos =
  let len = Array.length t.tokens in
  match (find_first_token_after t pos, len) with
  | None, 0 | Some 0, _ -> None
  | None, i | Some i, _ -> Some t.tokens.(i - 1)

let find_after t f (loc : Location.t) =
  match find_first_token_after t loc.loc_end with
  | None -> None
  | Some i ->
      let rec loop i =
        if i >= Array.length t.tokens then None
        else
          let tok, tok_loc = t.tokens.(i) in
          if f tok then Some tok_loc else loop (i + 1)
      in
      loop i

let extend_loc_to_include_attributes (t : t) (loc : Location.t)
    (l : Parsetree.attributes) =
  if
    List.exists l ~f:(fun {attr_loc; attr_name; _} ->
        Location.compare attr_name.loc attr_loc <> 0 )
  then
    (* Starting with OCaml 4.08, attributes have locations *)
    let loc_end =
      List.fold l ~init:loc
        ~f:(fun acc ({attr_loc; _} : Parsetree.attribute) ->
          if Location.compare_end attr_loc acc <= 0 then acc else attr_loc )
    in
    if phys_equal loc_end loc then loc
    else
      { loc with
        Location.loc_end=
          {loc.loc_end with pos_cnum= loc_end.loc_end.pos_cnum} }
  else
    let last_loc =
      List.fold l ~init:loc
        ~f:(fun
             (acc : Location.t)
             ({attr_name= {loc; _}; attr_payload= payload; _} :
               Parsetree.attribute )
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
                if !count = 0 then true else (Int.decr count ; false)
            (* It is not clear that an LBRACKET* will ever happen in
               practice, we're just being defensive here. *)
            | LBRACKET | LBRACKETBAR | LBRACKETLESS | LBRACKETGREATER
             |LBRACKETPERCENT | LBRACKETPERCENTPERCENT | LBRACKETAT
             |LBRACKETATAT | LBRACKETATATAT ->
                Int.incr count ; false
            | _ -> false )
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

let is_long_functor_syntax (t : t) ~from = function
  | Parsetree.Unit -> false
  | Parsetree.Named ({loc= upto; _}, _) -> (
      if Ocaml_version.(compare Parse.parser_version Releases.v4_12) < 0 then
        (* before 4.12 the functor keyword is the first token of the functor
           parameter *)
        contains_token_between t ~from ~upto Parser.FUNCTOR
      else
        (* since 4.12 the functor keyword is just before the loc of the
           functor parameter *)
        match last_token_before t from.loc_start with
        | Some (Parser.FUNCTOR, _) -> true
        | _ -> false )

let is_long_pmod_functor t Parsetree.{pmod_desc; pmod_loc= from; _} =
  match pmod_desc with
  | Pmod_functor (fp, _) -> is_long_functor_syntax t ~from fp
  | _ -> false

let is_long_pmty_functor t Parsetree.{pmty_desc; pmty_loc= from; _} =
  match pmty_desc with
  | Pmty_functor (fp, _) -> is_long_functor_syntax t ~from fp
  | _ -> false

let string_at_loc t (l : Location.t) = string_at t l.loc_start l.loc_end

let string_literal t mode (l : Location.t) =
  (* the location of a [string] might include surrounding comments and
     attributes because of [reloc_{exp,pat}] and a [string] can be found in
     attributes payloads. {[ f ((* comments *) "c" [@attributes]) ]} *)
  let toks =
    tokens_at t
      ~filter:(function
        | Parser.STRING (_, _, None) -> true
        | Parser.LBRACKETAT | Parser.LBRACKETATAT | Parser.LBRACKETATATAT ->
            true
        | _ -> false )
      l
  in
  match toks with
  | [(Parser.STRING (_, _, None), loc)]
   |(Parser.STRING (_, _, None), loc)
    :: ( Parser.LBRACKETATATAT, _
       | Parser.LBRACKETATAT, _
       | Parser.LBRACKETAT, _ )
       :: _ ->
      Option.value_exn ~message:"Parse error while reading string literal"
        (Literal_lexer.string mode (string_at_loc t loc))
  | _ -> impossible "Pconst_string is only produced by string literals"

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
        | _ -> false )
      l
  in
  match toks with
  | [(Parser.CHAR _, loc)]
   |(Parser.CHAR _, loc)
    :: ( Parser.LBRACKETATATAT, _
       | Parser.LBRACKETATAT, _
       | Parser.LBRACKETAT, _ )
       :: _ ->
      (Option.value_exn ~message:"Parse error while reading char literal")
        (Literal_lexer.char (string_at_loc t loc))
  | _ -> impossible "Pconst_char is only produced by char literals"

let begins_line ?(ignore_spaces = true) t (l : Location.t) =
  let rec begins_line_ cnum =
    cnum = 0
    ||
    let cnum = cnum - 1 in
    match t.text.[cnum] with
    | '\n' | '\r' -> true
    | c when Char.is_whitespace c && ignore_spaces -> begins_line_ cnum
    | _ -> false
  in
  begins_line_ l.loc_start.pos_cnum

let ends_line t (l : Location.t) =
  let len = String.length t.text in
  let rec ends_line_ cnum =
    if cnum >= len then true
    else
      match t.text.[cnum] with
      | '\n' | '\r' -> true
      | c when Char.is_whitespace c -> ends_line_ (cnum + 1)
      | _ -> false
  in
  if Location.compare Location.none l = 0 then false
  else ends_line_ l.loc_end.pos_cnum

let extension_using_sugar ~(name : string Location.loc)
    ~(payload : Location.t) =
  Source_code_position.ascending name.loc.loc_start payload.loc_start > 0

let type_constraint_is_first typ loc =
  Location.compare_start typ.Parsetree.ptyp_loc loc < 0

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

let locs_of_interval source loc =
  let toks =
    tokens_at source loc ~filter:(function
      | CHAR _ | DOTDOT | INT _ | STRING _ | FLOAT _ -> true
      | _ -> false )
  in
  match toks with
  | [ ((CHAR _ | INT _ | STRING _ | FLOAT _), loc1)
    ; (DOTDOT, _)
    ; ((CHAR _ | INT _ | STRING _ | FLOAT _), loc2) ] ->
      (loc1, loc2)
  | _ ->
      impossible
        "Ppat_interval is only produced by the sequence of 3 tokens: \
         CONSTANT-DOTDOT-CONSTANT "

let loc_of_constant t loc (cst : Parsetree.constant) =
  let loc_of_tok filter =
    match tokens_at t loc ~filter with [(_, loc)] -> loc | _ -> loc
  in
  match cst with
  | Pconst_string _ -> loc_of_tok (function STRING _ -> true | _ -> false)
  | Pconst_char _ -> loc_of_tok (function CHAR _ -> true | _ -> false)
  | Pconst_integer _ -> loc_of_tok (function INT _ -> true | _ -> false)
  | Pconst_float _ -> loc_of_tok (function FLOAT _ -> true | _ -> false)

let loc_of_pat_constant t (p : Parsetree.pattern) =
  match p.ppat_desc with
  | Ppat_constant cst ->
      loc_of_constant t (Location.smallest p.ppat_loc p.ppat_loc_stack) cst
  | _ -> impossible "loc_of_pat_constant is only called on constants"

let loc_of_expr_constant t (e : Parsetree.expression) =
  match e.pexp_desc with
  | Pexp_constant cst ->
      loc_of_constant t (Location.smallest e.pexp_loc e.pexp_loc_stack) cst
  | _ -> impossible "loc_of_expr_constant is only called on constants"

let is_quoted_string t loc =
  let toks =
    tokens_at t loc ~filter:(function
      | QUOTED_STRING_ITEM _ | QUOTED_STRING_EXPR _ -> true
      | _ -> false )
  in
  not (List.is_empty toks)

let loc_of_first_token_at t loc kwd =
  match tokens_at t loc ~filter:(Poly.( = ) kwd) with
  | [] -> None
  | (_, loc) :: _ -> Some loc
