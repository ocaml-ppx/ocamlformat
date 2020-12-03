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

let create ~text ~tokens =
  let tokens =
    List.filter tokens ~f:(fun (tok, _) ->
        match tok with Parser.EOL | Parser.EOF -> false | _ -> true )
  in
  {text; tokens= Array.of_list tokens}

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

let string_at t (l : Location.t) =
  let pos = l.loc_start.Lexing.pos_cnum
  and len = Position.distance l.loc_start l.loc_end in
  String.sub t.text ~pos ~len

let find_token t k pos =
  Array.binary_search t.tokens
    ~compare:(fun (_, elt) pos -> Position.compare elt.Location.loc_start pos)
    k pos

let tokens_between (t : t) ~filter loc_start loc_end =
  match find_token t `First_greater_than_or_equal_to loc_start with
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

let empty_line_between (t : t) p1 p2 =
  let l = tokens_between t ~filter:(function _ -> true) p1 p2 in
  let rec loop (prev : Lexing.position) (l : (_ * Location.t) list) =
    match l with
    | [] -> p2.pos_lnum - prev.pos_lnum > 1
    | (_tok, x) :: xs ->
        x.loc_start.pos_lnum - prev.pos_lnum > 1 || loop x.loc_end xs
  in
  loop p1 l

let tokens_at t ~filter (l : Location.t) : (Parser.token * Location.t) list =
  tokens_between t ~filter l.loc_start l.loc_end

let find_token_before t ~filter pos =
  match find_token t `Last_strictly_less_than pos with
  | None -> None
  | Some i ->
      let rec loop i =
        if i < 0 then None
        else
          let ((tok, _) as elt) = t.tokens.(i) in
          if filter tok then Some elt else loop (i - 1)
      in
      loop i

let find_token_after t ~filter pos =
  match find_token t `First_greater_than_or_equal_to pos with
  | None -> None
  | Some i ->
      let rec loop i =
        if i >= Array.length t.tokens then None
        else
          let ((tok, _) as elt) = t.tokens.(i) in
          if filter tok then Some elt else loop (i + 1)
      in
      loop i

let has_cmt_same_line_after t (loc : Location.t) =
  match find_token_after t ~filter:(fun _ -> true) loc.loc_end with
  | None -> false
  | Some ((COMMENT _ | DOCSTRING _), nloc) ->
      nloc.loc_start.pos_lnum = loc.loc_end.pos_lnum
  | Some _ -> false

let extend_loc_to_include_attributes (loc : Location.t)
    (l : Parsetree.attributes) =
  let loc_end =
    List.fold l ~init:loc
      ~f:(fun acc ({attr_loc; _} : Parsetree.attribute) ->
        if Location.compare_end attr_loc acc <= 0 then acc else attr_loc )
  in
  if phys_equal loc_end loc then loc
  else
    {loc with loc_end= {loc.loc_end with pos_cnum= loc_end.loc_end.pos_cnum}}

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
        match
          find_token_before t
            ~filter:(function COMMENT _ | DOCSTRING _ -> false | _ -> true)
            from.loc_start
        with
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
        (Literal_lexer.string mode (string_at t loc))
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
        (Literal_lexer.char (string_at t loc))
  | _ -> impossible "Pconst_char is only produced by char literals"

let begins_line ?(ignore_spaces = true) t (l : Location.t) =
  if not ignore_spaces then Position.column l.loc_start = 0
  else
    match find_token_before t ~filter:(fun _ -> true) l.loc_start with
    | None -> true
    | Some (_, prev) ->
        assert (Location.compare prev l < 0) ;
        prev.loc_end.pos_lnum < l.loc_start.pos_lnum

let ends_line t (l : Location.t) =
  match find_token_after t ~filter:(fun _ -> true) l.loc_end with
  | None -> true
  | Some (_, next) ->
      assert (Location.compare next l > 0) ;
      next.loc_start.pos_lnum > l.loc_end.pos_lnum

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
  let filter : Parser.token -> bool =
    match cst with
    | Pconst_string _ -> ( function STRING _ -> true | _ -> false )
    | Pconst_char _ -> ( function CHAR _ -> true | _ -> false )
    | Pconst_integer _ -> ( function INT _ -> true | _ -> false )
    | Pconst_float _ -> ( function FLOAT _ -> true | _ -> false )
  in
  match tokens_at t loc ~filter with [(_, loc)] -> loc | _ -> loc

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

type column_diff = Before | Same | After

type attached = bool

type prev = Same_line | After of attached * column_diff

type next = Same_line | Before of attached * column_diff

type 'a relative_info = {position: 'a; tokens_between: Parser.token list}

let tokens_no_cmt src ~(fst_loc : Location.t) ~(snd_loc : Location.t) =
  let filter = function Parser.COMMENT _ -> false | _ -> true in
  tokens_between src fst_loc.loc_end snd_loc.loc_start ~filter
  |> List.map ~f:fst

let column_diff ~(curr : Location.t) ~(other : Location.t) : column_diff =
  let col = Position.column in
  let cmp = col curr.loc_start - col other.loc_start in
  if cmp < 0 then Before else if cmp = 0 then Same else After

let relative_info_from_prev src ~(curr : Location.t) ~(prev : Location.t) =
  let column_diff = column_diff ~curr ~other:prev in
  let position =
    match curr.loc_start.pos_lnum - prev.loc_end.pos_lnum with
    | 0 -> (Same_line : prev)
    | 1 -> After (true, column_diff)
    | _ -> After (false, column_diff)
  in
  let tokens_between = tokens_no_cmt src ~fst_loc:prev ~snd_loc:curr in
  {position; tokens_between}

let relative_info_from_next src ~(curr : Location.t) ~(next : Location.t) =
  let column_diff = column_diff ~curr ~other:next in
  let position =
    match next.loc_start.pos_lnum - curr.loc_end.pos_lnum with
    | 0 -> Same_line
    | 1 -> Before (true, column_diff)
    | _ -> Before (false, column_diff)
  in
  let tokens_between = tokens_no_cmt src ~fst_loc:curr ~snd_loc:next in
  {position; tokens_between}

type cmt_placement = After_prev | Before_next

let can_attach_cmt ~before (tk, _) =
  let open Parser in
  match tk with
  (* keywords *)
  | WITH | WHILE | WHEN | VIRTUAL | VAL | UNDERSCORE | TYPE | TRY | TRUE
   |TO | THEN | STRUCT | SIG | REC | PRIVATE | OPEN | OF | OBJECT | NONREC
   |NEW | MUTABLE | MODULE | METHOD | MATCH | LETOP _ | LET | LAZY
   |INITIALIZER | INHERIT | INCLUDE | IN | IF | HASHOP _ | FUNCTOR
   |FUNCTION | FUN | FOR | ASSERT | AS | ANDOP _ | AND | FALSE | EXTERNAL
   |EXCEPTION | CLASS | BEGIN | DONE | DO | END | ELSE | DOWNTO ->
      true
  (* non-terminal *)
  | UIDENT _ | STRING _ | LIDENT _ | LABEL _ | INT _ | FLOAT _ | CHAR _
   |COMMENT _ | DOCSTRING _ | QUOTED_STRING_ITEM _ | QUOTED_STRING_EXPR _ ->
      true
  (* opening symbols allowing comments *)
  | LPAREN | LBRACKETLESS | LBRACKETGREATER | LBRACKETBAR | LBRACKET
   |LBRACELESS | LBRACE | BACKQUOTE | LBRACKETATATAT ->
      before
  (* opening symbols not allowing comments *)
  | LBRACKETPERCENTPERCENT | LBRACKETPERCENT | LBRACKETATAT | LBRACKETAT ->
      false
  (* closing symbols *)
  | RPAREN | RBRACKET | RBRACE | GREATERRBRACKET | GREATERRBRACE
   |BARRBRACKET ->
      not before
  (* infix operators, used to accept comments, but let's change that *)
  | INFIXOP4 _ | INFIXOP3 _ | INFIXOP2 _ | INFIXOP1 _ | INFIXOP0 _ -> false
  (* other symbols and operators *)
  | TILDE | STAR | SEMISEMI | SEMI | QUOTE | QUESTION | PREFIXOP _ | PLUSEQ
   |PLUSDOT | PLUS | PERCENT | OR | OPTLABEL _ | MINUSGREATER | MINUSDOT
   |MINUS | LESSMINUS | LESS | HASH | GREATER | EQUAL | EOL | EOF | DOTDOT
   |DOT | CONSTRAINT | COMMA | COLONGREATER | COLONEQUAL | COLONCOLON
   |COLON | DOTOP _ | BARBAR | BAR | BANG | AMPERSAND | AMPERAMPER ->
      false

let is_infix_op tk =
  let open Parser in
  match tk with
  | INFIXOP0 _ | INFIXOP1 _ | INFIXOP2 _ | INFIXOP3 _ | INFIXOP4 _ -> true
  | _ -> false

let is_bar = function Parser.BAR, _ -> true | _ -> false

let cmt_placement ~(prev : prev relative_info) ~prev_loc
    ~(next : next relative_info) ~next_loc src =
  match (prev.tokens_between, next.tokens_between) with
  | [], [] -> (
      let filter _ = true in
      match
        (tokens_at src prev_loc ~filter, tokens_at src next_loc ~filter)
      with
      | (_ :: _ as ptokens), ntoken :: _ -> (
          let ptoken = List.last_exn ptokens in
          if can_attach_cmt ~before:true ntoken then
            if can_attach_cmt ~before:false ptoken then
              match (prev.position, next.position) with
              | After (false, _), Before (false, _) -> Before_next
              (* Floating comments between toplevel items must be attached to
                 the next item, otherwise group of comments are split. *)
              | After (true, Same), Before (false, Same) -> Before_next
              | After (true, _), Before (false, _) -> After_prev
              | After (false, _), Before (true, _) -> Before_next
              | After (true, c1), Before (true, c2) -> (
                match (c1, c2) with
                | Before, _ -> Before_next
                | After, Same -> Before_next
                | After, _ -> After_prev
                | Same, Same -> Before_next
                | Same, _ -> After_prev )
              | Same_line, Before (false, _) -> After_prev
              (* In record fields, the `;` is part of the item and so is not
                 in the [tokens_between] list. *)
              | Same_line, Before (true, Same) -> Before_next
              | _ -> (
                (* Comments after infix operators are attached before the
                   following elements, for legacy reasons (would need to be
                   fixed one day). *)
                match tokens_at src prev_loc ~filter:(fun _ -> true) with
                | [(tk, _)] when is_infix_op tk -> Before_next
                | [(Parser.STRING _, _)] -> After_prev
                | [(Parser.LIDENT _, _)] -> After_prev
                | [(Parser.(TRUE | FALSE), _)] -> After_prev
                | [(_, _)] -> Before_next
                | _ -> After_prev )
            else Before_next
          else
            (* if the comment is vertically aligned with the bar of a pattern *)
            match (prev.position, next.position) with
            | After (_, (Before | Same)), Before (_, (Before | Same))
              when is_bar ntoken ->
                Before_next
            | _ -> After_prev )
      | _ -> Before_next )
  | [], [Parser.BAR] -> (
    match (prev.position, next.position) with
    | After (true, Before), Before (true, Before) -> Before_next
    | _ -> After_prev )
  | ([] | [Parser.SEMI]), _ :: _ -> After_prev
  | _ -> Before_next
