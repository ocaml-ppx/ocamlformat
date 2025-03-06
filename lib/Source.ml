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
open Extended_ast

(** Concrete syntax. *)
type t = {text: string; tokens: (Parser.token * Location.t) array}

let is_dummy_pos p = p.Lexing.pos_cnum < 0

let create ~text ~tokens =
  let tokens =
    List.filter tokens ~f:(fun (tok, _) ->
        match tok with Parser.EOL | Parser.EOF -> false | _ -> true )
  in
  {text; tokens= Array.of_list tokens}

let string_at t (l : Location.t) =
  let pos = l.loc_start.Lexing.pos_cnum
  and len = Position.distance l.loc_start l.loc_end in
  String.sub t.text ~pos ~len

let find_token t k pos =
  Array.binary_search t.tokens
    ~compare:(fun (_, elt) pos -> Position.compare elt.Location.loc_start pos)
    k pos

let find_first_token_on_line t line =
  match
    Array.binary_search t.tokens
      ~compare:(fun (_, elt) -> Int.compare elt.Location.loc_start.pos_lnum)
      `First_equal_to line
  with
  | None -> None
  | Some i when i >= Array.length t.tokens -> None
  | Some i -> Some t.tokens.(i)

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
  (not (is_dummy_pos p1 || is_dummy_pos p2)) && loop p1 l

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

let extend_loc_to_include_attributes (loc : Location.t) (l : attributes) =
  let loc_end =
    List.fold l ~init:loc ~f:(fun acc ({attr_loc; _} : attribute) ->
        if Location.compare_end attr_loc acc <= 0 then acc else attr_loc )
  in
  if phys_equal loc_end loc then loc
  else
    {loc with loc_end= {loc.loc_end with pos_cnum= loc_end.loc_end.pos_cnum}}

let string_literal t mode loc =
  Option.value_exn ~message:"Parse error while reading string literal"
    (Literal_lexer.string mode (string_at t loc))

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

let empty_line_before t (loc : Location.t) =
  match find_token_before t ~filter:(fun _ -> true) loc.loc_start with
  | Some (_, before) -> Location.line_difference before loc > 1
  | None -> false

let empty_line_after t (loc : Location.t) =
  match find_token_after t ~filter:(fun _ -> true) loc.loc_end with
  | Some (_, after) -> Location.line_difference loc after > 1
  | None -> false

let extension_using_sugar ~(name : string Location.loc)
    ~(payload : Location.t) =
  name.loc.loc_ghost
  || Source_code_position.ascending name.loc.loc_start payload.loc_start > 0

let type_constraint_is_first typ loc =
  Location.compare_start typ.ptyp_loc loc < 0

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
