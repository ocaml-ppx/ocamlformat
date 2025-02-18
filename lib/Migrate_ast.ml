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

module Lexing = struct
  include Lexing

  let set_position lexbuf position =
    lexbuf.lex_curr_p <-
      {position with pos_fname= lexbuf.lex_curr_p.pos_fname} ;
    lexbuf.lex_abs_pos <- position.pos_cnum

  let set_filename lexbuf fname =
    lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname= fname}
end

module Position = struct
  open Lexing

  type t = position

  let column {pos_bol; pos_cnum; _} = pos_cnum - pos_bol

  let fmt fs {pos_lnum; pos_bol; pos_cnum; pos_fname= _} =
    if pos_lnum = -1 then Format.fprintf fs "[%d]" pos_cnum
    else Format.fprintf fs "[%d,%d+%d]" pos_lnum pos_bol (pos_cnum - pos_bol)

  let to_string x = Format.asprintf "%a" fmt x

  let sexp_of_t x = Sexp.Atom (to_string x)

  let compare_col p1 p2 = Int.compare (column p1) (column p2)

  let compare p1 p2 =
    if phys_equal p1 p2 then 0 else Int.compare p1.pos_cnum p2.pos_cnum

  include (val Comparator.make ~compare ~sexp_of_t)

  let distance p1 p2 = p2.pos_cnum - p1.pos_cnum
end

module Location = struct
  include Location

  let fmt fs {loc_start; loc_end; loc_ghost} =
    Format.fprintf fs "(%a..%a)%s" Position.fmt loc_start Position.fmt
      loc_end
      (if loc_ghost then " ghost" else "")

  let to_string x = Format.asprintf "%a" fmt x

  let sexp_of_t x = Sexp.Atom (to_string x)

  let compare {loc_start; loc_end; loc_ghost} b =
    match Position.compare loc_start b.loc_start with
    | 0 -> (
      match Position.compare loc_end b.loc_end with
      | 0 -> Bool.compare loc_ghost b.loc_ghost
      | c -> c )
    | c -> c

  type location = t

  module Location_comparator = Comparator.Make (struct
    type t = location

    let sexp_of_t = sexp_of_t

    let compare = compare
  end)

  include Location_comparator

  let compare_start x y = Position.compare x.loc_start y.loc_start

  let compare_start_col x y = Position.compare_col x.loc_start y.loc_start

  let compare_end x y = Position.compare x.loc_end y.loc_end

  let compare_end_col x y = Position.compare_col x.loc_end y.loc_end

  let line_difference fst snd = snd.loc_start.pos_lnum - fst.loc_end.pos_lnum

  let contains l1 l2 = compare_start l1 l2 <= 0 && compare_end l1 l2 >= 0

  let width x = Position.distance x.loc_start x.loc_end

  let descending cmp a b = -cmp a b

  let compare_width_decreasing =
    Comparable.lexicographic [compare_start; descending compare_end; compare]

  let is_single_line x margin =
    (* The last character of a line can exceed the margin if it is not
       preceded by a break. Adding 1 here is a workaround for this bug. *)
    width x <= margin + 1 && x.loc_start.pos_lnum = x.loc_end.pos_lnum

  let smallest loc stack =
    let min a b = if width a < width b then a else b in
    List.reduce_exn (loc :: stack) ~f:min

  let of_lexbuf (lexbuf : Lexing.lexbuf) =
    { loc_start= lexbuf.lex_start_p
    ; loc_end= lexbuf.lex_curr_p
    ; loc_ghost= false }

  let of_lines ~filename:pos_fname lines =
    List.folding_mapi ~init:0 lines ~f:(fun i c s ->
        let loc =
          let pos_lnum = i + 1 in
          let loc_start : Lexing.position =
            {pos_fname; pos_lnum; pos_bol= c; pos_cnum= c}
          in
          let loc_end : Lexing.position =
            { pos_fname
            ; pos_lnum
            ; pos_bol= c
            ; pos_cnum= c + String.length (String.strip s) }
          in
          {loc_start; loc_end; loc_ghost= false}
        in
        (c + String.length (String.strip s) + 1, mkloc (String.strip s) loc) )

  let is_ghost loc = loc.loc_ghost
end

module Longident = struct
  include Longident

  let lident s =
    assert (not (String.contains s '.')) ;
    Lident s
end
