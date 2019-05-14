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

let selected_version = Migrate_parsetree.Versions.ocaml_408

module Selected_version = Ast_408
module Parsetree = Selected_version.Parsetree
module Ast_mapper = Selected_version.Ast_mapper
module Ast_helper = Selected_version.Ast_helper
module Asttypes = Selected_version.Asttypes

let map_structure = Selected_version.map_structure

let map_signature = Selected_version.map_signature

module Parse = struct
  open Migrate_parsetree

  let implementation = Parse.implementation selected_version

  let interface = Parse.interface selected_version

  let use_file lexbuf =
    List.filter (Parse.use_file selected_version lexbuf)
      ~f:(fun (p : Parsetree.toplevel_phrase) ->
        match p with
        | Ptop_def [] -> false
        | Ptop_def (_ :: _) | Ptop_dir _ -> true)
end

let to_current =
  Migrate_parsetree.Versions.(migrate selected_version ocaml_current)

module Printast = struct
  open Printast

  let implementation f x = implementation f (to_current.copy_structure x)

  let interface f x = interface f (to_current.copy_signature x)

  let expression n f x = expression n f (to_current.copy_expression x)

  let payload n f (x : Parsetree.payload) =
    payload n f
      ( match x with
      | PStr x -> PStr (to_current.copy_structure x)
      | PSig x -> PSig (to_current.copy_signature x)
      | PTyp x -> PTyp (to_current.copy_core_type x)
      | PPat (x, y) ->
          PPat
            ( to_current.copy_pattern x
            , Option.map ~f:to_current.copy_expression y ) )

  let use_file f (x : Parsetree.toplevel_phrase list) =
    List.iter x ~f:(fun (p : Parsetree.toplevel_phrase) ->
        top_phrase f (to_current.copy_toplevel_phrase p))
end

module Pprintast = struct
  open Pprintast

  let structure f x = structure f (to_current.copy_structure x)

  let signature f x = signature f (to_current.copy_signature x)

  let core_type f x = core_type f (to_current.copy_core_type x)

  let expression f x = expression f (to_current.copy_expression x)

  let pattern f x = pattern f (to_current.copy_pattern x)
end

(* Missing from ocaml_migrate_parsetree *)
let map_use_file mapper use_file =
  let open Parsetree in
  List.map use_file ~f:(fun toplevel_phrase ->
      match (toplevel_phrase : toplevel_phrase) with
      | Ptop_def structure ->
          Ptop_def (mapper.Ast_mapper.structure mapper structure)
      | Ptop_dir _ as d -> d)

module Position = struct
  open Lexing
  module Format = Format_

  let column {pos_bol; pos_cnum} = pos_cnum - pos_bol

  let fmt fs {pos_lnum; pos_bol; pos_cnum} =
    if pos_lnum = -1 then Format.fprintf fs "[%d]" pos_cnum
    else Format.fprintf fs "[%d,%d+%d]" pos_lnum pos_bol (pos_cnum - pos_bol)

  let compare_col p1 p2 = Int.compare (column p1) (column p2)

  let compare p1 p2 =
    if phys_equal p1 p2 then 0 else Int.compare p1.pos_cnum p2.pos_cnum

  let distance p1 p2 = p2.pos_cnum - p1.pos_cnum
end

module Location = struct
  include Selected_version.Location
  module Format = Format_

  let fmt fs {loc_start; loc_end; loc_ghost} =
    Format.fprintf fs "(%a..%a)%s" Position.fmt loc_start Position.fmt
      loc_end
      (if loc_ghost then " ghost" else "")

  let to_string x = Format.asprintf "%a" fmt x

  let sexp_of_t x = Sexp.Atom (to_string x)

  let compare : t -> t -> int = Poly.compare

  let hash = Hashtbl.hash

  let compare_start x y = Position.compare x.loc_start y.loc_start

  let compare_start_col x y = Position.compare_col x.loc_start y.loc_start

  let compare_end x y = Position.compare x.loc_end y.loc_end

  let compare_end_col x y = Position.compare_col x.loc_end y.loc_end

  let contains l1 l2 = compare_start l1 l2 <= 0 && compare_end l1 l2 >= 0

  let width x = Position.distance x.loc_start x.loc_end

  let compare_width_decreasing l1 l2 =
    match Position.compare l1.loc_start l2.loc_start with
    | 0 -> (
      match Position.compare l2.loc_end l1.loc_end with
      | 0 -> compare l1 l2
      | n -> n )
    | n -> n

  let is_single_line x margin =
    width x <= margin && x.loc_start.pos_lnum = x.loc_end.pos_lnum
end
