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

include (
  Ast_406 :
    module type of struct
      include Ast_406
    end
    with module Location := Ast_406.Location )

module Parse = struct
  open Migrate_parsetree

  let implementation = Parse.implementation Versions.ocaml_406

  let interface = Parse.interface Versions.ocaml_406

  let use_file lexbuf =
    List.filter (Parse.use_file Versions.ocaml_406 lexbuf)
      ~f:(fun (p : Parsetree.toplevel_phrase) ->
        match p with
        | Ptop_def [] -> false
        | Ptop_def (_ :: _) | Ptop_dir _ -> true )
end

let to_current =
  Migrate_parsetree.Versions.(migrate ocaml_406 ocaml_current)

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

  let copy_directive_argument (x : Parsetree.directive_argument) =
    let open Migrate_parsetree.Versions.OCaml_current.Ast.Parsetree in
    match x with
    | Pdir_none -> Pdir_none
    | Pdir_string s -> Pdir_string s
    | Pdir_int (s, c) -> Pdir_int (s, c)
    | Pdir_ident i -> Pdir_ident i
    | Pdir_bool b -> Pdir_bool b

  let use_file f (x : Parsetree.toplevel_phrase list) =
    List.iter x ~f:(fun (p : Parsetree.toplevel_phrase) ->
        match p with
        | Ptop_def s ->
            top_phrase f (Ptop_def (to_current.copy_structure s))
        | Ptop_dir (d, a) ->
            top_phrase f (Ptop_dir (d, copy_directive_argument a)) )
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
      | Ptop_dir _ as d -> d )

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
  include Ast_406.Location
  module Format = Format_

  let fmt fs {loc_start; loc_end; loc_ghost} =
    Format.fprintf fs "(%a..%a)%s" Position.fmt loc_start Position.fmt
      loc_end
      (if loc_ghost then " ghost" else "")

  let to_string x = Format.asprintf "%a" fmt x

  let sexp_of_t x = Sexp.Atom (to_string x)

  let compare = Poly.compare

  let hash = Hashtbl.hash

  let is_single_line x = x.loc_start.pos_lnum = x.loc_end.pos_lnum

  let compare_start x y = Position.compare x.loc_start y.loc_start

  let compare_start_col x y = Position.compare_col x.loc_start y.loc_start

  let compare_end x y = Position.compare x.loc_end y.loc_end

  let compare_end_col x y = Position.compare_col x.loc_end y.loc_end

  let contains l1 l2 = compare_start l1 l2 <= 0 && compare_end l1 l2 >= 0

  let width x = Position.distance x.loc_start x.loc_end

  let compare_width_decreasing l1 l2 = Int.compare (width l2) (width l1)
end
