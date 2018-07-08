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

include Ast_407
module Current = Migrate_parsetree.OCaml_current.Ast

module Parse = struct
  open Migrate_parsetree

  let implementation = Parse.implementation Versions.ocaml_407

  let interface = Parse.interface Versions.ocaml_407

  let use_file = Parse.use_file Versions.ocaml_407
end

let to_current = Migrate_parsetree.Versions.(migrate ocaml_407 ocaml_current)

module Printast = struct
  open Printast

  let implementation f x = implementation f (to_current.copy_structure x)

  let interface f x = interface f (to_current.copy_signature x)

  let expression n f x = expression n f (to_current.copy_expression x)

  let payload n f (x: Parsetree.payload) =
    payload n f
      ( match x with
      | PStr x -> PStr (to_current.copy_structure x)
      | PSig x -> PSig (to_current.copy_signature x)
      | PTyp x -> PTyp (to_current.copy_core_type x)
      | PPat (x, y) ->
          PPat
            ( to_current.copy_pattern x
            , Option.map ~f:to_current.copy_expression y ) )

  let use_file f (x: Parsetree.toplevel_phrase list) =
    List.iter x ~f:(fun x ->
        top_phrase f
          ( match (x : Parsetree.toplevel_phrase) with
          | Parsetree.Ptop_def x -> Ptop_def (to_current.copy_structure x)
          | Parsetree.Ptop_dir (a, b) ->
              let b =
                match b with
                | Pdir_none -> Current.Parsetree.Pdir_none
                | Pdir_string s -> Current.Parsetree.Pdir_string s
                | Pdir_int (s, c) -> Current.Parsetree.Pdir_int (s, c)
                | Pdir_ident i -> Current.Parsetree.Pdir_ident i
                | Pdir_bool b -> Current.Parsetree.Pdir_bool b
              in
              Ptop_dir (a, b) ) )
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
