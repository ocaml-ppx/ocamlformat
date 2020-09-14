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
open Parsetree
include Non_overlapping_interval_tree.Make (Location)

(** Use Ast_mapper to collect all locs in ast, and create tree of them. *)
let of_ast fragment ast src =
  let locs = ref [] in
  let add_loc loc = locs := loc :: !locs in
  let mapper =
    object
      inherit Ppxlib.Ast_traverse.map as super

      method! location loc = add_loc loc ; loc

      method! pattern p =
        ( match p.ppat_desc with
        | Ppat_record (flds, Open) ->
            Option.iter
              (Source.loc_of_underscore src flds p.ppat_loc)
              ~f:add_loc
        | _ -> () ) ;
        super#pattern p

      method! attribute attr =
        (* ignore location of docstrings *)
        if Ast.Attr.is_doc attr then attr else super#attribute attr

      (** Ast_traverse recurses down to locations in stacks *)
      method! location_stack l = l

      method! expression e =
        ( match e.pexp_desc with
        | Pexp_constant _ ->
            locs := Source.loc_of_expr_constant src e :: !locs
        | _ -> () ) ;
        super#expression e
    end
  in
  Mapper.map_ast fragment mapper ast |> ignore ;
  (of_list !locs, !locs)
