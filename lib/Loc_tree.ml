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

let of_ast fragment ast src =
  let attribute (m : Ast_mapper.mapper) attr =
    (* ignore location of docstrings *)
    if Ast.Attr.is_doc attr then attr
    else Ast_mapper.default_mapper.attribute m attr
  in
  let locs = ref [] in
  let location _ loc =
    locs := loc :: !locs ;
    loc
  in
  let pat m p =
    ( match p.ppat_desc with
    | Ppat_record (flds, Open) ->
        Option.iter (Source.loc_of_underscore src flds p.ppat_loc)
          ~f:(fun loc -> locs := loc :: !locs)
    | Ppat_constant _ -> locs := Source.loc_of_pat_constant src p :: !locs
    | _ -> () ) ;
    Ast_mapper.default_mapper.pat m p
  in
  let expr m e =
    ( match e.pexp_desc with
    | Pexp_constant _ -> locs := Source.loc_of_expr_constant src e :: !locs
    | _ -> () ) ;
    Ast_mapper.default_mapper.expr m e
  in
  let mapper =
    Ast_mapper.{default_mapper with location; pat; attribute; expr}
  in
  Mapper.map_ast fragment mapper ast |> ignore ;
  (of_list !locs, !locs)
