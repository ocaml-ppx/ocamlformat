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

module Location = Migrate_ast.Location
open Ocamlformat_parser_extended
include Non_overlapping_interval_tree.Make (Location)

(** Use [traverse] to apply a mapper that collects all locations in the AST,
    and create a tree of them. *)
let of_ast ~traverse ast =
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
  (* Ignore locations of arg_labels *)
  let arg_label _ lbl = lbl in
  let mapper =
    Ast_mapper.{default_mapper with location; attribute; arg_label}
  in
  traverse mapper ast |> ignore ;
  (of_list !locs, !locs)
