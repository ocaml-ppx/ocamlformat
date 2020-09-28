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

let cons_option xo xs = match xo with Some x -> x :: xs | None -> xs

let fold src =
  object
    inherit [Location.t list] Ppxlib.Ast_traverse.fold as super

    method! location loc locs = loc :: locs

    method! pattern p locs =
      let extra_loc =
        match p.ppat_desc with
        | Ppat_record (flds, Open) ->
            Source.loc_of_underscore src flds p.ppat_loc
        | _ -> None
      in
      cons_option extra_loc locs |> super#pattern p

    method! attribute attr locs =
      (* ignore location of docstrings *)
      if Ast.Attr.is_doc attr then locs else super#attribute attr locs

    (** Ast_traverse recurses down to locations in stacks *)
    method! location_stack _ l = l

    method! expression e locs =
      let extra_loc =
        match e.pexp_desc with
        | Pexp_constant _ -> Some (Source.loc_of_expr_constant src e)
        | _ -> None
      in
      cons_option extra_loc locs |> super#expression e
  end

(** Use Ast_mapper to collect all locs in ast, and create tree of them. *)
let of_ast fragment ast src =
  let locs = Mapper.fold_ast fragment (fold src) ast [] in
  (of_list locs, locs)
