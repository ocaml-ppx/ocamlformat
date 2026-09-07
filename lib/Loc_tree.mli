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

include Non_overlapping_interval_tree.S with type itv = Location.t

val of_ast :
     traverse:(Ocamlformat_parser_extended.Ast_mapper.mapper -> 'a -> 'a)
  -> 'a
  -> t * Location.t list
(** Use [traverse] to apply a mapper that collects all locations in the AST,
    and create a tree of them. *)
