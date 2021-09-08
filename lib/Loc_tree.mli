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

val of_ast : 'a Extended_ast.t -> 'a -> t * Location.t list
(** Use Ast_mapper to collect all locs in ast, and create a tree of them. *)
