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

val dedup_cmts : 'a Extended_ast.t -> 'a -> Cmt.t list -> Cmt.t list
(** Remove comments that duplicate docstrings (or other comments). *)
