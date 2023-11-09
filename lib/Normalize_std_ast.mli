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

type 'a t = 'a Std_ast.t

val ast : 'a t -> Conf.t -> 'a -> 'a
(** Normalize an AST fragment. *)

val equal : 'a t -> ignore_doc_comments:bool -> Conf.t -> 'a -> 'a -> bool
(** Compare fragments for equality up to normalization. *)

val moved_docstrings : 'a t -> Conf.t -> 'a -> 'a -> Cmt.error list
