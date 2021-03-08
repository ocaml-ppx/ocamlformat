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

(** Normalize abstract syntax trees *)

open Ast_passes

val dedup_cmts : 'a Ast_final.t -> 'a -> Cmt.t list -> Cmt.t list

val comment : string -> string
(** Normalize a comment. *)

val docstring : Conf.t -> string -> string
(** Normalize a docstring. *)

val normalize : 'a Ast_final.t -> Conf.t -> 'a -> 'a
(** Normalize an AST fragment. *)

val equal :
  'a Ast_final.t -> ignore_doc_comments:bool -> Conf.t -> 'a -> 'a -> bool
(** Compare fragments for equality up to normalization. *)

type docstring_error =
  | Moved of Location.t * Location.t * string
  | Unstable of Location.t * string

val moved_docstrings :
  'a Ast_final.t -> Conf.t -> 'a -> 'a -> docstring_error list
