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

val dedup_cmts : Ast_final.t -> Cmt.t list -> Cmt.t list

val comment : string -> string
(** Normalize a comment. *)

val docstring : Conf.t -> string -> string
(** Normalize a docstring. *)

val normalize : Conf.t -> Ast_final.t -> Ast_final.t
(** Normalize an AST fragment. *)

val equal :
  ignore_doc_comments:bool -> Conf.t -> Ast_final.t -> Ast_final.t -> bool
(** Compare fragments for equality up to normalization. *)

type docstring_error =
  | Moved of Location.t * Location.t * string
  | Unstable of Location.t * string

val moved_docstrings :
  Conf.t -> Ast_final.t -> Ast_final.t -> docstring_error list
