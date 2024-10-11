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

type 'a t = 'a Extended_ast.t

val dedup_cmts : 'a t -> 'a -> Cmt.t list -> Cmt.t list
(** Remove comments that duplicate docstrings (or other comments). *)

val equal : 'a t -> ignore_doc_comments:bool -> Conf.t -> 'a -> 'a -> bool
(** Compare fragments for equality up to normalization. *)

val diff_cmts :
  Conf.t -> Cmt.t list -> Cmt.t list -> (unit, Cmt.error list) Result.t
(** Difference between two lists of comments. *)

val normalize_code : Conf.t -> string -> string
(** Normalize a code block in docstrings. *)
