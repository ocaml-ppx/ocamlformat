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

open Extended_ast

val rewrite_type_declaration_imm_attr_to_jkind_annot :
  type_declaration -> attribute option * type_declaration
(** Rewrites [@@immediate] to [_ : immediate] and does the same for [@@immediate64].
    This only happens if there's no existing jkind annotation AND there's only
    one immediacy attribute. If the rewrite occurred, also returns the attribute that
    participated. *)

val dedup_cmts : 'a t -> 'a -> Cmt.t list -> Cmt.t list
(** Remove comments that duplicate docstrings (or other comments). *)

val equal : 'a t -> ignore_doc_comments:bool -> Conf.t -> 'a -> 'a -> bool
(** Compare fragments for equality up to normalization. *)

val diff_cmts :
  Conf.t -> Cmt.t list -> Cmt.t list -> (unit, Cmt.error list) Result.t
(** Difference between two lists of comments. *)
