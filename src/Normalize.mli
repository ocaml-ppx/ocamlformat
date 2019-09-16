(**********************************************************************
 *                                                                    *
 *                            OCamlFormat                             *
 *                                                                    *
 *  Copyright (c) 2017-present, Facebook, Inc.  All rights reserved.  *
 *                                                                    *
 *  This source code is licensed under the MIT license found in the   *
 *  LICENSE file in the root directory of this source tree.           *
 *                                                                    *
 **********************************************************************)

(** Normalize abstract syntax trees *)

open Migrate_ast
open Parsetree

val dedup_cmts :
  (Ast_mapper.mapper -> 'a -> 'a) -> 'a -> Cmt.t list -> Cmt.t list

val comment : string -> string
(** Normalize a comment. *)

val docstring : Conf.t -> string -> string
(** Normalize a docstring. *)

val impl : Conf.t -> structure -> structure
(** Normalize a structure. *)

val intf : Conf.t -> signature -> signature
(** Normalize a signature. *)

val toplevel : Conf.t -> toplevel_phrase list -> toplevel_phrase list
(** Normalize a toplevel structure. *)

val equal_impl :
  ignore_doc_comments:bool -> Conf.t -> structure -> structure -> bool
(** Compare structures for equality up to normalization. *)

val equal_intf :
  ignore_doc_comments:bool -> Conf.t -> signature -> signature -> bool
(** Compare signatures for equality up to normalization. *)

val equal_toplevel :
     ignore_doc_comments:bool
  -> Conf.t
  -> toplevel_phrase list
  -> toplevel_phrase list
  -> bool
(** Compare toplevel for equality up to normalization. *)

val mapper : Conf.t -> Ast_mapper.mapper
(** Ast_mapper for normalization transformations. *)

type docstring_error =
  | Moved of Location.t * Location.t * string
  | Unstable of Location.t * string

val moved_docstrings_impl :
  Conf.t -> structure -> structure -> docstring_error list

val moved_docstrings_intf :
  Conf.t -> signature -> signature -> docstring_error list

val moved_docstrings_toplevel :
     Conf.t
  -> toplevel_phrase list
  -> toplevel_phrase list
  -> docstring_error list
