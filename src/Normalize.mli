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

val impl : structure -> structure
(** Normalize a structure. *)

val intf : signature -> signature
(** Normalize a signature. *)

val use_file : toplevel_phrase list -> toplevel_phrase list
(** Normalize a use_file. *)

val equal_impl : ignore_doc_comments:bool -> structure -> structure -> bool
(** Compare structures for equality up to normalization. *)

val equal_intf : ignore_doc_comments:bool -> signature -> signature -> bool
(** Compare signatures for equality up to normalization. *)

val equal_use_file :
     ignore_doc_comments:bool
  -> toplevel_phrase list
  -> toplevel_phrase list
  -> bool
(** Compare use_file for equality up to normalization. *)

val mapper : Ast_mapper.mapper
(** Ast_mapper for normalization transformations. *)
