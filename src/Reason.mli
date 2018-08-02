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

(** Support for reading Reason code *)

open Migrate_ast
open Parsetree
open Translation_unit

val input_impl : Conf.t -> In_channel.t -> structure with_comments
(** Reads a serialized structure from an input channel. It is assumed to be
    the output of `refmt --print=binary_reason` where `refmt` has been
    compiled with the same version of `ocaml` as `ocamlformat`. *)

val input_intf : Conf.t -> In_channel.t -> signature with_comments
(** Reads a serialized signature from an input channel. It is assumed to be
    the output of `refmt --print=binary_reason` where `refmt` has been
    compiled with the same version of `ocaml` as `ocamlformat`. *)

val norm_impl : structure with_comments -> structure
(** Normalize a structure. *)

val norm_intf : signature with_comments -> signature
(** Normalize a signature. *)

val equal_impl :
     ignore_doc_comments:bool
  -> structure with_comments
  -> structure with_comments
  -> bool
(** Compare structures for equality up to normalization. *)

val equal_intf :
     ignore_doc_comments:bool
  -> signature with_comments
  -> signature with_comments
  -> bool
(** Compare signatures for equality up to normalization. *)
