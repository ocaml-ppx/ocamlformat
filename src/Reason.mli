(**********************************************************************)
(*                                                                    *)
(*                            OCamlFormat                             *)
(*                                                                    *)
(*  Copyright (c) 2017-present, Facebook, Inc.  All rights reserved.  *)
(*                                                                    *)
(*  This source code is licensed under the MIT license found in the   *)
(*  LICENSE file in the root directory of this source tree.           *)
(*                                                                    *)
(**********************************************************************)

(** Support for reading Reason code *)

open Migrate_ast
open Parsetree

val input_impl :
  string -> In_channel.t -> structure * (string * Location.t) list
(** Reads a serialized structure from an input channel. It is assumed to be
    the output of `refmt --print=binary_reason` where `refmt` has been
    compiled with the same version of `ocaml` as `ocamlformat`. *)

val input_intf :
  string -> In_channel.t -> signature * (string * Location.t) list
(** Reads a serialized signature from an input channel. It is assumed to be
    the output of `refmt --print=binary_reason` where `refmt` has been
    compiled with the same version of `ocaml` as `ocamlformat`. *)

val norm_impl : structure * (string * Location.t) list -> structure
(** Normalize a structure. *)

val norm_intf : signature * (string * Location.t) list -> signature
(** Normalize a signature. *)

val equal_impl :
  structure * (string * Location.t) list
  -> structure * (string * Location.t) list -> bool
(** Compare structures for equality up to normalization. *)

val equal_intf :
  signature * (string * Location.t) list
  -> signature * (string * Location.t) list -> bool
(** Compare signatures for equality up to normalization. *)
