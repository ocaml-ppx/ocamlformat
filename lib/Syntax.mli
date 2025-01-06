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

type t =
  | Structure
  | Signature
  | Use_file
  | Core_type
  | Module_type
  | Expression
  | Pattern
  | Repl_file
  | Documentation

val of_fname : string -> t option
(** The expected syntax of a file given its name. *)
