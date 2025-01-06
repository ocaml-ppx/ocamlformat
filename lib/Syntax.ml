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

let of_fname fname =
  match Filename.extension fname with
  | ".ml" | ".mlt" | ".eliom" -> Some Use_file
  | ".mli" | ".eliomi" -> Some Signature
  | ".mld" -> Some Documentation
  | _ -> None
