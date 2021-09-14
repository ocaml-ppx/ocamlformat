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

val fmt :
  fmt_code:(string -> (Fmt.t, unit) Result.t) -> Odoc_parser.Ast.t -> Fmt.t

val is_tag_only : Odoc_parser.Ast.t -> bool
(** [true] if the documentation only contains tags *)
