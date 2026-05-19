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

val diff_cmts :
  Conf.t -> 'a t -> 'a t -> (unit, Cmt.error list) Result.t
(** Difference between the comments of two parsed values. *)

val normalize_code : Conf.t -> string -> string
(** Normalize a code block in docstrings. *)
