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

type fmt_code = Conf.t -> string -> Fmt.t or_error

val fmt_ast : Conf.t -> fmt_code:fmt_code -> Odoc_parser.Ast.t -> Fmt.t

val fmt_parsed :
     Conf.t
  -> fmt_code:fmt_code
  -> input:string
  -> (Odoc_parser.Ast.t, Odoc_parser.Warning.t list) Result.t
  -> Fmt.t
