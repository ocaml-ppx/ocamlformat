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

open Fmt
open Menhir_parser.Syntax

type conf = {fmt_code: Fmt.code_formatter}

let fmt ~fmt_code (g : partial_grammar) =
  ignore g ;
  ignore {fmt_code} ;
  str "TODO"
