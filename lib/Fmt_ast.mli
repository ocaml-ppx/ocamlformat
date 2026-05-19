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

(** Format OCaml Ast *)

val fmt_ast :
     'a Extended_ast.t
  -> debug:bool
  -> Conf.t
  -> Fmt.t * Cmts.t option
(** Format a parsed fragment. Internally takes a fresh copy of the embedded
    [Cmts.t] (so the embedded state is preserved). Returns the formatter
    and the copied [Cmts.t] (consumed once the formatter is evaluated),
    or [None] for [Documentation]. *)
