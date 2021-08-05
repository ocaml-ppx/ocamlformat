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
  -> Source.t
  -> Cmts.t
  -> Conf.t
  -> 'a
  -> Fmt.t
(** Format a fragment. *)
