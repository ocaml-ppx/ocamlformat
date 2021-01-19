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
     debug:bool
  -> Source.t
  -> Cmts.t
  -> Conf.t
  -> Ast_passes.Ast_final.t
  -> Fmt.t
(** Format a fragment. *)
