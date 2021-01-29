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

type token = Cmt of string Location.loc | S of string Location.loc

val lex_comments : string -> token list
(** [lex_comments x] splits [x] into a sequence of comments and non-comments
    strings. *)
