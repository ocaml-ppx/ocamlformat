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

open Migrate_ast

module Cmt_lexer : sig
  type token = Cmt of string | Other of string

  val lex_comments : string -> token list
  (** [lex_comments x] splits [x] into a sequence of comments and
      non-comments strings.

      Exposed for tests. *)
end

module Split : sig
  val split_on_linebreaks : Cmt_lexer.token list -> string list
  (** Exposed for tests. *)

  val fragment : 'a Traverse.fragment -> string -> string list
  (** [fragment fg x] splits [x] of a fragment kind [fg] into a list of
      fragments.

      Exposed for tests. *)
end

module Recover : sig
  val fragment : 'a Traverse.fragment -> string -> string
end
