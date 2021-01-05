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

module Split : sig
  val fragment : 'a Traverse.fragment -> string -> string list
end

module Recover : sig
  val fragment : 'a Traverse.fragment -> string -> string
end

module Parse : sig
  val fragment : 'a Traverse.fragment -> string -> ('a, string) Result.t list
end
