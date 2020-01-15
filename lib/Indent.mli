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

val indent_from_locs :
     'a Migrate_ast.Traverse.fragment
  -> unformatted:'a * Source.t
  -> formatted:'a * Source.t
  -> lines:string list
  -> range:int * int
  -> (int list, [`Msg of string]) Result.t

val indent_from_lines :
     lines:string list
  -> range:int * int
  -> (int list, [`Msg of string]) Result.t
