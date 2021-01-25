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

module Valid_ast : sig
  val indent :
       'a Migrate_ast.Traverse.fragment
    -> unformatted:'a * Source.t
    -> formatted:'a * Source.t
    -> lines:string list
    -> range:int * int
    -> (int list, [`Msg of string]) Result.t
end

module Partial_ast : sig
  val indent :
       lines:string list
    -> range:int * int
    -> (int list, [`Msg of string]) Result.t
end
