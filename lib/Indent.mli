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
  val indent_range :
       'a Extended_ast.t
    -> unformatted:'a * string
    -> formatted:'a * Source.t
    -> lines:string list
    -> range:Range.t
    -> int list
end

module Partial_ast : sig
  val indent_range : source:string -> range:Range.t -> int list
end
