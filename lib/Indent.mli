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
       'a Migrate_ast.Traverse.fragment
    -> unformatted:'a * Source.t
    -> formatted:'a * Source.t
    -> lines:string list
    -> range:int * int
    -> int list
end

module Partial_ast : sig
  val indent_line : ?prev:int * string -> i:int -> line:string -> int -> int

  val indent_range : lines:string list -> range:int * int -> int list
end
