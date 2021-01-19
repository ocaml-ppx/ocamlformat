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

module Ast0 : sig
  include module type of Ppxlib.Parsetree

  type use_file = toplevel_phrase list

  type t =
    | Past_str of structure
    | Past_sig of signature
    | Past_usf of use_file
end

module Ast_final : sig
  include module type of Ppxlib.Parsetree

  val equal_core_type : core_type -> core_type -> bool

  type use_file = toplevel_phrase list

  type t =
    | Past_str of structure
    | Past_sig of signature
    | Past_usf of use_file

  val equal : t -> t -> bool

  class map :
    object
      inherit Ppxlib.Ast_traverse.map

      method use_file : use_file -> use_file

      method ast : t -> t
    end

  val map : map -> t -> t

  val iter : Ppxlib.Ast_traverse.iter -> t -> unit

  val fold : 'r Ppxlib.Ast_traverse.fold -> t -> 'r -> 'r
end

val run : Ast0.t -> Ast_final.t
