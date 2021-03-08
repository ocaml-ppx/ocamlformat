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

  type 'a t =
    | Structure : structure t
    | Signature : signature t
    | Use_file : use_file t

  module Parse : sig
    val ast : 'a t -> Lexing.lexbuf -> 'a

    val parser_version : Ocaml_version.t
  end
end

module Ast_final : sig
  include module type of Ppxlib.Parsetree

  val equal_core_type : core_type -> core_type -> bool

  type use_file = toplevel_phrase list

  type 'a t =
    | Structure : structure t
    | Signature : signature t
    | Use_file : use_file t

  val equal : 'a t -> 'a -> 'a -> bool

  class map :
    object
      inherit Ppxlib.Ast_traverse.map
    end

  val map : 'a t -> Ppxlib.Ast_traverse.map -> 'a -> 'a

  val iter : 'a t -> Ppxlib.Ast_traverse.iter -> 'a -> unit

  val fold : 'a t -> 'r Ppxlib.Ast_traverse.fold -> 'a -> 'r -> 'r

  module Printast : sig
    val implementation : Format.formatter -> structure -> unit

    val interface : Format.formatter -> signature -> unit

    val payload : Format.formatter -> payload -> unit

    val expression : Format.formatter -> expression -> unit

    val use_file : Format.formatter -> toplevel_phrase list -> unit

    val ast : 'a t -> Format.formatter -> 'a -> unit
  end

  module Pprintast = Ppxlib.Pprintast
end

val run : 'a Ast0.t -> 'b Ast_final.t -> 'a -> 'b
