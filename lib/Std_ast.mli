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

(** Interface over the AST defined in vendor/ocaml-4.13 *)

open Ocamlformat_parser_standard

include module type of Parsetree

type use_file = toplevel_phrase list

type 'a t =
  | Structure : structure t
  | Signature : signature t
  | Use_file : use_file t
  | Core_type : core_type t
  | Module_type : module_type t
  | Expression : expression t
  | Pattern : pattern t
  (* not implemented *)
  | Repl_file : unit t
  | Documentation : unit t

type any_t = Any : 'a t -> any_t [@@unboxed]

val of_syntax : Syntax.t -> any_t

module Parse : sig
  val ast :
       'a t
    -> ocaml_version:Ocaml_version.t
    -> input_name:string
    -> string
    -> 'a
end

val equal : 'a t -> 'a -> 'a -> bool

val map : 'a t -> Ast_mapper.mapper -> 'a -> 'a

module Printast : sig
  include module type of Printast

  val ast : 'a t -> Format.formatter -> 'a -> unit
end
