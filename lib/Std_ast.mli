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

(** Standard OCaml parser AST (vendor/parser-standard).

    OCamlformat uses an extended parser for formatting that supports
    additional syntax constructs (field punning, begin..end nodes, labeled
    tuples, etc.). To verify that formatting preserves semantics,
    ocamlformat re-parses both the original and formatted source with the
    standard OCaml parser and compares the resulting ASTs. If they differ,
    formatting changed the meaning of the program — which is a bug.

    The standard parser is used for this comparison because it represents
    what the OCaml compiler sees: two programs are equivalent if the
    compiler parses them identically. *)

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
