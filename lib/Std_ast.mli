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

open Ocaml_413

include module type of Parsetree

type use_file = toplevel_phrase list

type 'a t =
  | Structure : structure t
  | Signature : signature t
  | Use_file : use_file t
  | Core_type : core_type t
  | Module_type : module_type t
  | Expression : expression t

module Parse : sig
  val ast : 'a t -> Lexing.lexbuf -> 'a
end

val equal : 'a t -> 'a -> 'a -> bool

val map : 'a t -> Ast_mapper.mapper -> 'a -> 'a

module Pprintast : sig
  include module type of Pprintast

  val ast : 'a t -> Format.formatter -> 'a -> unit
end

(** Normalize abstract syntax trees *)
module Normalize : sig
  val is_doc : attribute -> bool

  val dedup_cmts : 'a t -> 'a -> Cmt.t list -> Cmt.t list
  (** Remove comments that duplicate docstrings (or other comments). *)

  val ast : 'a t -> Conf.t -> 'a -> 'a
  (** Normalize an AST fragment. *)

  val equal : 'a t -> ignore_doc_comments:bool -> Conf.t -> 'a -> 'a -> bool
  (** Compare fragments for equality up to normalization. *)

  val moved_docstrings : 'a t -> Conf.t -> 'a -> 'a -> Docstring.error list

  val diff_docstrings :
       Conf.t
    -> Cmt.t list
    -> Cmt.t list
    -> (string, string) Either.t Sequence.t
  (** Difference between two lists of doc comments. *)

  val diff_cmts :
       Conf.t
    -> Cmt.t list
    -> Cmt.t list
    -> (string, string) Either.t Sequence.t
  (** Difference between two lists of comments. *)
end
