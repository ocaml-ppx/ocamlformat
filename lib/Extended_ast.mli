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

open Ocamlformat_parser_extended

include module type of Parsetree

type use_file = toplevel_phrase list

type repl_file = repl_phrase list

module Std_parsetree = Ocamlformat_parser_standard.Parsetree

(** A parsed fragment. Each constructor identifies the AST kind and carries
    the metadata produced by parsing. Paired kinds carry both the extended
    AST [ast] and its standard-library counterpart [std] (used for
    equivalence checking). Only [Use_file] carries a shebang [prefix].
    [Documentation] has no comment-placement state at all; the other kinds
    carry a [Cmts.t] holding the placed comments and the [Source.t]. *)
type 'a t =
  | Structure :
      {ast: structure; std: Std_parsetree.structure; cmts: Cmts.t}
      -> structure t
  | Signature :
      {ast: signature; std: Std_parsetree.signature; cmts: Cmts.t}
      -> signature t
  | Use_file :
      { ast: use_file
      ; std: Std_parsetree.toplevel_phrase list
      ; prefix: string
      ; cmts: Cmts.t }
      -> use_file t
  | Core_type :
      {ast: core_type; std: Std_parsetree.core_type; cmts: Cmts.t}
      -> core_type t
  | Module_type :
      {ast: module_type; std: Std_parsetree.module_type; cmts: Cmts.t}
      -> module_type t
  | Expression :
      {ast: expression; std: Std_parsetree.expression; cmts: Cmts.t}
      -> expression t
  | Pattern :
      {ast: pattern; std: Std_parsetree.pattern; cmts: Cmts.t}
      -> pattern t
  | Repl_file : {ast: repl_file; cmts: Cmts.t} -> repl_file t
  | Documentation :
      Ocamlformat_odoc_parser.Ast.t
      -> Ocamlformat_odoc_parser.Ast.t t

type any_t = Any : 'a t -> any_t [@@unboxed]

val ast : 'a t -> 'a

val cmts : 'a t -> Cmts.t option
(** [None] for [Documentation]. *)

val copy_cmts : 'a t -> 'a t
(** Return [t] with a deep copy of its embedded [Cmts.t] (no-op for
    [Documentation]). Used to format without consuming the original. *)

val traverse : 'a t -> Ast_mapper.mapper -> 'a -> 'a
(** Apply a mapper to the AST inside [t]. [Documentation] is left
    unchanged. *)

val map : Ast_mapper.mapper -> 'a t -> 'a t

module Printast : sig
  include module type of Printast

  val ast : Format.formatter -> 'a t -> unit
end

module Asttypes : sig
  include module type of Asttypes

  val is_override : override_flag -> bool

  val is_recursive : rec_flag -> bool
end

exception Warning50 of (Location.t * Warnings.t) list

val parse :
     ?disable_w50:bool
  -> ?disable_deprecated:bool
  -> Syntax.t
  -> Conf.t
  -> input_name:string
  -> source:string
  -> any_t
(** Parse source with warning handling, hash-bang detection, and comment
    placement. For paired fragment kinds, also parses with the standard
    parser. *)

val reparse :
     ?disable_w50:bool
  -> ?disable_deprecated:bool
  -> 'a t
  -> Conf.t
  -> input_name:string
  -> source:string
  -> 'a t
(** Re-parse [source] using the same fragment kind as the supplied parsed
    value. Preserves the type so the result fits in the same context. *)

val parse_toplevel :
     ?disable_w50:bool
  -> ?disable_deprecated:bool
  -> Conf.t
  -> input_name:string
  -> source:string
  -> (use_file t, repl_file t) Either.t
(** Parse source as toplevel phrases or REPL phrases depending on content. *)

type std_value = Std_value : 'a Std_ast.t * 'a -> std_value

val get_std : 'a t -> std_value option
(** Extract the std AST with its [Std_ast.t] witness, or [None] for
    [Repl_file] and [Documentation]. *)

val dump : Format.formatter -> 'a t -> unit
(** Print the std AST for debug output. Falls back to extended AST for
    [Repl_file] and [Documentation]. *)

val dump_normalized :
     normalize_code:(string -> string)
  -> Conf.t
  -> Format.formatter
  -> 'a t
  -> unit
(** Print the normalized std AST for debug output. *)

type ast_check_result =
  | Ast_preserved
  | Docstrings_moved of Cmt.error list
  | Ast_changed

val equivalent :
     normalize_code:(string -> string)
  -> Conf.t
  -> 'a t
  -> 'a t
  -> ast_check_result
(** Check whether formatting preserved the standard AST. For [Repl_file]
    and [Documentation], always returns [Ast_preserved]. *)
