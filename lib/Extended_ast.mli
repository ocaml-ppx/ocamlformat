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

(** Pairs an extended AST (['a]) with its standard-library counterpart
    (['b]). Parsing produces both ASTs so that the extended one can be used
    for formatting while the standard one is used for equivalence checking. *)
type ('a, 'b) paired = {extended: 'a; std: 'b}

(** Fragment types. OCaml fragments carry both extended and standard ASTs.
    [Repl_file] has no standard parser counterpart. [Documentation] uses
    the odoc parser directly and does not need a paired representation. *)
type 'a t =
  | Structure : (structure, Std_parsetree.structure) paired t
  | Signature : (signature, Std_parsetree.signature) paired t
  | Use_file : (use_file, Std_parsetree.toplevel_phrase list) paired t
  | Core_type : (core_type, Std_parsetree.core_type) paired t
  | Module_type : (module_type, Std_parsetree.module_type) paired t
  | Expression : (expression, Std_parsetree.expression) paired t
  | Pattern : (pattern, Std_parsetree.pattern) paired t
  | Repl_file : repl_file t
  | Documentation : Ocamlformat_odoc_parser.Ast.t t
  | Mll_file : Ocamlformat_mll_parser.Mll_ast.lexer_def t

type any_t = Any : 'a t -> any_t [@@unboxed]

val of_syntax : Syntax.t -> any_t

val map : 'a t -> Ast_mapper.mapper -> 'a -> 'a

module Printast : sig
  include module type of Printast

  val ast : 'a t -> Format.formatter -> 'a -> unit
end

module Asttypes : sig
  include module type of Asttypes

  val is_override : override_flag -> bool

  val is_recursive : rec_flag -> bool
end

module Parsed : sig
  type 'a t =
    {ast: 'a; comments: Cmt.t list; prefix: string; source: Source.t}
end

exception Warning50 of (Location.t * Warnings.t) list

val parse :
     ?disable_w50:bool
  -> ?disable_deprecated:bool
  -> 'a t
  -> Conf.t
  -> input_name:string
  -> source:string
  -> 'a Parsed.t
(** Parse source with warning handling, hash-bang detection, and token
    collection. For paired fragment types, also parses with the standard
    parser. *)

val parse_toplevel :
     ?disable_w50:bool
  -> ?disable_deprecated:bool
  -> Conf.t
  -> input_name:string
  -> source:string
  -> ( (use_file, Std_parsetree.toplevel_phrase list) paired Parsed.t
     , repl_file Parsed.t )
     Either.t
(** Parse source as toplevel phrases or REPL phrases depending on content. *)

type std_value = Std_value : 'a Std_ast.t * 'a -> std_value

val get_std : 'a t -> 'a -> std_value option
(** Extract the std AST with its [Std_ast.t] witness, or [None] for
    [Repl_file] and [Documentation]. *)

val dump : 'a t -> Format.formatter -> 'a -> unit
(** Print the std AST for debug output. Falls back to extended AST for
    [Repl_file] and [Documentation]. *)

val dump_normalized :
     'a t
  -> normalize_code:(string -> string)
  -> Conf.t
  -> Format.formatter
  -> 'a
  -> unit
(** Print the normalized std AST for debug output. *)

type ast_check_result =
  | Ast_preserved
  | Docstrings_moved of Cmt.error list
  | Ast_changed

val equivalent :
     'a t
  -> normalize_code:(string -> string)
  -> Conf.t
  -> 'a
  -> 'a
  -> ast_check_result
(** Check whether formatting preserved the standard AST. For [Repl_file]
    and [Documentation], always returns [Ast_preserved]. *)
