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

(** This module contains the types of configuration options, along with small
    helper functions. It is separated from [Conf] to avoid dependency cycles. *)

type parsed_from = [`File of Location.t | `Attribute of Location.t]

type updated_from = [`Env | `Commandline | `Parsed of parsed_from]

type from =
  [ `Default
  | `Profile of string * updated_from
  | `Updated of updated_from * from option (* when redundant definition *) ]

module Error : sig
  type t =
    | Bad_value of string * string
    | Malformed of string
    | Misplaced of string * string
    | Unknown of string * [`Msg of string] option
    | Version_mismatch of {read: string; installed: string}

  val to_string : t -> string
end

module Elt : sig
  (** An ['a Elt.t] represent a set config option of type ['a], along with
      the indication of where the option was set (commandline, config file
      etc). *)
  type 'a t = {v: 'a; from: from}

  val v : 'a t -> 'a

  val from : 'a t -> from

  val make : 'a -> from -> 'a t
end

type 'a elt = 'a Elt.t

(** Formatting options *)
type fmt_opts =
  { align_symbol_open_paren: bool elt
  ; assignment_operator: [`Begin_line | `End_line] elt
  ; break_before_in: [`Fit_or_vertical | `Auto] elt
  ; break_cases:
      [`Fit | `Nested | `Toplevel | `Fit_or_vertical | `Vertical | `All] elt
  ; break_collection_expressions: [`Wrap | `Fit_or_vertical] elt
  ; break_colon: [`Before | `After] elt
  ; break_infix: [`Wrap | `Fit_or_vertical | `Wrap_or_vertical] elt
  ; break_infix_before_func: bool elt
  ; break_fun_decl: [`Wrap | `Fit_or_vertical | `Smart] elt
  ; break_fun_sig: [`Wrap | `Fit_or_vertical | `Smart] elt
  ; break_separators: [`Before | `After] elt
  ; break_sequences: bool elt
  ; break_string_literals: [`Auto | `Never] elt
        (** How to potentially break string literals into new lines. *)
  ; break_struct: bool elt
  ; cases_exp_indent: int elt
  ; cases_matching_exp_indent: [`Normal | `Compact] elt
  ; disambiguate_non_breaking_match: bool elt
  ; doc_comments: [`Before | `Before_except_val | `After_when_possible] elt
  ; doc_comments_padding: int elt
  ; doc_comments_tag_only: [`Fit | `Default] elt
  ; dock_collection_brackets: bool elt
  ; exp_grouping: [`Parens | `Preserve] elt
  ; extension_indent: int elt
  ; field_space: [`Tight | `Loose | `Tight_decl] elt
  ; function_indent: int elt
  ; function_indent_nested: [`Always | `Auto | `Never] elt
  ; if_then_else:
      [`Compact | `Fit_or_vertical | `Keyword_first | `K_R | `Vertical] elt
  ; indent_after_in: int elt
  ; indicate_multiline_delimiters:
      [`No | `Space | `Closing_on_separate_line] elt
  ; indicate_nested_or_patterns: [`Space | `Unsafe_no] elt
  ; infix_precedence: [`Indent | `Parens] elt
  ; leading_nested_match_parens: bool elt
  ; let_and: [`Compact | `Sparse] elt
  ; let_binding_indent: int elt
  ; let_binding_deindent_fun: bool elt
        (** De-indent the [fun] in a let-binding body. *)
  ; let_binding_spacing: [`Compact | `Sparse | `Double_semicolon] elt
  ; let_module: [`Compact | `Sparse] elt
  ; line_endings: [`Lf | `Crlf] elt
  ; margin: int elt  (** Format code to fit within [margin] columns. *)
  ; match_indent: int elt
  ; match_indent_nested: [`Always | `Auto | `Never] elt
  ; max_indent: int option elt
  ; module_item_spacing: [`Compact | `Preserve | `Sparse] elt
  ; nested_match: [`Wrap | `Align] elt
  ; ocp_indent_compat: bool elt  (** Try to indent like ocp-indent *)
  ; parens_ite: bool elt
  ; parens_tuple: [`Always | `Multi_line_only] elt
  ; parens_tuple_patterns: [`Always | `Multi_line_only] elt
  ; parse_docstrings: bool elt
  ; parse_toplevel_phrases: bool elt
  ; sequence_blank_line: [`Compact | `Preserve_one] elt
  ; sequence_style: [`Before | `Separator | `Terminator] elt
  ; single_case: [`Compact | `Sparse] elt
  ; space_around_arrays: bool elt
  ; space_around_lists: bool elt
  ; space_around_records: bool elt
  ; space_around_variants: bool elt
  ; stritem_extension_indent: int elt
  ; type_decl: [`Compact | `Sparse] elt
  ; type_decl_indent: int elt
  ; wrap_comments: bool elt  (** Wrap comments at margin. *)
  ; wrap_fun_args: bool elt }

(** Options changing the tool's behavior *)
type opr_opts =
  { comment_check: bool elt
  ; debug: bool elt  (** Generate debugging output if true. *)
  ; disable: bool elt
  ; margin_check: bool elt
        (** Check whether the formatted output exceeds the margin. *)
  ; max_iters: int elt
        (** Fail if output of formatting does not stabilize within
            [max_iters] iterations. *)
  ; ocaml_version: Ocaml_version.t elt
        (** Version of OCaml syntax of the output. *)
  ; quiet: bool elt
  ; range: (string -> Range.t) elt
  ; disable_conf_attrs: bool elt
  ; version_check: bool elt }

type t =
  { fmt_opts: fmt_opts
  ; opr_opts: opr_opts
  ; profile: [`default | `conventional | `ocamlformat | `janestreet] elt }
