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

(** Configuration options *)

(** Formatting options *)
type fmt_opts =
  { align_pattern_matching_bar: [`Paren | `Keyword]
  ; assignment_operator: [`Begin_line | `End_line]
  ; break_before_in: [`Fit_or_vertical | `Auto]
  ; break_cases:
      [`Fit | `Nested | `Toplevel | `Fit_or_vertical | `Vertical | `All]
  ; break_collection_expressions: [`Wrap | `Fit_or_vertical]
  ; break_colon: [`Before | `After]
  ; break_infix: [`Wrap | `Fit_or_vertical | `Wrap_or_vertical]
  ; break_infix_before_func: bool
  ; break_fun_decl: [`Wrap | `Fit_or_vertical | `Smart]
  ; break_fun_sig: [`Wrap | `Fit_or_vertical | `Smart]
  ; break_separators: [`Before | `After]
  ; break_sequences: bool
  ; break_string_literals: [`Auto | `Never]
        (** How to potentially break string literals into new lines. *)
  ; break_struct: bool
  ; cases_exp_indent: int
  ; cases_matching_exp_indent: [`Normal | `Compact]
  ; disambiguate_non_breaking_match: bool
  ; doc_comments: [`Before | `Before_except_val | `After_when_possible]
  ; doc_comments_padding: int
  ; doc_comments_tag_only: [`Fit | `Default]
  ; dock_collection_brackets: bool
  ; exp_grouping: [`Parens | `Preserve]
  ; extension_indent: int
  ; field_space: [`Tight | `Loose | `Tight_decl]
  ; function_indent: int
  ; function_indent_nested: [`Always | `Auto | `Never]
  ; if_then_else:
      [`Compact | `Fit_or_vertical | `Vertical | `Keyword_first | `K_R]
  ; indent_after_in: int
  ; indicate_multiline_delimiters: [`No | `Space | `Closing_on_separate_line]
  ; indicate_nested_or_patterns: [`Space | `Unsafe_no]
  ; infix_precedence: [`Indent | `Parens]
  ; leading_nested_match_parens: bool
  ; let_and: [`Compact | `Sparse]
  ; let_binding_indent: int
  ; let_binding_spacing: [`Compact | `Sparse | `Double_semicolon]
  ; let_module: [`Compact | `Sparse]
  ; line_endings: [`Lf | `Crlf]
  ; margin: int  (** Format code to fit within [margin] columns. *)
  ; match_indent: int
  ; match_indent_nested: [`Always | `Auto | `Never]
  ; max_indent: int option
  ; module_item_spacing: [`Compact | `Preserve | `Sparse]
  ; nested_match: [`Wrap | `Align]
  ; ocp_indent_compat: bool  (** Try to indent like ocp-indent *)
  ; parens_ite: bool
  ; parens_tuple: [`Always | `Multi_line_only]
  ; parens_tuple_patterns: [`Always | `Multi_line_only]
  ; parse_docstrings: bool
  ; parse_toplevel_phrases: bool
  ; sequence_blank_line: [`Compact | `Preserve_one]
  ; sequence_style: [`Before | `Separator | `Terminator]
  ; single_case: [`Compact | `Sparse]
  ; space_around_arrays: bool
  ; space_around_lists: bool
  ; space_around_records: bool
  ; space_around_variants: bool
  ; stritem_extension_indent: int
  ; type_decl: [`Compact | `Sparse]
  ; type_decl_indent: int
  ; wrap_comments: bool  (** Wrap comments at margin. *)
  ; wrap_fun_args: bool }

val default_profile : fmt_opts

(** Options changing the tool's behavior *)
type opr_opts =
  { comment_check: bool
  ; debug: bool  (** Generate debugging output if true. *)
  ; disable: bool
  ; margin_check: bool
        (** Check whether the formatted output exceeds the margin. *)
  ; max_iters: int
        (** Fail if output of formatting does not stabilize within
            [max_iters] iterations. *)
  ; ocaml_version: Ocaml_version.t
        (** Version of OCaml syntax of the output. *)
  ; quiet: bool
  ; range: string -> Range.t
  ; disable_conf_attrs: bool
  ; version_check: bool }

type t = {fmt_opts: fmt_opts; opr_opts: opr_opts}

val default : t

val update : ?quiet:bool -> t -> Parsetree.attribute -> t
(** [update ?quiet c a] updates configuration [c] after reading attribute
    [a]. [quiet] is false by default. *)

val update_value :
  t -> name:string -> value:string -> (t, Config_option.Error.t) Result.t

val update_state : t -> [`Enable | `Disable] -> t

val parse_line :
     t
  -> ?version_check:bool
  -> ?disable_conf_attrs:bool
  -> from:[< `Attribute of Warnings.loc | `File of Warnings.loc]
  -> string
  -> (t, Config_option.Error.t) Result.t

val print_config : t -> unit

val collect_warnings : (unit -> t) -> t * (unit -> unit)

val warn :
  loc:Warnings.loc -> ('a, Format.formatter, unit, unit) format4 -> 'a

module UI : sig
  val profile : t Config_option.UI.t

  val fmt_opts : t Config_option.UI.t list

  val opr_opts : t Config_option.UI.t list
end

module C : Config_option.S with type config = t

module Operational : sig
  val update : f:(opr_opts -> opr_opts) -> t -> t
end
