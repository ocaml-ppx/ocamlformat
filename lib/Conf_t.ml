type from = Config_option.from

(** Configuration options *)

module Elt = struct
  type 'a t = {v: 'a; from: Config_option.from}

  let v elt = elt.v

  let from elt = elt.from

  let make v from = {v; from}
end

type 'a elt = 'a Elt.t

type fmt_opts =
  { align_pattern_matching_bar: [`Paren | `Keyword] elt
  ; assignment_operator: [`Begin_line | `End_line] elt
  ; break_before_in: [`Fit_or_vertical | `Auto] elt
  ; break_cases:
      [`Fit | `Nested | `Toplevel | `Fit_or_vertical | `All | `Vertical] elt
  ; break_collection_expressions: [`Wrap | `Fit_or_vertical] elt
  ; break_colon: [`Before | `After] elt
  ; break_infix: [`Wrap | `Fit_or_vertical | `Wrap_or_vertical] elt
  ; break_infix_before_func: bool elt
  ; break_fun_decl: [`Wrap | `Fit_or_vertical | `Smart] elt
  ; break_fun_sig: [`Wrap | `Fit_or_vertical | `Smart] elt
  ; break_separators: [`Before | `After] elt
  ; break_sequences: bool elt
  ; break_string_literals: [`Auto | `Never] elt
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
  ; let_binding_spacing: [`Compact | `Sparse | `Double_semicolon] elt
  ; let_module: [`Compact | `Sparse] elt
  ; line_endings: [`Lf | `Crlf] elt
  ; margin: int elt
  ; match_indent: int elt
  ; match_indent_nested: [`Always | `Auto | `Never] elt
  ; max_indent: int option elt
  ; module_item_spacing: [`Compact | `Preserve | `Sparse] elt
  ; nested_match: [`Wrap | `Align] elt
  ; ocp_indent_compat: bool elt
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
  ; wrap_comments: bool elt
  ; wrap_fun_args: bool elt }

type opr_opts =
  { comment_check: bool elt
  ; debug: bool elt
  ; disable: bool elt
  ; margin_check: bool elt
  ; max_iters: int elt
  ; ocaml_version: Ocaml_version.t elt
  ; quiet: bool elt
  ; range: (string -> Range.t) elt
  ; disable_conf_attrs: bool elt
  ; version_check: bool elt }

type t = {fmt_opts: fmt_opts; opr_opts: opr_opts}
