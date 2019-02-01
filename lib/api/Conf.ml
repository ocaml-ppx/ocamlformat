(**********************************************************************
 *                                                                    *
 *                            OCamlFormat                             *
 *                                                                    *
 *  Copyright (c) 2017-present, Facebook, Inc.  All rights reserved.  *
 *                                                                    *
 *  This source code is licensed under the MIT license found in the   *
 *  LICENSE file in the root directory of this source tree.           *
 *                                                                    *
 **********************************************************************)

(** Configuration options *)

type t =
  { break_cases: [`Fit | `Nested | `All]
  ; break_collection_expressions: [`Wrap | `Fit_or_vertical]
  ; break_infix: [`Wrap | `Fit_or_vertical]
  ; break_infix_before_func: bool
  ; break_separators: [`Before | `After | `After_and_docked]
  ; break_sequences: bool
  ; break_string_literals: [`Newlines | `Never | `Wrap]
  ; break_struct: bool
  ; cases_exp_indent: int
  ; comment_check: bool
  ; disable: bool
  ; doc_comments: [`Before | `After]
  ; escape_chars: [`Decimal | `Hexadecimal | `Preserve]
  ; escape_strings: [`Decimal | `Hexadecimal | `Preserve]
  ; extension_sugar: [`Preserve | `Always]
  ; field_space: [`Tight | `Loose]
  ; if_then_else: [`Compact | `Keyword_first]
  ; indicate_multiline_delimiters: bool
  ; indicate_nested_or_patterns: bool
  ; infix_precedence: [`Indent | `Parens]
  ; leading_nested_match_parens: bool
  ; let_and: [`Compact | `Sparse]
  ; let_binding_spacing: [`Compact | `Sparse | `Double_semicolon]
  ; let_open: [`Preserve | `Auto | `Short | `Long]
  ; margin: int
  ; max_iters: int
  ; module_item_spacing: [`Compact | `Sparse]
  ; ocp_indent_compat: bool
  ; parens_ite: bool
  ; parens_tuple: [`Always | `Multi_line_only]
  ; parens_tuple_patterns: [`Always | `Multi_line_only]
  ; parse_docstrings: bool
  ; quiet: bool
  ; sequence_style: [`Separator | `Terminator]
  ; single_case: [`Compact | `Sparse]
  ; space_around_collection_expressions: bool
  ; type_decl: [`Compact | `Sparse]
  ; wrap_comments: bool
  ; wrap_fun_args: bool }

let debug = ref false

type 'a input = {kind: 'a; name: string; file: string; conf: t}

type action =
  | In_out of [`Impl | `Intf | `Use_file] input * string option
      (** Format input file (or [-] for stdin) of given kind to output file,
          or stdout if None. *)
  | Inplace of [`Impl | `Intf | `Use_file] input list
      (** Format in-place, overwriting input file(s). *)

let action = ref (fun () -> failwith "action undefined")

let parse_line_in_attribute =
  ref (fun _ _ -> failwith "parse_line_in_attribute undefined")
