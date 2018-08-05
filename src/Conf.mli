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
  { break_infix: [`Wrap | `Fit_or_vertical]
  ; break_string_literals: [`Newlines | `Never | `Wrap]
        (** How to potentially break string literals into new lines. *)
  ; no_comment_check: bool
  ; doc_comments: [`Before | `After]
  ; escape_chars: [`Decimal | `Hexadecimal | `Preserve]
        (** Escape encoding for chars literals. *)
  ; escape_strings: [`Decimal | `Hexadecimal | `Preserve]
        (** Escape encoding for string literals. *)
  ; if_then_else: [`Compact | `Keyword_first]
  ; infix_precedence: [`Indent | `Parens]
  ; margin: int  (** Format code to fit within [margin] columns. *)
  ; max_iters: int
        (** Fail if output of formatting does not stabilize within
            [max_iters] iterations. *)
  ; ocp_indent_compat: bool  (** Try to indent like ocp-indent *)
  ; parens_tuple: [`Always | `Multi_line_only]
  ; quiet: bool
  ; sparse: bool  (** Generate more sparsely formatted code if true. *)
  ; type_decl: [`Compact | `Sparse]
  ; wrap_comments: bool  (** Wrap comments at margin. *) }

type 'a input = {kind: 'a; name: string; file: string; conf: t}

type action =
  | In_out of [`Impl | `Intf | `Use_file] input * string option
      (** Format input file of given kind to output file, or stdout if None. *)
  | Inplace of [`Impl | `Intf | `Use_file] input list
      (** Format in-place, overwriting input file(s). *)

val action : action
(** Formatting action: input type and source, and output destination. *)

val debug : bool
(** Generate debugging output if true. *)

val parse_line_in_attribute :
     t
  -> string
  -> ( t
     , [ `Unknown of string * string
       | `Bad_value of string * string
       | `Malformed of string
       | `Misplaced of string * string ] )
     Result.t
