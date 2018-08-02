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

type t = private
  { margin: int  (** Format code to fit within [margin] columns. *)
  ; sparse: bool  (** Generate more sparsely formatted code if true. *)
  ; max_iters: int
        (** Fail if output of formatting does not stabilize within
            [max_iters] iterations. *)
  ; escape_chars: [`Decimal | `Hexadecimal | `Preserve]
        (** Escape encoding for chars literals. *)
  ; escape_strings: [`Decimal | `Hexadecimal | `Preserve]
        (** Escape encoding for string literals. *)
  ; break_string_literals: [`Never | `Newlines | `Wrap]
        (** How to potentially break string literals into new lines. *)
  ; wrap_comments: bool  (** Wrap comments at margin. *)
  ; doc_comments: [`Before | `After]
  ; parens_tuple: [`Always | `Multi_line_only]
  ; if_then_else: [`Compact | `Keyword_first]
  ; break_infix: [`Wrap | `Fit_or_vertical]
  ; ocp_indent_compat: bool  (** Try to indent like ocp-indent *)
  ; quiet: bool
  ; dont_check_comments: bool }

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
