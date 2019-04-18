(**********************************************************************
 *                                                                    *
 *                            OCamlFormat                             *
 *                                                                    *
 *  Copyright (c) 2019-present, Facebook, Inc.  All rights reserved.  *
 *                                                                    *
 *  This source code is licensed under the MIT license found in the   *
 *  LICENSE file in the root directory of this source tree.           *
 *                                                                    *
 **********************************************************************)

open Fmt

type cases =
  { leading_space: Fmt.t
  ; bar: Fmt.t
  ; box_all: Fmt.t -> Fmt.t
  ; box_pattern_arrow: Fmt.t -> Fmt.t
  ; break_before_arrow: Fmt.t
  ; break_after_arrow: Fmt.t
  ; break_after_opening_paren: Fmt.t }

let get_cases (c : Conf.t) ~first ~indent ~parens_here =
  match c.break_cases with
  | `Fit ->
      { leading_space= fmt_if (not first) "@ "
      ; bar= fmt_or_k first (if_newline "| ") (fmt "| ")
      ; box_all= hvbox indent
      ; box_pattern_arrow= hovbox 2
      ; break_before_arrow= fmt "@;<1 0>"
      ; break_after_arrow= noop
      ; break_after_opening_paren= fmt "@ " }
  | `Nested ->
      { leading_space= fmt_if (not first) "@ "
      ; bar= fmt_or_k first (if_newline "| ") (fmt "| ")
      ; box_all= Fn.id
      ; box_pattern_arrow= hovbox 0
      ; break_before_arrow= fmt "@;<1 2>"
      ; break_after_arrow= fmt_if (not parens_here) "@;<0 3>"
      ; break_after_opening_paren= fmt_or (indent > 2) "@;<1 4>" "@;<1 2>"
      }
  | `Toplevel | `All ->
      { leading_space= break_unless_newline 1000 0
      ; bar= fmt "| "
      ; box_all= hvbox indent
      ; box_pattern_arrow= hovbox 0
      ; break_before_arrow= fmt "@;<1 2>"
      ; break_after_arrow= fmt_if (not parens_here) "@;<0 3>"
      ; break_after_opening_paren= fmt "@ " }

type record_type =
  { docked_before: Fmt.t
  ; break_before: Fmt.t
  ; box_record: Fmt.t -> Fmt.t
  ; sep_before: Fmt.t
  ; sep_after: Fmt.t
  ; break_after: Fmt.t
  ; docked_after: Fmt.t }

let get_record_type (c : Conf.t) ~wrap_record =
  let sparse_type_decl = Poly.(c.type_decl = `Sparse) in
  match c.break_separators with
  | `Before ->
      { docked_before= noop
      ; break_before= fmt "@ "
      ; box_record= (fun k -> hvbox 0 (wrap_record c k))
      ; sep_before= fmt_or sparse_type_decl "@;<1000 0>; " "@,; "
      ; sep_after= noop
      ; break_after= noop
      ; docked_after= noop }
  | `After ->
      { docked_before= noop
      ; break_before= fmt "@ "
      ; box_record= (fun k -> hvbox 2 (wrap_record c k))
      ; sep_before= noop
      ; sep_after= fmt_or sparse_type_decl "@;<1000 0>" "@ "
      ; break_after= noop
      ; docked_after= noop }
  | `After_and_docked ->
      let space = if c.space_around_collection_expressions then 1 else 0 in
      { docked_before= fmt " {"
      ; break_before= break space 0
      ; box_record= Fn.id
      ; sep_before= noop
      ; sep_after= fmt_or sparse_type_decl "@;<1000 0>" "@ "
      ; break_after= break space (-2)
      ; docked_after= fmt "}" }
