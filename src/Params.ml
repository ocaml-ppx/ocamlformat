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
  ; box_pattern_guard: Fmt.t -> Fmt.t
  ; break_before_arrow: Fmt.t
  ; break_after_arrow: Fmt.t
  ; break_after_opening_paren: Fmt.t
  ; box_rhs: Fmt.t -> Fmt.t }

let fit ~first ~indent ~parens_here:_ =
  { leading_space= fmt_if (not first) "@ "
  ; bar= (if first then if_newline "| " else fmt "| ")
  ; box_all= hvbox indent
  ; box_pattern_arrow= hovbox 2
  ; box_pattern_guard= hvbox 0
  ; break_before_arrow= fmt "@;<1 0>"
  ; break_after_arrow= fmt ""
  ; break_after_opening_paren= fmt "@ "
  ; box_rhs= hovbox 0 }

let nested ~first ~indent ~parens_here =
  { leading_space= fmt_if (not first) "@ "
  ; bar= (if first then if_newline "| " else fmt "| ")
  ; box_all= Fn.id
  ; box_pattern_arrow= hovbox 0
  ; box_pattern_guard= hvbox 0
  ; break_before_arrow= fmt "@;<1 2>"
  ; break_after_arrow= fmt_if_k (not parens_here) (fmt "@;<0 3>")
  ; break_after_opening_paren=
      fmt_or_k (indent > 2) (fmt "@;<1 4>") (fmt "@;<1 2>")
  ; box_rhs= hovbox 0 }

let toplevel ~first:_ ~indent ~parens_here =
  { leading_space= break_unless_newline 1000 0
  ; bar= fmt "| "
  ; box_all= hvbox indent
  ; box_pattern_arrow= hovbox 0
  ; box_pattern_guard= hvbox 0
  ; break_before_arrow= fmt "@;<1 2>"
  ; break_after_arrow= fmt_if_k (not parens_here) (fmt "@;<0 3>")
  ; break_after_opening_paren= fmt "@ "
  ; box_rhs= hovbox 0 }

let all ~first ~indent ~parens_here =
  { leading_space= fmt_if (not first) "@ "
  ; bar= break_unless_newline 1000 0 $ fmt "| "
  ; box_all= hvbox indent
  ; box_pattern_arrow= hovbox 0
  ; box_pattern_guard= hvbox 0
  ; break_before_arrow=
      fmt_or_k parens_here (fmt "@;<1 2>") (fmt "@;<1 -2>")
  ; break_after_arrow= fmt_if_k (not parens_here) (fmt "@;<0 3>")
  ; break_after_opening_paren= fmt "@ "
  ; box_rhs= hovbox 0 }

let get_cases (c : Conf.t) =
  match c.break_cases with
  | `Fit -> fit
  | `Nested -> nested
  | `Toplevel -> toplevel
  | `All -> all

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
      { docked_before= fmt ""
      ; break_before= fmt "@ "
      ; box_record= (fun k -> hvbox 0 (wrap_record c k))
      ; sep_before= fmt_or sparse_type_decl "@;<1000 0>; " "@,; "
      ; sep_after= fmt ""
      ; break_after= fmt ""
      ; docked_after= fmt "" }
  | `After ->
      { docked_before= fmt ""
      ; break_before= fmt "@ "
      ; box_record= (fun k -> hvbox 2 (wrap_record c k))
      ; sep_before= fmt ""
      ; sep_after= fmt_or sparse_type_decl "@;<1000 0>" "@ "
      ; break_after= fmt ""
      ; docked_after= fmt "" }
  | `After_and_docked ->
      let space = if c.space_around_collection_expressions then 1 else 0 in
      { docked_before= fmt " {"
      ; break_before= break space 0
      ; box_record= Fn.id
      ; sep_before= fmt ""
      ; sep_after= fmt_or sparse_type_decl "@;<1000 0>" "@ "
      ; break_after= break space (-2)
      ; docked_after= fmt "}" }
