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

module Cases = struct
  type t =
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

  let get c =
    match c.Conf.break_cases with
    | `Fit -> fit
    | `Nested -> nested
    | `Toplevel -> toplevel
    | `All -> all
end
