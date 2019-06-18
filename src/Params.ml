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

module Format = Format_
open Migrate_ast
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
  | `Fit_or_vertical ->
      { leading_space= break_unless_newline 1000 0
      ; bar= fmt "| "
      ; box_all= hovbox indent
      ; box_pattern_arrow= hovbox 0
      ; break_before_arrow= fmt "@;<1 2>"
      ; break_after_arrow= fmt_if (not parens_here) "@;<0 3>"
      ; break_after_opening_paren= fmt "@ " }
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
      let space = if c.space_around_records then 1 else 0 in
      { docked_before= fmt " {"
      ; break_before= break space 0
      ; box_record= Fn.id
      ; sep_before= noop
      ; sep_after= fmt_or sparse_type_decl "@;<1000 0>" "@ "
      ; break_after= break space (-2)
      ; docked_after= fmt "}" }

type if_then_else =
  { box_branch: Fmt.t -> Fmt.t
  ; cond: Fmt.t
  ; box_keyword_and_expr: Fmt.t -> Fmt.t
  ; branch_pro: Fmt.t
  ; wrap_parens: Fmt.t -> Fmt.t
  ; expr_pro: Fmt.t option
  ; expr_eol: Fmt.t option
  ; break_end_branch: Fmt.t
  ; space_between_branches: Fmt.t }

let get_if_then_else (c : Conf.t) ~first ~last ~parens ~parens_bch ~xcond
    ~expr_loc ~fmt_extension_suffix ~fmt_attributes ~fmt_cond =
  let imd = c.indicate_multiline_delimiters in
  let cond () =
    match xcond with
    | Some xcnd ->
        hvbox
          (if parens then -2 else 0)
          ( hvbox
              (if parens then 0 else 2)
              ( fmt_if (not first) "else "
              $ str "if"
              $ fmt_if_k first fmt_extension_suffix
              $ fmt_attributes $ fmt "@ " $ fmt_cond xcnd )
          $ fmt "@ then" )
    | None -> str "else"
  in
  let wrap_parens ~opn_hint:(ohimd, ohno) k =
    fmt_if_k parens_bch
      (str "(" $ fits_breaks "" ~hint:(if imd then ohimd else ohno) "")
    $ k
    $ fmt_if_k parens_bch
        (fmt_if_k imd (fits_breaks "" ~hint:(1, 0) "") $ str ")")
  in
  let branch_pro = fmt_or parens_bch " " "@;<1 2>" in
  match c.if_then_else with
  | `Compact ->
      { box_branch= hovbox (if first && parens then 0 else 2)
      ; cond= cond ()
      ; box_keyword_and_expr= Fn.id
      ; branch_pro= fmt_or parens_bch " " "@ "
      ; wrap_parens= wrap_parens ~opn_hint:((1, 0), (0, 0))
      ; expr_pro= None
      ; expr_eol= None
      ; break_end_branch= noop
      ; space_between_branches= fmt "@ " }
  | `K_R ->
      { box_branch= Fn.id
      ; cond= cond ()
      ; box_keyword_and_expr= Fn.id
      ; branch_pro
      ; wrap_parens= wrap_if parens_bch "(@;<0 2>" ")"
      ; expr_pro= None
      ; expr_eol= Some (fmt "@;<1 2>")
      ; break_end_branch= fmt_if_k (parens_bch || not last) (break 1000 0)
      ; space_between_branches= fmt_if parens_bch " " }
  | `Fit_or_vertical ->
      { box_branch= hovbox 0
      ; cond= cond ()
      ; box_keyword_and_expr= Fn.id
      ; branch_pro
      ; wrap_parens= wrap_parens ~opn_hint:((1, 2), (0, 2))
      ; expr_pro=
          Some
            (fmt_if_k
               (not (Location.is_single_line expr_loc c.margin))
               (break_unless_newline 1000 2))
      ; expr_eol= Some (fmt "@;<1 2>")
      ; break_end_branch= noop
      ; space_between_branches= fmt "@ " }
  | `Keyword_first ->
      { box_branch= Fn.id
      ; cond=
          opt xcond (fun xcnd ->
              hvbox 2
                ( fmt_or_k first
                    (str "if" $ fmt_extension_suffix)
                    (str "else if")
                $ fmt_attributes $ str " " $ fmt_cond xcnd )
              $ fmt "@ ")
      ; box_keyword_and_expr=
          (fun k ->
            hvbox 2 (fmt_or (Option.is_some xcond) "then" "else" $ k))
      ; branch_pro= fmt_or parens_bch " " "@ "
      ; wrap_parens= wrap_parens ~opn_hint:((1, 0), (0, 0))
      ; expr_pro= None
      ; expr_eol= None
      ; break_end_branch= noop
      ; space_between_branches= fmt "@ " }
