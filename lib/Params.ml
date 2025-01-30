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

module Location = Migrate_ast.Location
open Extended_ast
open Asttypes
open Fmt
open Ast

(** Shorthand for a commonly used option. *)
let ocp c = c.Conf.fmt_opts.ocp_indent_compat.v

(** Whether [exp] occurs in [args] as a labelled argument. *)
let is_labelled_arg args exp =
  List.exists
    ~f:(function
      | Nolabel, _ -> false
      | Labelled _, x | Optional _, x -> phys_equal x exp )
    args

(** Like [is_labelled_arg] but look at an expression's context. *)
let is_labelled_arg' xexp =
  match xexp.Ast.ctx with
  | Exp {pexp_desc= Pexp_apply (_, args); _} -> is_labelled_arg args xexp.ast
  | _ -> false

let parens_if parens (c : Conf.t) ?(disambiguate = false) k =
  if disambiguate && c.fmt_opts.disambiguate_non_breaking_match.v then
    wrap_if_fits_or parens "(" ")" k
  else if not parens then k
  else
    match c.fmt_opts.indicate_multiline_delimiters.v with
    | `Space ->
        Fmt.fits_breaks "(" "(" $ k $ Fmt.fits_breaks ")" ~hint:(1, 0) ")"
    | `Closing_on_separate_line ->
        Fmt.fits_breaks "(" "(" $ k $ Fmt.fits_breaks ")" ~hint:(1000, 0) ")"
    | `No -> wrap "(" ")" k

let parens c ?disambiguate k = parens_if true c ?disambiguate k

module Exp = struct
  module Infix_op_arg = struct
    let wrap (c : Conf.t) ?(parens_nested = false) ~parens k =
      if parens || parens_nested then
        let opn, hint, cls =
          if parens || Poly.(c.fmt_opts.infix_precedence.v = `Parens) then
            match c.fmt_opts.indicate_multiline_delimiters.v with
            | `Space -> ("( ", Some (1, 0), ")")
            | `No -> ("(", Some (0, 0), ")")
            | `Closing_on_separate_line -> ("(", Some (1000, 0), ")")
          else ("", None, "")
        in
        wrap_if_k (parens || parens_nested) (Fmt.fits_breaks "(" opn)
          (Fmt.fits_breaks ")" ?hint cls)
          k
      else k

    let dock (c : Conf.t) xarg =
      if not c.fmt_opts.ocp_indent_compat.v then false
      else
        match xarg.ast.pexp_desc with
        | Pexp_apply (_, args) -> (
          (* Rhs is an apply and it ends with a [fun]. *)
          match List.last_exn args with
          | _, {pexp_desc= Pexp_fun _ | Pexp_newtype _ | Pexp_function _; _}
            ->
              true
          | _ -> false )
        | Pexp_match _ | Pexp_try _ -> true
        | _ -> false
  end

  let wrap (c : Conf.t) ?(disambiguate = false) ?(fits_breaks = true)
      ?(offset_closing_paren = 0) ~parens k =
    if disambiguate && c.fmt_opts.disambiguate_non_breaking_match.v then
      wrap_if_fits_or parens "(" ")" k
    else if not parens then k
    else if fits_breaks then wrap_fits_breaks ~space:false c "(" ")" k
    else
      match c.fmt_opts.indicate_multiline_delimiters.v with
      | `Space ->
          Fmt.fits_breaks "(" "(" $ k $ Fmt.fits_breaks ")" ~hint:(1, 0) ")"
      | `Closing_on_separate_line ->
          Fmt.fits_breaks "(" "(" $ k
          $ Fmt.fits_breaks ")" ~hint:(1000, offset_closing_paren) ")"
      | `No -> wrap "(" ")" k

  let box_fun_decl_args c ~parens ~kw ~args ~annot =
    let box_decl, should_box_args =
      if ocp c then (hvbox (if parens then 1 else 2), false)
      else (hovbox 4, not c.fmt_opts.wrap_fun_args.v)
    in
    box_decl (kw $ hvbox_if should_box_args 0 args $ fmt_opt annot)
end

module Mod = struct
  type args = {dock: bool; arg_psp: Fmt.t; indent: int; align: bool}

  let arg_is_sig arg =
    match arg.txt with
    | Named
        ( _
        , { pmty_desc=
              Pmty_signature _ | Pmty_typeof {pmod_desc= Pmod_structure _; _}
          ; _ } ) ->
        true
    | _ -> false

  let get_args (c : Conf.t) args =
    let indent, psp_indent = if ocp c then (2, 2) else (0, 4) in
    let dock =
      (* ocp-indent-compat: Dock only one argument to avoid alignment of
         subsequent arguments. *)
      if ocp c then match args with [arg] -> arg_is_sig arg | _ -> false
      else List.for_all ~f:arg_is_sig args
    in
    let arg_psp = if dock then str " " else break 1 psp_indent in
    let align = ocp c in
    {dock; arg_psp; indent; align}

  let break_constraint c ~rhs =
    if ocp c then
      match rhs.pmty_desc with
      | Pmty_signature _ when ocp c -> break 1 0
      | _ -> break 1 2
    else break 1 2
end

module Pcty = struct
  let is_sig rhs =
    match rhs.pcty_desc with Pcty_signature _ -> true | _ -> false

  let arrow (c : Conf.t) ~rhs =
    let pre, post =
      match c.fmt_opts.break_separators.v with
      | `Before -> (fmt "@ ", str " ")
      | `After -> (str " ", fmt "@ ")
    in
    let post = if is_sig rhs then break 1 ~-2 else post in
    pre $ str "->" $ post

  let break_let_open _conf ~rhs = break 1000 (if is_sig rhs then ~-2 else 0)
end

let get_or_pattern_sep ?(cmts_before = false) ?(space = false) (c : Conf.t)
    ~ctx =
  let nspaces = if cmts_before then 1000 else 1 in
  match ctx with
  | Ast.Exp {pexp_desc= Pexp_function _ | Pexp_match _ | Pexp_try _; _} -> (
    match c.fmt_opts.break_cases.v with
    | `Nested -> break nspaces 0 $ str "| "
    | _ -> (
        let nspaces =
          match c.fmt_opts.break_cases.v with
          | `All | `Vertical -> 1000
          | _ -> nspaces
        in
        match c.fmt_opts.indicate_nested_or_patterns.v with
        | `Space ->
            cbreak ~fits:("", nspaces, "| ")
              ~breaks:("", 0, if space then " | " else " |")
        | `Unsafe_no -> break nspaces 0 $ str "| " ) )
  | _ -> break nspaces 0 $ str "| "

type cases =
  { leading_space: Fmt.t
  ; bar: Fmt.t
  ; box_all: Fmt.t -> Fmt.t
  ; box_pattern_arrow: Fmt.t -> Fmt.t
  ; break_before_arrow: Fmt.t
  ; break_after_arrow: Fmt.t
  ; open_paren_branch: Fmt.t
  ; break_after_opening_paren: Fmt.t
  ; expr_parens: bool option
  ; branch_expr: expression Ast.xt
  ; close_paren_branch: Fmt.t }

let get_cases (c : Conf.t) ~ctx ~first ~last ~xbch:({ast; _} as xast) =
  let indent =
    match (c.fmt_opts.cases_matching_exp_indent.v, (ctx, ast.pexp_desc)) with
    | ( `Compact
      , ( Exp {pexp_desc= Pexp_function _ | Pexp_match _ | Pexp_try _; _}
        , (Pexp_match _ | Pexp_try _ | Pexp_beginend _) ) ) ->
        2
    | _, _ -> c.fmt_opts.cases_exp_indent.v
  in
  let align_nested_match =
    match (ast.pexp_desc, c.fmt_opts.nested_match.v) with
    | (Pexp_match _ | Pexp_try _), `Align -> last
    | _ -> false
  in
  let body_has_parens =
    match ast.pexp_desc with
    | Pexp_tuple _ when Poly.(c.fmt_opts.parens_tuple.v = `Always) ->
        (* [fmt_expression] doesn't respect [~parens] for tuples when this
           option is set to [always]. *)
        true
    | _ -> Ast.Exp.is_symbol ast
  in
  let parens_branch, expr_parens =
    if align_nested_match then (false, Some false)
    else if c.fmt_opts.leading_nested_match_parens.v then (false, None)
    else (parenze_exp xast && not body_has_parens, Some false)
  in
  let indent = if align_nested_match then 0 else indent in
  let open_paren_branch, close_paren_branch, branch_expr =
    match ast with
    | {pexp_desc= Pexp_beginend nested_exp; pexp_attributes= []; _} ->
        let close_paren =
          let offset =
            match c.fmt_opts.break_cases.v with `Nested -> 0 | _ -> -2
          in
          fits_breaks " end" ~level:1 ~hint:(1000, offset) "end"
        in
        (fmt "@;<1 0>begin", close_paren, sub_exp ~ctx:(Exp ast) nested_exp)
    | _ ->
        let close_paren =
          fmt_if_k parens_branch
            ( match c.fmt_opts.indicate_multiline_delimiters.v with
            | `Space -> fmt "@ )"
            | `No -> fmt "@,)"
            | `Closing_on_separate_line -> fmt "@;<1000 -2>)" )
        in
        (fmt_if parens_branch " (", close_paren, xast)
  in
  match c.fmt_opts.break_cases.v with
  | `Fit ->
      { leading_space= fmt_if (not first) "@ "
      ; bar= fmt_or_k first (if_newline "| ") (str "| ")
      ; box_all= hvbox indent
      ; box_pattern_arrow= hovbox 2
      ; break_before_arrow= fmt "@;<1 0>"
      ; break_after_arrow= noop
      ; open_paren_branch
      ; break_after_opening_paren= fmt "@ "
      ; expr_parens
      ; branch_expr
      ; close_paren_branch }
  | `Nested ->
      { leading_space= fmt_if (not first) "@ "
      ; bar= fmt_or_k first (if_newline "| ") (str "| ")
      ; box_all= Fn.id
      ; box_pattern_arrow= hovbox 0
      ; break_before_arrow= fmt "@;<1 2>"
      ; break_after_arrow= fmt_if (not parens_branch) "@;<0 3>"
      ; open_paren_branch
      ; break_after_opening_paren= fmt_or (indent > 2) "@;<1 4>" "@;<1 2>"
      ; expr_parens
      ; branch_expr
      ; close_paren_branch }
  | `Fit_or_vertical ->
      { leading_space= break_unless_newline 1000 0
      ; bar= str "| "
      ; box_all= hovbox indent
      ; box_pattern_arrow= hovbox 0
      ; break_before_arrow= fmt "@;<1 2>"
      ; break_after_arrow= fmt_if (not parens_branch) "@;<0 3>"
      ; open_paren_branch
      ; break_after_opening_paren= fmt "@ "
      ; expr_parens
      ; branch_expr
      ; close_paren_branch }
  | `Toplevel | `All ->
      { leading_space= break_unless_newline 1000 0
      ; bar= str "| "
      ; box_all= hvbox indent
      ; box_pattern_arrow= hovbox 0
      ; break_before_arrow= fmt "@;<1 2>"
      ; break_after_arrow= fmt_if (not parens_branch) "@;<0 3>"
      ; open_paren_branch
      ; break_after_opening_paren= fmt "@ "
      ; expr_parens
      ; branch_expr
      ; close_paren_branch }
  | `Vertical ->
      { leading_space= break_unless_newline 1000 0
      ; bar= str "| "
      ; box_all= hvbox indent
      ; box_pattern_arrow= hovbox 0
      ; break_before_arrow= fmt "@;<1 2>"
      ; break_after_arrow= fmt_if (not parens_branch) "@;<0 3>"
      ; open_paren_branch
      ; break_after_opening_paren= break 1000 0
      ; expr_parens
      ; branch_expr
      ; close_paren_branch }

let wrap_collec c ~space_around opn cls =
  if space_around then wrap_k (str opn $ char ' ') (break 1 0 $ str cls)
  else wrap_fits_breaks c opn cls

let wrap_record (c : Conf.t) ~unboxed =
  let left_brace = if unboxed then "#{" else "{" in
  wrap_collec c ~space_around:c.fmt_opts.space_around_records.v left_brace
    "}"

let wrap_tuple (c : Conf.t) ~unboxed ~parens ~no_parens_if_break =
  if unboxed then wrap_fits_breaks c "#(" ")"
  else if parens then wrap_fits_breaks c "(" ")"
  else if no_parens_if_break then Fn.id
  else wrap_k (fits_breaks "" "( ") (fits_breaks "" ~hint:(1, 0) ")")

type record_type =
  { docked_before: Fmt.t
  ; break_before: Fmt.t
  ; box_record: Fmt.t -> Fmt.t
  ; box_spaced: bool
  ; sep_before: Fmt.t
  ; sep_after: Fmt.t
  ; break_after: Fmt.t
  ; docked_after: Fmt.t }

let get_record_type (c : Conf.t) ~unboxed =
  let sparse_type_decl = Poly.(c.fmt_opts.type_decl.v = `Sparse) in
  let space = if c.fmt_opts.space_around_records.v then 1 else 0 in
  let dock = c.fmt_opts.dock_collection_brackets.v in
  let break_before, sep_before, sep_after =
    match c.fmt_opts.break_separators.v with
    | `Before ->
        ( fmt_or_k dock (break space 2) (fmt "@ ")
        , fmt_or sparse_type_decl "@;<1000 0>; " "@,; "
        , noop )
    | `After ->
        ( fmt_or_k dock (break space 0) (fmt "@ ")
        , noop
        , fmt_or_k dock
            (fmt_or sparse_type_decl "@;<1000 0>" "@ ")
            (fmt_or sparse_type_decl "@;<1000 2>" "@;<1 2>") )
  in
  let box_margin = if unboxed then 1 else 0 in
  { docked_before= fmt_if dock (if unboxed then " #{" else " {")
  ; break_before
  ; box_record=
      (fun k ->
        if dock then k else hvbox box_margin (wrap_record c ~unboxed k) )
  ; box_spaced= c.fmt_opts.space_around_records.v
  ; sep_before
  ; sep_after
  ; break_after= fmt_if_k dock (break space (-2))
  ; docked_after= fmt_if dock "}" }

type elements_collection =
  { box: Fmt.t -> Fmt.t
  ; sep_before: Fmt.t
  ; sep_after_non_final: Fmt.t
  ; sep_after_final: Fmt.t }

type elements_collection_record_expr = {break_after_with: Fmt.t}

type elements_collection_record_pat = {wildcard: Fmt.t}

let get_record_expr (c : Conf.t) ~unboxed =
  let space = if c.fmt_opts.space_around_records.v then 1 else 0 in
  let dock = c.fmt_opts.dock_collection_brackets.v in
  let box k =
    let margin = if unboxed then 1 else 0 in
    if dock then
      hvbox margin
        (wrap
           (if unboxed then "#{" else "{")
           "}"
           (break space 2 $ k $ break space 0) )
    else hvbox margin (wrap_record c ~unboxed k)
  in
  ( ( match c.fmt_opts.break_separators.v with
    | `Before ->
        { box
        ; sep_before= fmt "@,; "
        ; sep_after_non_final= noop
        ; sep_after_final= noop }
    | `After ->
        { box
        ; sep_before= noop
        ; sep_after_non_final= fmt ";@;<1 2>"
        ; sep_after_final= fmt_if_k dock (fits_breaks ~level:0 "" ";") } )
  , {break_after_with= break 1 2} )

let box_collec (c : Conf.t) =
  match c.fmt_opts.break_collection_expressions.v with
  | `Wrap -> hovbox
  | `Fit_or_vertical -> hvbox

let collection_expr (c : Conf.t) ~space_around opn cls =
  let space = if space_around then 1 else 0 in
  let dock = c.fmt_opts.dock_collection_brackets.v in
  let offset = if dock then -2 else String.length opn - 1 in
  match c.fmt_opts.break_separators.v with
  | `Before ->
      { box=
          (fun k ->
            if dock then
              hvbox 0
                (wrap_k (str opn) (str cls)
                   ( break space (String.length opn + 1)
                   $ box_collec c 0 k $ break space 0 ) )
            else box_collec c 0 (wrap_collec c ~space_around opn cls k) )
      ; sep_before= break 0 offset $ str "; "
      ; sep_after_non_final= noop
      ; sep_after_final= noop }
  | `After ->
      { box=
          (fun k ->
            if dock then
              hvbox 0
                (wrap_k (str opn) (str cls)
                   (break space 2 $ box_collec c 0 k $ break space 0) )
            else box_collec c 0 (wrap_collec c ~space_around opn cls k) )
      ; sep_before= noop
      ; sep_after_non_final=
          fmt_or_k dock (fmt ";@;<1 0>")
            (char ';' $ break 1 (String.length opn + 1))
      ; sep_after_final= fmt_if_k dock (fits_breaks ~level:1 "" ";") }

let get_list_expr (c : Conf.t) =
  collection_expr c ~space_around:c.fmt_opts.space_around_lists.v "[" "]"

let get_array_expr (c : Conf.t) =
  collection_expr c ~space_around:c.fmt_opts.space_around_arrays.v "[|" "|]"

let get_iarray_expr (c : Conf.t) =
  collection_expr c ~space_around:c.fmt_opts.space_around_arrays.v "[:" ":]"

(* Modeled after [collection_expr] in [`After] mode *)
let wrap_comprehension (c : Conf.t) ~space_around ~punctuation comp =
  let opn = "[" ^ punctuation in
  let cls = punctuation ^ "]" in
  let space = if space_around then 1 else 0 in
  if c.fmt_opts.dock_collection_brackets.v then
    hvbox 0
      (wrap_k (str opn) (str cls)
         (break space 2 $ hvbox 0 comp $ break space 0) )
  else hvbox 0 (wrap_collec c ~space_around opn cls comp)

let box_pattern_docked (c : Conf.t) ~ctx ~space_around opn cls k =
  let space = if space_around then 1 else 0 in
  let indent_opn, indent_cls =
    match (ctx, c.fmt_opts.break_separators.v) with
    | Ast.Exp {pexp_desc= Pexp_match _ | Pexp_try _; _}, `Before ->
        (String.length opn - 3, 1 - String.length opn)
    | Ast.Exp {pexp_desc= Pexp_match _ | Pexp_try _; _}, `After -> (-3, 1)
    | Ast.Exp {pexp_desc= Pexp_let _; _}, _ -> (-4, 0)
    | _ -> (0, 0)
  in
  hvbox indent_opn
    (wrap_k (str opn) (str cls) (break space 2 $ k $ break space indent_cls))

let get_record_pat (c : Conf.t) ~ctx ~unboxed =
  let params, _ = get_record_expr c ~unboxed in
  let box =
    if c.fmt_opts.dock_collection_brackets.v then
      let left_brace = if unboxed then "#{" else "{" in
      box_pattern_docked c ~ctx
        ~space_around:c.fmt_opts.space_around_records.v left_brace "}"
    else params.box
  in
  ( {params with box}
  , {wildcard= params.sep_before $ str "_" $ params.sep_after_final} )

let collection_pat (c : Conf.t) ~ctx ~space_around opn cls =
  let params = collection_expr c ~space_around opn cls in
  let box =
    if c.fmt_opts.dock_collection_brackets.v then
      box_collec c 0 >> box_pattern_docked c ~ctx ~space_around opn cls
    else params.box
  in
  {params with box}

let get_list_pat (c : Conf.t) ~ctx =
  collection_pat c ~ctx ~space_around:c.fmt_opts.space_around_lists.v "[" "]"

let get_array_pat (c : Conf.t) ~ctx =
  collection_pat c ~ctx ~space_around:c.fmt_opts.space_around_arrays.v "[|"
    "|]"

let get_iarray_pat (c : Conf.t) ~ctx =
  collection_pat c ~ctx ~space_around:c.fmt_opts.space_around_arrays.v "[:"
    ":]"

type if_then_else =
  { box_branch: Fmt.t -> Fmt.t
  ; cond: Fmt.t
  ; box_keyword_and_expr: Fmt.t -> Fmt.t
  ; branch_pro: Fmt.t
  ; wrap_parens: Fmt.t -> Fmt.t
  ; box_expr: bool option
  ; expr_pro: Fmt.t option
  ; expr_eol: Fmt.t option
  ; branch_expr: expression Ast.xt
  ; break_end_branch: Fmt.t
  ; space_between_branches: Fmt.t }

let get_if_then_else (c : Conf.t) ~first ~last ~parens_bch ~parens_prev_bch
    ~xcond ~xbch ~expr_loc ~fmt_extension_suffix ~fmt_attributes ~fmt_cond =
  let imd = c.fmt_opts.indicate_multiline_delimiters.v in
  let beginend, branch_expr =
    let ast = xbch.Ast.ast in
    match ast with
    | {pexp_desc= Pexp_beginend nested_exp; pexp_attributes= []; _} ->
        (true, sub_exp ~ctx:(Exp ast) nested_exp)
    | _ -> (false, xbch)
  in
  let wrap_parens ~wrap_breaks k =
    if beginend then wrap "begin" "end" (wrap_breaks k)
    else if parens_bch then wrap "(" ")" (wrap_breaks k)
    else k
  in
  let get_parens_breaks ~opn_hint_indent ~cls_hint:(ch_sp, ch_sl) =
    let brk hint = fits_breaks "" ~hint "" in
    let oh_other = ((if beginend then 1 else 0), opn_hint_indent) in
    if beginend then
      let _, offset = ch_sl in
      wrap_k (brk oh_other) (break 1000 offset)
    else
      match imd with
      | `Space -> wrap_k (brk (1, opn_hint_indent)) (brk ch_sp)
      | `No -> wrap_k (brk oh_other) noop
      | `Closing_on_separate_line -> wrap_k (brk oh_other) (brk ch_sl)
  in
  let cond () =
    match xcond with
    | Some xcnd ->
        hvbox 0
          ( hvbox 2
              ( fmt_if (not first) "else "
              $ str "if"
              $ fmt_if_k first (fmt_opt fmt_extension_suffix)
              $ fmt_attributes $ fmt "@ " $ fmt_cond xcnd )
          $ fmt "@ then" )
    | None -> str "else"
  in
  let branch_pro = fmt_or (beginend || parens_bch) " " "@;<1 2>" in
  match c.fmt_opts.if_then_else.v with
  | `Compact ->
      { box_branch= hovbox 2
      ; cond= cond ()
      ; box_keyword_and_expr= Fn.id
      ; branch_pro= fmt_or (beginend || parens_bch) " " "@ "
      ; wrap_parens=
          wrap_parens
            ~wrap_breaks:
              (get_parens_breaks ~opn_hint_indent:0
                 ~cls_hint:((1, 0), (1000, -2)) )
      ; box_expr= Some false
      ; expr_pro= None
      ; expr_eol= None
      ; branch_expr
      ; break_end_branch= noop
      ; space_between_branches= fmt "@ " }
  | `K_R ->
      { box_branch= Fn.id
      ; cond= cond ()
      ; box_keyword_and_expr= Fn.id
      ; branch_pro
      ; wrap_parens= wrap_parens ~wrap_breaks:(wrap_k (break 1000 2) noop)
      ; box_expr= Some false
      ; expr_pro= None
      ; expr_eol= Some (fmt "@;<1 2>")
      ; branch_expr
      ; break_end_branch=
          fmt_if_k (parens_bch || beginend || not last) (break 1000 0)
      ; space_between_branches= fmt_if (beginend || parens_bch) " " }
  | `Fit_or_vertical ->
      { box_branch=
          hovbox
            ( match imd with
            | `Closing_on_separate_line when parens_prev_bch -> -2
            | _ -> 0 )
      ; cond= cond ()
      ; box_keyword_and_expr= Fn.id
      ; branch_pro
      ; wrap_parens=
          wrap_parens
            ~wrap_breaks:
              (get_parens_breaks ~opn_hint_indent:2
                 ~cls_hint:((1, 0), (1000, 0)) )
      ; box_expr= Some false
      ; expr_pro=
          Some
            (fmt_if_k
               (not (Location.is_single_line expr_loc c.fmt_opts.margin.v))
               (break_unless_newline 1000 2) )
      ; expr_eol= Some (fmt "@;<1 2>")
      ; branch_expr
      ; break_end_branch= noop
      ; space_between_branches=
          fmt
            ( match imd with
            | `Closing_on_separate_line when beginend || parens_bch -> " "
            | _ -> "@ " ) }
  | `Vertical ->
      { box_branch= Fn.id
      ; cond= cond ()
      ; box_keyword_and_expr= Fn.id
      ; branch_pro
      ; wrap_parens=
          wrap_parens
            ~wrap_breaks:
              (get_parens_breaks ~opn_hint_indent:2
                 ~cls_hint:((1, 0), (1000, 0)) )
      ; box_expr= None
      ; expr_pro= Some (break_unless_newline 1000 2)
      ; expr_eol= None
      ; branch_expr
      ; break_end_branch= noop
      ; space_between_branches=
          fmt
            ( match imd with
            | `Closing_on_separate_line when parens_bch -> " "
            | _ -> "@ " ) }
  | `Keyword_first ->
      { box_branch= Fn.id
      ; cond=
          opt xcond (fun xcnd ->
              hvbox 2
                ( fmt_or_k first
                    (str "if" $ fmt_opt fmt_extension_suffix)
                    (str "else if")
                $ fmt_attributes
                $ fmt_or (Option.is_some fmt_extension_suffix) "@ " " "
                $ fmt_cond xcnd )
              $ fmt "@ " )
      ; box_keyword_and_expr=
          (fun k -> hvbox 2 (fmt_or (Option.is_some xcond) "then" "else" $ k))
      ; branch_pro= fmt_or (beginend || parens_bch) " " "@ "
      ; wrap_parens=
          wrap_parens
            ~wrap_breaks:
              (get_parens_breaks ~opn_hint_indent:0
                 ~cls_hint:((1, 0), (1000, -2)) )
      ; box_expr= Some false
      ; expr_pro= None
      ; expr_eol= None
      ; branch_expr
      ; break_end_branch= noop
      ; space_between_branches= fmt "@ " }

let match_indent ?(default = 0) (c : Conf.t) ~parens ~(ctx : Ast.t) =
  match (c.fmt_opts.match_indent_nested.v, ctx) with
  | `Always, _ | _, (Top | Sig _ | Str _) -> c.fmt_opts.match_indent.v
  | _, Exp {pexp_desc= Pexp_infix _; _}
    when c.fmt_opts.ocp_indent_compat.v && parens ->
      2 (* Match is docked *)
  | _ -> default

let comma_sep (c : Conf.t) : Fmt.s =
  match c.fmt_opts.break_separators.v with
  | `Before -> "@,, "
  | `After -> ",@;<1 2>"

module Align = struct
  (** Whether [exp] occurs in [args] as a labelled argument. *)
  let is_labelled_arg args exp =
    List.exists
      ~f:(function
        | Nolabel, _ -> false
        | Labelled _, x | Optional _, x -> phys_equal x exp )
      args

  let general (c : Conf.t) t =
    hvbox_if (not c.fmt_opts.align_symbol_open_paren.v) 0 t

  let infix_op = general

  let match_ (c : Conf.t) ~xexp:{ast; ctx} t =
    (* Matches on the RHS of an infix are docked in ocp-indent-compat. *)
    let docked =
      match ctx with
      | Exp {pexp_desc= Pexp_infix (_, _, rhs); _} when phys_equal rhs ast ->
          c.fmt_opts.ocp_indent_compat.v
      | _ -> false
    in
    let align = (not c.fmt_opts.align_symbol_open_paren.v) && not docked in
    hvbox_if align 0 t

  let function_ (c : Conf.t) ~parens ~(ctx0 : Ast.t) ~self t =
    let align =
      match ctx0 with
      | Exp {pexp_desc= Pexp_infix (_, _, {pexp_desc= Pexp_function _; _}); _}
        ->
          false
      | Exp {pexp_desc= Pexp_apply (_, args); _}
        when is_labelled_arg args self ->
          false
      | _ -> parens && not c.fmt_opts.align_symbol_open_paren.v
    in
    hvbox_if align 0 t
end

module Indent = struct
  let function_ ?(default = 0) (c : Conf.t) ~parens xexp =
    match c.fmt_opts.function_indent_nested.v with
    | `Always -> c.fmt_opts.function_indent.v
    | _ when ocp c && parens && not (is_labelled_arg' xexp) -> default + 1
    | _ -> default

  let fun_ ?eol (c : Conf.t) =
    match c.fmt_opts.function_indent_nested.v with
    | `Always -> c.fmt_opts.function_indent.v
    | _ ->
        if Option.is_none eol then 2
        else if c.fmt_opts.let_binding_deindent_fun.v then 1
        else 0

  let fun_type_annot c = if ocp c then 2 else 4

  let fun_args c = if ocp c then 6 else 4

  let docked_function (c : Conf.t) ~parens xexp =
    if ocp c then if parens then 3 else 2
    else
      let default = if c.fmt_opts.wrap_fun_args.v then 2 else 4 in
      function_ ~default c ~parens:false xexp

  let docked_function_after_fun (c : Conf.t) ~parens ~lbl =
    if ocp c then if parens && Poly.equal lbl Nolabel then 3 else 2 else 0

  let fun_args_group (c : Conf.t) ~lbl exp =
    if not (ocp c) then 2
    else
      match exp.pexp_desc with
      | Pexp_function _ -> 2
      | _ -> ( match lbl with Nolabel -> 3 | _ -> 2 )

  let docked_fun (c : Conf.t) ~source ~loc ~lbl =
    if not (ocp c) then 2
    else
      let loc, if_breaks =
        match lbl with
        | Nolabel -> (loc, 3)
        | Optional x | Labelled x -> (x.loc, 2)
      in
      if Source.begins_line ~ignore_spaces:true source loc then if_breaks
      else 0

  let record_docstring (c : Conf.t) =
    if ocp c then
      match c.fmt_opts.break_separators.v with `Before -> -2 | `After -> 0
    else 4

  let constructor_docstring c = if ocp c then 0 else 4

  let exp_constraint c = if ocp c then 1 else 2

  let assignment_operator_bol c = if ocp c then 0 else 2

  let mod_constraint c ~lhs =
    if ocp c then match lhs.pmod_desc with Pmod_structure _ -> 0 | _ -> 2
    else 2

  let mod_unpack_annot c = if ocp c then 0 else 2

  let mty_with c = if ocp c then 0 else 2

  let type_constr c = if ocp c then 2 else 0
end
