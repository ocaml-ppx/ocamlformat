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

module Format = Format_
module Location = Migrate_ast.Location
open Extended_ast
open Fmt

let parens_or_begin_end (c : Conf.t) source ~loc =
  match c.exp_grouping with
  | `Parens -> `Parens
  | `Preserve -> (
    match
      Source.find_token_after source
        ~filter:(fun _ -> true)
        loc.Location.loc_start
    with
    | Some (BEGIN, _) -> `Begin_end
    | None | Some _ -> `Parens )

let parens_if parens (c : Conf.t) k =
  if not parens then k
  else
    match c.Conf.indicate_multiline_delimiters with
    | `Space ->
        Fmt.fits_breaks "(" "(" $ k $ Fmt.fits_breaks ")" ~hint:(1, 0) ")"
    | `Closing_on_separate_line ->
        Fmt.fits_breaks "(" "(" $ k $ Fmt.fits_breaks ")" ~hint:(1000, 0) ")"
    | `No -> wrap "(" ")" k

let parens c k = parens_if true c k

module Exp = struct
  module Infix_op_arg = struct
    let wrap (c : Conf.t) ?(parens_nested = false) ~ext ~parens ~loc source k
        =
      match parens_or_begin_end c source ~loc with
      | `Parens when parens || parens_nested ->
          let opn, hint, cls =
            if parens || Poly.(c.infix_precedence = `Parens) then
              match c.indicate_multiline_delimiters with
              | `Space -> ("( ", Some (1, 0), ")")
              | `No -> ("(", Some (0, 0), ")")
              | `Closing_on_separate_line -> ("(", Some (1000, 0), ")")
            else ("", None, "")
          in
          wrap_if_k (parens || parens_nested) (Fmt.fits_breaks "(" opn)
            (Fmt.fits_breaks ")" ?hint cls)
            k
      | `Parens -> k
      | `Begin_end ->
          vbox 2
            (wrap_k
               (str "begin" $ ext)
               (str "end")
               (wrap_k (break 1 0) (break 1000 ~-2) k) )
  end

  let wrap (c : Conf.t) ?(fits_breaks = true) ?(offset_closing_paren = 0)
      ~parens ~loc source k =
    match parens_or_begin_end c source ~loc with
    | (`Parens | `Begin_end) when not parens -> k
    | `Parens when fits_breaks -> wrap_fits_breaks ~space:false c "(" ")" k
    | `Parens -> (
      match c.Conf.indicate_multiline_delimiters with
      | `Space ->
          Fmt.fits_breaks "(" "(" $ k $ Fmt.fits_breaks ")" ~hint:(1, 0) ")"
      | `Closing_on_separate_line ->
          Fmt.fits_breaks "(" "(" $ k
          $ Fmt.fits_breaks ")" ~hint:(1000, offset_closing_paren) ")"
      | `No -> wrap "(" ")" k )
    | `Begin_end ->
        vbox 2 (wrap "begin" "end" (wrap_k (break 1 0) (break 1000 ~-2) k))
end

let get_or_pattern_sep ?(cmts_before = false) ?(space = false) (c : Conf.t)
    ~ctx =
  let nspaces = if cmts_before then 1000 else 1 in
  match ctx with
  | Ast.Exp { pexp_desc= Pexp_function _ | Pexp_match _ | Pexp_try _; _ }
    -> (
    match c.break_cases with
    | `Nested -> break nspaces 0 $ str "| "
    | _ -> (
        let nspaces =
          match c.break_cases with `All -> 1000 | _ -> nspaces
        in
        match c.indicate_nested_or_patterns with
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
  ; close_paren_branch: Fmt.t
  }

let get_cases (c : Conf.t) ~first ~indent ~parens_branch source ~loc =
  let grouping = parens_or_begin_end c source ~loc in
  let open_paren_branch =
    fmt_if parens_branch
      (match grouping with `Parens -> " (" | `Begin_end -> "@;<1 0>begin")
  in
  let close_paren_branch =
    fmt_if_k parens_branch
      ( match grouping with
      | `Parens -> (
        match c.indicate_multiline_delimiters with
        | `Space -> fmt "@ )"
        | `No -> fmt "@,)"
        | `Closing_on_separate_line -> fmt "@;<1000 -2>)" )
      | `Begin_end ->
          let offset = match c.break_cases with `Nested -> 0 | _ -> -2 in
          fits_breaks " end" ~level:1 ~hint:(1000, offset) "end" )
  in
  match c.break_cases with
  | `Fit ->
      { leading_space= fmt_if (not first) "@ "
      ; bar= fmt_or_k first (if_newline "| ") (str "| ")
      ; box_all= hvbox indent
      ; box_pattern_arrow= hovbox 2
      ; break_before_arrow= fmt "@;<1 0>"
      ; break_after_arrow= noop
      ; open_paren_branch
      ; break_after_opening_paren= fmt "@ "
      ; close_paren_branch
      }
  | `Nested ->
      { leading_space= fmt_if (not first) "@ "
      ; bar= fmt_or_k first (if_newline "| ") (str "| ")
      ; box_all= Fn.id
      ; box_pattern_arrow= hovbox 0
      ; break_before_arrow= fmt "@;<1 2>"
      ; break_after_arrow= fmt_if (not parens_branch) "@;<0 3>"
      ; open_paren_branch
      ; break_after_opening_paren= fmt_or (indent > 2) "@;<1 4>" "@;<1 2>"
      ; close_paren_branch
      }
  | `Fit_or_vertical ->
      { leading_space= break_unless_newline 1000 0
      ; bar= str "| "
      ; box_all= hovbox indent
      ; box_pattern_arrow= hovbox 0
      ; break_before_arrow= fmt "@;<1 2>"
      ; break_after_arrow= fmt_if (not parens_branch) "@;<0 3>"
      ; open_paren_branch
      ; break_after_opening_paren= fmt "@ "
      ; close_paren_branch
      }
  | `Toplevel | `All ->
      { leading_space= break_unless_newline 1000 0
      ; bar= str "| "
      ; box_all= hvbox indent
      ; box_pattern_arrow= hovbox 0
      ; break_before_arrow= fmt "@;<1 2>"
      ; break_after_arrow= fmt_if (not parens_branch) "@;<0 3>"
      ; open_paren_branch
      ; break_after_opening_paren= fmt "@ "
      ; close_paren_branch
      }

let wrap_collec opn cls = wrap_k (str opn $ char ' ') (break 1 0 $ str cls)
let wrap_record = wrap_collec "{" "}"

let wrap_tuple (c : Conf.t) ~parens ~no_parens_if_break =
  if parens then wrap_fits_breaks c "(" ")"
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
  ; docked_after: Fmt.t
  }

let get_record_type (c : Conf.t) =
  let dock = c.dock_collection_brackets in
  let break_before, sep_before, sep_after =
    match c.break_separators with
    | `Before ->
        (fmt_or_k dock (break 1 2) (fmt "@ "), fmt "@;<1000 0>; ", noop)
    | `After ->
        ( fmt_or_k dock (break 1 0) (fmt "@ ")
        , noop
        , fmt_or dock "@;<1000 0>" "@;<1000 2>" )
  in
  { docked_before= fmt_if dock " {"
  ; break_before
  ; box_record= (fun k -> if dock then k else hvbox 0 (wrap_record k))
  ; box_spaced= true
  ; sep_before
  ; sep_after
  ; break_after= fmt_if_k dock (break 1 (-2))
  ; docked_after= fmt_if dock "}"
  }

type elements_collection =
  { box: Fmt.t -> Fmt.t
  ; sep_before: Fmt.t
  ; sep_after_non_final: Fmt.t
  ; sep_after_final: Fmt.t
  }

type elements_collection_record_expr = { break_after_with: Fmt.t }
type elements_collection_record_pat = { wildcard: Fmt.t }

let get_record_expr (c : Conf.t) =
  let dock = c.dock_collection_brackets in
  let box k =
    if dock then hvbox 0 (wrap "{" "}" (break 1 2 $ k $ break 1 0))
    else hvbox 0 (wrap_record k)
  in
  ( ( match c.break_separators with
    | `Before ->
        { box
        ; sep_before= fmt "@,; "
        ; sep_after_non_final= noop
        ; sep_after_final= noop
        }
    | `After ->
        { box
        ; sep_before= noop
        ; sep_after_non_final= fmt ";@;<1 2>"
        ; sep_after_final= fmt_if_k dock (fits_breaks ~level:0 "" ";")
        } )
  , { break_after_with= break 1 2 } )

let box_collec (c : Conf.t) =
  match c.break_collection_expressions with
  | `Wrap -> hovbox
  | `Fit_or_vertical -> hvbox

let collection_expr (c : Conf.t) opn cls =
  let dock = c.dock_collection_brackets in
  let offset = if dock then -2 else String.length opn - 1 in
  match c.break_separators with
  | `Before ->
      { box=
          (fun k ->
            if dock then
              hvbox 0
                (wrap_k (str opn) (str cls)
                   ( break 1 (String.length opn + 1)
                   $ box_collec c 0 k $ break 1 0 ) )
            else box_collec c 0 (wrap_collec opn cls k) )
      ; sep_before= break 0 offset $ str "; "
      ; sep_after_non_final= noop
      ; sep_after_final= noop
      }
  | `After ->
      { box=
          (fun k ->
            if dock then
              hvbox 0
                (wrap_k (str opn) (str cls)
                   (break 1 2 $ box_collec c 0 k $ break 1 0) )
            else box_collec c 0 (wrap_collec opn cls k) )
      ; sep_before= noop
      ; sep_after_non_final=
          fmt_or_k dock (fmt ";@;<1 0>")
            (char ';' $ break 1 (String.length opn + 1))
      ; sep_after_final= fmt_if_k dock (fits_breaks ~level:1 "" ";")
      }

let get_list_expr c = collection_expr c "[" "]"
let get_array_expr c = collection_expr c "[|" "|]"

let box_pattern_docked (c : Conf.t) ~ctx opn cls k =
  let indent_opn, indent_cls =
    match (ctx, c.break_separators) with
    | Ast.Exp { pexp_desc= Pexp_match _ | Pexp_try _; _ }, `Before ->
        (String.length opn - 3, 1 - String.length opn)
    | Ast.Exp { pexp_desc= Pexp_match _ | Pexp_try _; _ }, `After -> (-3, 1)
    | Ast.Exp { pexp_desc= Pexp_let _; _ }, _ -> (-4, 0)
    | _ -> (0, 0)
  in
  hvbox indent_opn
    (wrap_k (str opn) (str cls) (break 1 2 $ k $ break 1 indent_cls))

let get_record_pat (c : Conf.t) ~ctx =
  let params, _ = get_record_expr c in
  let box =
    if c.dock_collection_brackets then box_pattern_docked c ~ctx "{" "}"
    else params.box
  in
  ( { params with box }
  , { wildcard= params.sep_before $ str "_" $ params.sep_after_final } )

let collection_pat (c : Conf.t) ~ctx opn cls =
  let params = collection_expr c opn cls in
  let box =
    if c.dock_collection_brackets then
      box_collec c 0 >> box_pattern_docked c ~ctx opn cls
    else params.box
  in
  { params with box }

let get_list_pat c ~ctx = collection_pat c ~ctx "[" "]"
let get_array_pat c ~ctx = collection_pat c ~ctx "[|" "|]"

type if_then_else =
  { box_branch: Fmt.t -> Fmt.t
  ; cond: Fmt.t
  ; box_keyword_and_expr: Fmt.t -> Fmt.t
  ; branch_pro: Fmt.t
  ; wrap_parens: Fmt.t -> Fmt.t
  ; expr_pro: Fmt.t option
  ; expr_eol: Fmt.t option
  ; break_end_branch: Fmt.t
  ; space_between_branches: Fmt.t
  }

let get_if_then_else (c : Conf.t) ~first ~last ~parens ~parens_bch
    ~parens_prev_bch ~xcond ~expr_loc ~bch_loc ~fmt_extension_suffix
    ~fmt_attributes ~fmt_cond source =
  let imd = c.indicate_multiline_delimiters in
  let exp_grouping = parens_or_begin_end c source ~loc:expr_loc in
  let exp_grouping_bch = parens_or_begin_end c source ~loc:bch_loc in
  let wrap_parens ~wrap_breaks k =
    match exp_grouping_bch with
    | (`Parens | `Begin_end) when not parens_bch -> k
    | `Parens -> wrap "(" ")" (wrap_breaks k)
    | `Begin_end -> wrap "begin" "end" (wrap_breaks k)
  in
  let get_parens_breaks ~opn_hint:(oh_space, oh_other)
      ~cls_hint:(ch_sp, ch_sl) =
    let brk hint = fits_breaks "" ~hint "" in
    match (exp_grouping_bch, imd) with
    | `Parens, `Space -> wrap_k (brk oh_space) (brk ch_sp)
    | `Parens, `No -> wrap_k (brk oh_other) noop
    | `Parens, `Closing_on_separate_line -> wrap_k (brk oh_other) (brk ch_sl)
    | `Begin_end, _ ->
        let _, offset = ch_sl in
        wrap_k (brk oh_other) (break 1000 offset)
  in
  let cond () =
    match xcond with
    | Some xcnd ->
        hvbox
          ( match (parens, imd) with
          | false, _ -> 0
          | true, `No -> -1
          | true, (`Space | `Closing_on_separate_line) -> -2 )
          ( hvbox
              (if parens then 0 else 2)
              ( fmt_if (not first) "else "
              $ str "if"
              $ fmt_if_k first (fmt_opt fmt_extension_suffix)
              $ fmt_attributes $ fmt "@ " $ fmt_cond xcnd )
          $ fmt "@ then" )
    | None -> str "else"
  in
  let branch_pro = fmt_or parens_bch " " "@;<1 2>" in
  match c.if_then_else with
  | `Compact ->
      let box_branch =
        if first && parens && Poly.(exp_grouping = `Parens) then hovbox 0
        else hovbox 2
      in
      { box_branch
      ; cond= cond ()
      ; box_keyword_and_expr= Fn.id
      ; branch_pro= fmt_or parens_bch " " "@ "
      ; wrap_parens=
          wrap_parens
            ~wrap_breaks:
              (get_parens_breaks
                 ~opn_hint:((1, 0), (0, 0))
                 ~cls_hint:((1, 0), (1000, -2)) )
      ; expr_pro= None
      ; expr_eol= None
      ; break_end_branch= noop
      ; space_between_branches= fmt "@ "
      }
  | `K_R ->
      { box_branch= Fn.id
      ; cond= cond ()
      ; box_keyword_and_expr= Fn.id
      ; branch_pro
      ; wrap_parens= wrap_parens ~wrap_breaks:(wrap_k (break 1000 2) noop)
      ; expr_pro= None
      ; expr_eol= Some (fmt "@;<1 2>")
      ; break_end_branch= fmt_if_k (parens_bch || not last) (break 1000 0)
      ; space_between_branches= fmt_if parens_bch " "
      }
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
              (get_parens_breaks
                 ~opn_hint:((1, 2), (0, 2))
                 ~cls_hint:((1, 0), (1000, 0)) )
      ; expr_pro=
          Some
            (fmt_if_k
               (not (Location.is_single_line expr_loc c.margin))
               (break_unless_newline 1000 2) )
      ; expr_eol= Some (fmt "@;<1 2>")
      ; break_end_branch= noop
      ; space_between_branches=
          fmt
            ( match imd with
            | `Closing_on_separate_line when parens_bch -> " "
            | _ -> "@ " )
      }
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
      ; branch_pro= fmt_or parens_bch " " "@ "
      ; wrap_parens=
          wrap_parens
            ~wrap_breaks:
              (get_parens_breaks
                 ~opn_hint:((1, 0), (0, 0))
                 ~cls_hint:((1, 0), (1000, -2)) )
      ; expr_pro= None
      ; expr_eol= None
      ; break_end_branch= noop
      ; space_between_branches= fmt "@ "
      }

let function_indent ?(default = 0) = function
  | Ast.Top | Sig _ | Str _ -> 2
  | _ -> default

let comma_sep (c : Conf.t) : Fmt.s =
  match c.break_separators with `Before -> "@,, " | `After -> ",@;<1 2>"

let semi_sep (c : Conf.t) : Fmt.s =
  match c.break_separators with `Before -> "@,; " | `After -> ";@;<1 2>"
