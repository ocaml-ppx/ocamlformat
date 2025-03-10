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

let ctx_is_infix = function
  | Exp {pexp_desc= Pexp_infix ({txt= ":="; _}, _, _); _} -> false
  | Exp {pexp_desc= Pexp_infix _; _} -> true
  | _ -> false

let ctx_is_rhs_of_infix ~ctx0 ~ctx =
  match (ctx0, ctx) with
  | Exp {pexp_desc= Pexp_infix ({txt= ":="; _}, _, _); _}, _ -> false
  | Exp {pexp_desc= Pexp_infix (_, _, rhs); _}, Exp ctx
    when phys_equal rhs ctx ->
      true
  | _ -> false

(** Return [None] if [ctx0] is not an application or [ctx] is not one of its
    argument. *)
let ctx_is_apply_and_exp_is_arg ~ctx ctx0 =
  match (ctx, ctx0) with
  | Exp exp, Exp {pexp_desc= Pexp_apply (_, args); _} ->
      let last_lbl, last_arg = List.last_exn args in
      if phys_equal last_arg exp then Some (last_lbl, exp, true)
      else
        List.find_map
          ~f:(fun (lbl, x) ->
            if phys_equal x exp then Some (lbl, exp, false) else None )
          args
  | _ -> None

let ctx_is_apply_and_exp_is_func ~ctx ctx0 =
  match (ctx, ctx0) with
  | Exp exp, Exp {pexp_desc= Pexp_apply (func, _); _} -> phys_equal func exp
  | _ -> false

let ctx_is_apply_and_exp_is_last_arg_and_other_args_are_simple c ~ctx ctx0 =
  match (ctx, ctx0) with
  | Exp exp, Exp {pexp_desc= Pexp_apply (_, args); _} ->
      let (_lbl, last_arg), args_before =
        match List.rev args with
        | [] -> assert false
        | hd :: tl -> (hd, List.rev tl)
      in
      let args_are_simple =
        List.for_all args_before ~f:(fun (_, eI) ->
            is_simple c (fun _ -> 0) (sub_exp ~ctx:ctx0 eI) )
      in
      Poly.equal last_arg exp && args_are_simple
  | _ -> false

(** [ctx_is_let_or_fun ~ctx ctx0] checks whether [ctx0] is a let binding containing
    [ctx] or a [fun] with [ctx] on the RHS. *)
let ctx_is_let_or_fun ~ctx ctx0 =
  match (ctx0, ctx) with
  | Str {pstr_desc= Pstr_value _; _}, _ -> true
  | _, Lb {pvb_body= Pfunction_cases _; _} ->
      (* This happens when a synthetic expression is constructed while
         formatting let bindings. *)
      true
  | Lb {pvb_body= Pfunction_body body; _}, Exp exp -> phys_equal body exp
  | Bo _, _ -> true
  | Exp {pexp_desc= Pexp_let ({pvbs_bindings; _}, _, _); _}, Exp exp
    when List.exists pvbs_bindings ~f:(fun pvb ->
             match (pvb.pvb_body, exp) with
             | Pfunction_body body, _ -> phys_equal body exp
             | Pfunction_cases _, {pexp_desc= Pexp_let _; _} ->
                 (* This also happens while formatting let bindings. *)
                 true
             | _ -> false ) ->
      true
  | Exp {pexp_desc= Pexp_function (_, _, Pfunction_body rhs); _}, Exp exp ->
      phys_equal rhs exp
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
    | `No -> wrap (str "(") (str ")") k

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
        wrap_if (parens || parens_nested) (Fmt.fits_breaks "(" opn)
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
          | _, {pexp_desc= Pexp_function _; _} -> true
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
      | `No -> wrap (str "(") (str ")") k

  let break_fun_kw c ~ctx ~ctx0 ~last_arg =
    let is_labelled_arg =
      match ctx_is_apply_and_exp_is_arg ~ctx ctx0 with
      | Some ((Labelled _ | Optional _), _, _) -> true
      | _ -> false
    in
    if Conf.(c.fmt_opts.ocp_indent_compat.v) then
      if last_arg || is_labelled_arg then break 1 2 else str " "
    else if is_labelled_arg then break 1 2
    else if last_arg then break 1 0
    else str " "

  let box_fun_decl_args ~ctx ~ctx0 ?(last_arg = false) ?epi c ~parens ~kw
      ~args ~annot =
    let is_let_func =
      match ctx0 with
      | Ast.Str _ | Lb _ ->
          (* special case than aligns the arguments of [let _ = fun ...] *)
          true
      | _ -> false
    in
    let kw_in_box = (not last_arg) && ocp c in
    let name = "Params.box_fun_decl_args" in
    let box_decl, should_box_args =
      if ocp c then
        let is_labelled_arg =
          match ctx_is_apply_and_exp_is_arg ~ctx ctx0 with
          | Some ((Labelled _ | Optional _), _, _) -> true
          | _ -> false
        in
        if is_labelled_arg then (Fn.id, true)
        else (hvbox ~name (if parens then 1 else 2), false)
      else
        (* The box for the arguments after [let _ = fun] is different than
           for other [fun] expressions. *)
        let box =
          if is_let_func then if kw_in_box then hovbox ~name 4 else Fn.id
          else
            match ctx_is_apply_and_exp_is_arg ~ctx ctx0 with
            | Some (_, _, true) ->
                (* Is last arg. *) hvbox ~name (if parens then 0 else 2)
            | Some (Nolabel, _, false) ->
                (* TODO: Inconsistent formatting of fun args. *)
                hovbox ~name 0
            | Some ((Labelled _ | Optional _), _, false) -> hvbox ~name 0
            | None -> Fn.id
        in
        (box, not c.fmt_opts.wrap_fun_args.v)
    in
    let kw_out_of_box, kw_in_box =
      if kw_in_box then (noop, kw) else (kw, noop)
    in
    kw_out_of_box
    $ box_decl
        ( kw_in_box
        $ hvbox_if should_box_args 0 (args $ fmt_opt annot $ fmt_opt epi) )

  let box_fun_expr (c : Conf.t) ~source ~ctx0 ~ctx =
    let indent =
      if ctx_is_rhs_of_infix ~ctx0 ~ctx then 0
      else if Poly.equal c.fmt_opts.function_indent_nested.v `Always then
        c.fmt_opts.function_indent.v
      else if ctx_is_let_or_fun ~ctx ctx0 then
        if c.fmt_opts.let_binding_deindent_fun.v then 1 else 0
      else if ocp c then
        let begins_line loc =
          Source.begins_line ~ignore_spaces:true source loc
        in
        match ctx_is_apply_and_exp_is_arg ~ctx ctx0 with
        | Some (Nolabel, fun_exp, is_last_arg) ->
            if begins_line fun_exp.pexp_loc then if is_last_arg then 5 else 3
            else 2
        | Some ((Labelled x | Optional x), fun_exp, is_last_arg) ->
            if begins_line fun_exp.pexp_loc then
              (* The [fun] had to break after the label, nested boxes must be
                 indented less. The last argument is special as the box
                 structure is different. *)
              if is_last_arg then 4 else 2
            else if begins_line x.loc then 4
            else 2
        | None -> if ctx_is_apply_and_exp_is_func ~ctx ctx0 then 3 else 2
      else if
        ctx_is_apply_and_exp_is_last_arg_and_other_args_are_simple c ~ctx
          ctx0
      then 4
      else 2
    in
    let name = "Params.box_fun_expr" in
    let mkbox = if ctx_is_let_or_fun ~ctx ctx0 then hvbox else hovbox in
    (mkbox ~name indent, ~-indent)

  (* if the function is the last argument of an apply and no other arguments
     are "complex" (approximation). *)
  let function_attrs_sp c ~ctx0 ~ctx =
    let arg_is_simple_approx (_, exp) =
      Ast.is_simple c (fun _ -> 0) (sub_exp ~ctx:ctx0 exp)
    in
    match (ctx0, ctx) with
    | Exp {pexp_desc= Pexp_apply (_, args); _}, Exp exp -> (
      match List.rev args with
      | [] -> false
      | (_, last_arg) :: other_args ->
          phys_equal exp last_arg
          && List.for_all ~f:arg_is_simple_approx other_args )
    | _ -> false

  let break_fun_decl_args c ~ctx ~last_arg =
    match ctx with
    | _ when (not last_arg) && ocp c -> str " "
    | Ast.Str _ | Lb _ ->
        (* special case that break the arrow in [let _ = fun ... ->] *)
        str " "
    | Clf _ ->
        (* special case for methods. *)
        str " "
    | _ -> break 1 ~-2

  let single_line_function ~ctx ~ctx0 ~args =
    match ctx_is_apply_and_exp_is_arg ~ctx ctx0 with
    | Some (_, _, true) -> List.is_empty args
    | _ -> false

  let indent_function (c : Conf.t) ~ctx ~ctx0 ~parens =
    if ctx_is_rhs_of_infix ~ctx0 ~ctx then if ocp c && parens then 1 else 0
    else if Poly.equal c.fmt_opts.function_indent_nested.v `Always then
      c.fmt_opts.function_indent.v
    else
      match ctx_is_apply_and_exp_is_arg ~ctx ctx0 with
      | Some _ -> 2
      | None -> if ocp c && parens then 2 else 0

  let box_function_cases c ?indent ~ctx ~ctx0 ~parens =
    let indent =
      match indent with
      | Some i -> i
      | None -> indent_function c ~ctx ~ctx0 ~parens
    in
    match ctx0 with
    | Exp {pexp_desc= Pexp_ifthenelse _; _}
      when Stdlib.(Conf.(c.fmt_opts.if_then_else.v) = `Compact) ->
        hvbox ~name:"cases box" indent
    | _ ->
        if
          ctx_is_apply_and_exp_is_last_arg_and_other_args_are_simple c ~ctx
            ctx0
          || ctx_is_let_or_fun ~ctx ctx0
        then Fn.id
        else hvbox indent

  let box_fun_decl ~ctx0 c k =
    match ctx0 with
    | _ when ocp c -> hvbox 2 k
    | Str _ | Lb _ | Clf _ | Exp {pexp_desc= Pexp_let _; _} -> hovbox 4 k
    | _ -> hvbox 2 k
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
    let align = (not dock) && ocp c in
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
      | `Before -> (space_break, str " ")
      | `After -> (str " ", space_break)
    in
    let post = if is_sig rhs then break 1 ~-2 else post in
    pre $ str "->" $ post

  let break_let_open _conf ~rhs = break 1000 (if is_sig rhs then ~-2 else 0)
end

(* Whether [pat] appears in [ctx] as a match/function/try case. *)
let get_or_pattern_is_nested ~ctx pat =
  let check_cases = List.exists ~f:(fun c -> phys_equal c.pc_lhs pat) in
  match ctx with
  | _ when not (List.is_empty pat.ppat_attributes) -> true
  | Ast.Exp
      { pexp_desc=
          ( Pexp_function (_, _, Pfunction_cases (cases, _, _))
          | Pexp_match (_, cases)
          | Pexp_try (_, cases) )
      ; _ }
   |Lb {pvb_body= Pfunction_cases (cases, _, _); _} ->
      not (check_cases cases)
  | Exp {pexp_desc= Pexp_let (bindings, _, _); _}
   |Cl {pcl_desc= Pcl_let (bindings, _, _); _}
   |Str {pstr_desc= Pstr_value bindings; _} ->
      not
        (List.exists bindings.pvbs_bindings ~f:(function
          | {pvb_body= Pfunction_cases (cases, _, _); _} -> check_cases cases
          | _ -> false ))
  | _ -> true

let get_or_pattern_sep ?(cmts_before = false) ?(space = false) (c : Conf.t)
    ~nested =
  let nspaces = if cmts_before then 1000 else 1 in
  match c.fmt_opts.break_cases.v with
  | _ when nested -> break nspaces 0 $ str "| "
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
      | `Unsafe_no -> break nspaces 0 $ str "| " )

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
  ; expr_eol: Fmt.t option
  ; branch_expr: expression Ast.xt
  ; close_paren_branch: Fmt.t }

let get_cases (c : Conf.t) ~ctx ~first ~last ~cmts_before
    ~xbch:({ast; _} as xast) =
  let indent =
    match (c.fmt_opts.cases_matching_exp_indent.v, (ctx, ast.pexp_desc)) with
    | ( `Compact
      , ( ( Exp
              { pexp_desc=
                  Pexp_function _ | Pexp_match _ | Pexp_try _ | Pexp_let _
              ; _ }
          | Lb {pvb_body= Pfunction_cases _; _} )
        , (Pexp_match _ | Pexp_try _ | Pexp_beginend _) ) ) ->
        2
    | _, _ -> c.fmt_opts.cases_exp_indent.v
  in
  let align_nested_match =
    match (ast.pexp_desc, c.fmt_opts.nested_match.v) with
    | (Pexp_match _ | Pexp_try _), `Align -> last
    | ( Pexp_extension
          ( ext
          , PStr
              [ { pstr_loc= _
                ; pstr_desc=
                    Pstr_eval
                      ({pexp_desc= Pexp_match _ | Pexp_try _; pexp_loc; _}, _)
                } ] )
      , `Align )
      when Source.extension_using_sugar ~name:ext ~payload:pexp_loc ->
        last
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
    | {pexp_desc= Pexp_beginend nested_exp; pexp_attributes= []; _}
      when not cmts_before ->
        let close_paren =
          let offset =
            match c.fmt_opts.break_cases.v with `Nested -> 0 | _ -> -2
          in
          fits_breaks " end" ~level:1 ~hint:(1000, offset) "end"
        in
        ( break 1 0 $ str "begin"
        , close_paren
        , sub_exp ~ctx:(Exp ast) nested_exp )
    | _ ->
        let close_paren =
          fmt_if parens_branch
            ( match c.fmt_opts.indicate_multiline_delimiters.v with
            | `Space -> space_break $ str ")"
            | `No -> cut_break $ str ")"
            | `Closing_on_separate_line -> break 1000 (-2) $ str ")" )
        in
        (fmt_if parens_branch (str " ("), close_paren, xast)
  in
  let expr_eol = Option.some_if cmts_before force_break in
  match c.fmt_opts.break_cases.v with
  | `Fit ->
      { leading_space= fmt_if (not first) space_break
      ; bar= fmt_or first (if_newline "| ") (str "| ")
      ; box_all= hvbox indent
      ; box_pattern_arrow= hovbox 2
      ; break_before_arrow= break 1 0
      ; break_after_arrow= noop
      ; open_paren_branch
      ; break_after_opening_paren= space_break
      ; expr_parens
      ; expr_eol
      ; branch_expr
      ; close_paren_branch }
  | `Nested ->
      { leading_space= fmt_if (not first) space_break
      ; bar= fmt_or first (if_newline "| ") (str "| ")
      ; box_all= Fn.id
      ; box_pattern_arrow= hovbox 0
      ; break_before_arrow= break 1 2
      ; break_after_arrow= fmt_if (not parens_branch) (break 0 3)
      ; open_paren_branch
      ; break_after_opening_paren=
          fmt_or (indent > 2) (break 1 4) (break 1 2)
      ; expr_parens
      ; expr_eol
      ; branch_expr
      ; close_paren_branch }
  | `Fit_or_vertical ->
      { leading_space= break_unless_newline 1000 0
      ; bar= str "| "
      ; box_all= hovbox indent
      ; box_pattern_arrow= hovbox 0
      ; break_before_arrow= break 1 2
      ; break_after_arrow= fmt_if (not parens_branch) (break 0 3)
      ; open_paren_branch
      ; break_after_opening_paren= space_break
      ; expr_parens
      ; expr_eol
      ; branch_expr
      ; close_paren_branch }
  | `Toplevel | `All ->
      { leading_space= break_unless_newline 1000 0
      ; bar= str "| "
      ; box_all= hvbox indent
      ; box_pattern_arrow= hovbox 0
      ; break_before_arrow= break 1 2
      ; break_after_arrow= fmt_if (not parens_branch) (break 0 3)
      ; open_paren_branch
      ; break_after_opening_paren= space_break
      ; expr_parens
      ; expr_eol
      ; branch_expr
      ; close_paren_branch }
  | `Vertical ->
      { leading_space= break_unless_newline 1000 0
      ; bar= str "| "
      ; box_all= hvbox indent
      ; box_pattern_arrow= hovbox 0
      ; break_before_arrow= break 1 2
      ; break_after_arrow= fmt_if (not parens_branch) (break 0 3)
      ; open_paren_branch
      ; break_after_opening_paren= break 1000 0
      ; expr_parens
      ; expr_eol
      ; branch_expr
      ; close_paren_branch }

let wrap_collec c ~space_around opn cls =
  if space_around then wrap (str opn $ char ' ') (break 1 0 $ str cls)
  else wrap_fits_breaks c opn cls

let wrap_record (c : Conf.t) =
  wrap_collec c ~space_around:c.fmt_opts.space_around_records.v "{" "}"

let wrap_tuple (c : Conf.t) ~parens ~no_parens_if_break items =
  let tuple_sep =
    match c.fmt_opts.break_separators.v with
    | `Before -> fits_breaks ", " ~hint:(1000, -2) ", "
    | `After -> str "," $ space_break
  in
  let k = list items tuple_sep Fn.id in
  if parens then wrap_fits_breaks c "(" ")" (hvbox 0 k)
  else if no_parens_if_break then k
  else fits_breaks "" "( " $ hvbox 0 k $ fits_breaks "" ~hint:(1, 0) ")"

type record_type =
  { docked_before: Fmt.t
  ; break_before: Fmt.t
  ; box_record: Fmt.t -> Fmt.t
  ; box_spaced: bool
  ; sep_before: Fmt.t
  ; sep_after: Fmt.t
  ; break_after: Fmt.t
  ; docked_after: Fmt.t }

let get_record_type (c : Conf.t) =
  let sparse_type_decl = Poly.(c.fmt_opts.type_decl.v = `Sparse) in
  let space = if c.fmt_opts.space_around_records.v then 1 else 0 in
  let dock = c.fmt_opts.dock_collection_brackets.v in
  let break_before, sep_before, sep_after =
    match c.fmt_opts.break_separators.v with
    | `Before ->
        ( fmt_or dock (break space 2) space_break
        , fmt_or sparse_type_decl
            (force_break $ str "; ")
            (cut_break $ str "; ")
        , noop )
    | `After ->
        ( fmt_or dock (break space 0) space_break
        , noop
        , fmt_or dock
            (fmt_or sparse_type_decl force_break space_break)
            (fmt_or sparse_type_decl (break 1000 2) (break 1 2)) )
  in
  { docked_before= fmt_if dock (str " {")
  ; break_before
  ; box_record= (fun k -> if dock then k else hvbox 0 (wrap_record c k))
  ; box_spaced= c.fmt_opts.space_around_records.v
  ; sep_before
  ; sep_after
  ; break_after= fmt_if dock (break space (-2))
  ; docked_after= fmt_if dock (str "}") }

type elements_collection =
  { box: Fmt.t -> Fmt.t
  ; sep_before: Fmt.t
  ; sep_after_non_final: Fmt.t
  ; sep_after_final: Fmt.t }

type elements_collection_record_expr = {break_after_with: Fmt.t}

let get_record_expr (c : Conf.t) =
  let space = if c.fmt_opts.space_around_records.v then 1 else 0 in
  let dock = c.fmt_opts.dock_collection_brackets.v in
  let box k =
    if dock then
      hvbox 0 (wrap (str "{") (str "}") (break space 2 $ k $ break space 0))
    else hvbox 0 (wrap_record c k)
  in
  ( ( match c.fmt_opts.break_separators.v with
    | `Before ->
        { box
        ; sep_before= cut_break $ str "; "
        ; sep_after_non_final= noop
        ; sep_after_final= noop }
    | `After ->
        { box
        ; sep_before= noop
        ; sep_after_non_final= str ";" $ break 1 2
        ; sep_after_final= fmt_if dock (fits_breaks ~level:0 "" ";") } )
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
                (wrap (str opn) (str cls)
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
                (wrap (str opn) (str cls)
                   (break space 2 $ box_collec c 0 k $ break space 0) )
            else box_collec c 0 (wrap_collec c ~space_around opn cls k) )
      ; sep_before= noop
      ; sep_after_non_final=
          fmt_or dock
            (str ";" $ break 1 0)
            (char ';' $ break 1 (String.length opn + 1))
      ; sep_after_final= fmt_if dock (fits_breaks ~level:1 "" ";") }

let get_list_expr (c : Conf.t) =
  collection_expr c ~space_around:c.fmt_opts.space_around_lists.v "[" "]"

let get_array_expr (c : Conf.t) =
  collection_expr c ~space_around:c.fmt_opts.space_around_arrays.v "[|" "|]"

let box_pattern_docked (c : Conf.t) ~ctx ~space_around ~pat opn cls k =
  let space = if space_around then 1 else 0 in
  let indent_opn, indent_cls =
    match (ctx, c.fmt_opts.break_separators.v) with
    | Ast.Exp {pexp_desc= Pexp_match _ | Pexp_try _; _}, `Before ->
        (String.length opn - 3, 1 - String.length opn)
    | Ast.Exp {pexp_desc= Pexp_match _ | Pexp_try _; _}, `After -> (-3, 1)
    | Ast.Exp {pexp_desc= Pexp_let ({pvbs_bindings; _}, _, _); _}, _
      when List.exists pvbs_bindings ~f:(fun b -> phys_equal b.pvb_pat pat)
      ->
        let ext_length =
          let binding =
            List.find_exn pvbs_bindings ~f:(fun b ->
                phys_equal b.pvb_pat pat )
          in
          binding.pvb_attributes.attrs_extension
          |> Option.map ~f:(fun ext -> ext.txt |> String.length |> ( + ) 1)
          |> Option.value ~default:0
        in
        (-4 - ext_length, 0)
    | _ -> (0, 0)
  in
  hvbox indent_opn
    (wrap (str opn) (str cls) (break space 2 $ k $ break space indent_cls))

let get_record_pat (c : Conf.t) ~ctx pat =
  let params, _ = get_record_expr c in
  let box =
    if c.fmt_opts.dock_collection_brackets.v then
      box_pattern_docked c ~ctx
        ~space_around:c.fmt_opts.space_around_records.v ~pat "{" "}"
    else params.box
  in
  {params with box}

let collection_pat (c : Conf.t) ~ctx ~space_around ~pat opn cls =
  let params = collection_expr c ~space_around opn cls in
  let box =
    if c.fmt_opts.dock_collection_brackets.v then
      box_collec c 0 >> box_pattern_docked c ~ctx ~space_around ~pat opn cls
    else params.box
  in
  {params with box}

let get_list_pat (c : Conf.t) ~ctx pat =
  collection_pat c ~ctx ~space_around:c.fmt_opts.space_around_lists.v ~pat
    "[" "]"

let get_array_pat (c : Conf.t) ~ctx pat =
  collection_pat c ~ctx ~space_around:c.fmt_opts.space_around_arrays.v ~pat
    "[|" "|]"

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
    ~xcond ~xbch ~expr_loc ~fmt_extension_suffix ~fmt_attributes ~fmt_cond
    ~cmts_before_kw ~cmts_after_kw =
  let imd = c.fmt_opts.indicate_multiline_delimiters.v in
  let beginend, branch_expr =
    let ast = xbch.Ast.ast in
    match ast with
    | {pexp_desc= Pexp_beginend nested_exp; pexp_attributes= []; _} ->
        (true, sub_exp ~ctx:(Exp ast) nested_exp)
    | _ -> (false, xbch)
  in
  let wrap_parens ~wrap_breaks k =
    if beginend then wrap (str "begin") (str "end") (wrap_breaks k)
    else if parens_bch then wrap (str "(") (str ")") (wrap_breaks k)
    else k
  in
  let get_parens_breaks ~opn_hint_indent ~cls_hint:(ch_sp, ch_sl) =
    let brk hint = fits_breaks "" ~hint "" in
    let oh_other = ((if beginend then 1 else 0), opn_hint_indent) in
    if beginend then
      let _, offset = ch_sl in
      wrap (brk oh_other) (break 1000 offset)
    else
      match imd with
      | `Space -> wrap (brk (1, opn_hint_indent)) (brk ch_sp)
      | `No -> wrap (brk oh_other) noop
      | `Closing_on_separate_line -> wrap (brk oh_other) (brk ch_sl)
  in
  let cond () =
    match xcond with
    | Some xcnd ->
        hvbox 2
          ( hvbox 0
              ( hvbox 2
                  ( fmt_if (not first) (str "else ")
                  $ str "if"
                  $ fmt_if first (fmt_opt fmt_extension_suffix)
                  $ fmt_attributes $ space_break $ fmt_cond xcnd )
              $ space_break $ cmts_before_kw $ str "then" )
          $ opt cmts_after_kw Fn.id )
    | None -> cmts_before_kw $ hvbox 2 (str "else" $ opt cmts_after_kw Fn.id)
  in
  let branch_pro ?(indent = 2) () =
    if Option.is_some cmts_after_kw then break 1000 indent
    else if beginend || parens_bch then str " "
    else break 1 indent
  in
  match c.fmt_opts.if_then_else.v with
  | `Compact ->
      { box_branch= hovbox ~name:"Params.get_if_then_else `Compact" 2
      ; cond= cond ()
      ; box_keyword_and_expr= Fn.id
      ; branch_pro= branch_pro ~indent:0 ()
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
      ; space_between_branches= space_break }
  | `K_R ->
      { box_branch= Fn.id
      ; cond= cond ()
      ; box_keyword_and_expr= Fn.id
      ; branch_pro= branch_pro ()
      ; wrap_parens= wrap_parens ~wrap_breaks:(wrap (break 1000 2) noop)
      ; box_expr= Some false
      ; expr_pro= None
      ; expr_eol= Some (break 1 2)
      ; branch_expr
      ; break_end_branch=
          fmt_if (parens_bch || beginend || not last) (break 1000 0)
      ; space_between_branches= fmt_if (beginend || parens_bch) (str " ") }
  | `Fit_or_vertical ->
      { box_branch=
          hovbox
            ( match imd with
            | `Closing_on_separate_line when parens_prev_bch -> -2
            | _ -> 0 )
      ; cond= cond ()
      ; box_keyword_and_expr= Fn.id
      ; branch_pro= branch_pro ()
      ; wrap_parens=
          wrap_parens
            ~wrap_breaks:
              (get_parens_breaks ~opn_hint_indent:2
                 ~cls_hint:((1, 0), (1000, 0)) )
      ; box_expr= Some false
      ; expr_pro=
          Some
            (fmt_if
               (not (Location.is_single_line expr_loc c.fmt_opts.margin.v))
               (break_unless_newline 1000 2) )
      ; expr_eol= Some (break 1 2)
      ; branch_expr
      ; break_end_branch= noop
      ; space_between_branches=
          ( match imd with
          | `Closing_on_separate_line when beginend || parens_bch -> str " "
          | _ -> space_break ) }
  | `Vertical ->
      { box_branch= Fn.id
      ; cond= cond ()
      ; box_keyword_and_expr= Fn.id
      ; branch_pro= branch_pro ()
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
          ( match imd with
          | `Closing_on_separate_line when parens_bch -> str " "
          | _ -> space_break ) }
  | `Keyword_first ->
      let keyword =
        hvbox 2
          ( fmt_or (Option.is_some xcond) (str "then") (str "else")
          $ opt cmts_after_kw Fn.id )
      and cond =
        match xcond with
        | Some xcond ->
            hvbox 2
              ( fmt_or first
                  (str "if" $ fmt_opt fmt_extension_suffix)
                  (str "else if")
              $ fmt_attributes $ space_break $ fmt_cond xcond
              $ cmts_before_kw )
            $ space_break
        | None -> cmts_before_kw
      in
      { box_branch= Fn.id
      ; cond
      ; box_keyword_and_expr= (fun k -> hovbox 2 (keyword $ k))
      ; branch_pro= branch_pro ~indent:0 ()
      ; wrap_parens=
          wrap_parens
            ~wrap_breaks:
              (get_parens_breaks ~opn_hint_indent:0
                 ~cls_hint:((1, 0), (1000, -2)) )
      ; box_expr= None
      ; expr_pro= None
      ; expr_eol= None
      ; branch_expr
      ; break_end_branch= noop
      ; space_between_branches= space_break }

let match_indent ?(default = 0) (c : Conf.t) ~parens ~(ctx : Ast.t) =
  match (c.fmt_opts.match_indent_nested.v, ctx) with
  | `Always, _ | _, (Top | Sig _ | Str _) -> c.fmt_opts.match_indent.v
  | _, Exp {pexp_desc= Pexp_infix _; _}
    when c.fmt_opts.ocp_indent_compat.v && parens ->
      2 (* Match is docked *)
  | _ -> default

let comma_sep (c : Conf.t) : Fmt.t =
  match c.fmt_opts.break_separators.v with
  | `Before -> cut_break $ str ", "
  | `After -> str "," $ break 1 2

module Align = struct
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

  let fun_decl (c : Conf.t) ~decl ~pattern ~args =
    if c.fmt_opts.ocp_indent_compat.v then
      hovbox 4 (decl $ hvbox 2 (pattern $ args))
    else hovbox 4 (decl $ pattern) $ args

  let module_pack (c : Conf.t) ~me =
    if not c.fmt_opts.ocp_indent_compat.v then false
    else
      (* Align when the constraint is not desugared. *)
      match me.pmod_desc with
      | Pmod_structure _ | Pmod_ident _ -> false
      | _ -> true
end

module Indent = struct
  let function_ = Exp.indent_function

  let fun_type_annot c = if ocp c then 2 else 4

  let fun_args c = if ocp c then 6 else 4

  let docked_function_after_fun (c : Conf.t) ~parens ~ctx0 ~ctx =
    match ctx0 with
    | Str _ | Lb _ ->
        (* Cases must be 2-indented relative to the [let], even when
           [let_binding_deindent_fun] is on. *)
        if c.fmt_opts.let_binding_deindent_fun.v then 1 else 0
    | _ when ctx_is_infix ctx0 -> 0
    | _ when ocp c -> (
      match ctx_is_apply_and_exp_is_arg ~ctx ctx0 with
      | Some (_, _, true) -> (* Last argument *) 2
      | _ -> if parens then 3 else 2 )
    | _ -> 2

  let fun_args_group (c : Conf.t) ~lbl exp =
    if not (ocp c) then 2
    else
      match exp.pexp_desc with
      | Pexp_function ([], None, Pfunction_cases _) -> 2
      | _ -> ( match lbl with Nolabel -> 3 | _ -> 2 )

  let record_docstring (c : Conf.t) =
    if ocp c then
      match c.fmt_opts.break_separators.v with `Before -> 0 | `After -> 2
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

  let variant c ~parens = if ocp c && parens then 3 else 2

  let variant_type_arg c = if ocp c then 2 else 0
end
