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

open Migrate_ast
open Asttypes
open Parsetree
open Ast

let rec sugar_arrow_typ cmts ({ast= typ} as xtyp) =
  let ctx = Typ typ in
  let {ptyp_desc; ptyp_loc} = typ in
  match ptyp_desc with
  | Ptyp_arrow (l, t1, t2) ->
      Cmts.relocate cmts ~src:ptyp_loc ~before:t1.ptyp_loc
        ~after:t2.ptyp_loc ;
      let rest =
        match t2.ptyp_attributes with
        | [] -> sugar_arrow_typ cmts (sub_typ ~ctx t2)
        | _ -> [(Nolabel, sub_typ ~ctx t2)]
      in
      (l, sub_typ ~ctx t1) :: rest
  | _ -> [(Nolabel, xtyp)]

let rec sugar_class_arrow_typ cmts ({ast= typ} as xtyp) =
  let ctx = Cty typ in
  let {pcty_desc; pcty_loc} = typ in
  match pcty_desc with
  | Pcty_arrow (l, t1, t2) ->
      Cmts.relocate cmts ~src:pcty_loc ~before:t1.ptyp_loc
        ~after:t2.pcty_loc ;
      let rest =
        match t2.pcty_attributes with
        | [] -> sugar_class_arrow_typ cmts (sub_cty ~ctx t2)
        | _ -> [(Nolabel, `class_type (sub_cty ~ctx t2))]
      in
      (l, `core_type (sub_typ ~ctx t1)) :: rest
  | _ -> [(Nolabel, `class_type xtyp)]

let rec sugar_or_pat ?(allow_attribute = true) cmts ({ast= pat} as xpat) =
  let ctx = Pat pat in
  match pat with
  | {ppat_desc= Ppat_or (pat1, pat2); ppat_loc; ppat_attributes= []} ->
      Cmts.relocate cmts ~src:ppat_loc ~before:pat1.ppat_loc
        ~after:pat2.ppat_loc ;
      sugar_or_pat ~allow_attribute:false cmts (sub_pat ~ctx pat1)
      @ sugar_or_pat ~allow_attribute:false cmts (sub_pat ~ctx pat2)
  | {ppat_desc= Ppat_or (pat1, pat2); ppat_loc} when allow_attribute ->
      Cmts.relocate cmts ~src:ppat_loc ~before:pat1.ppat_loc
        ~after:pat2.ppat_loc ;
      [sub_pat ~ctx pat1; sub_pat ~ctx pat2]
  | _ -> [xpat]

type arg_kind =
  | Val of arg_label * pattern xt * expression xt option
  | Newtypes of string loc list

let sugar_fun cmts ?(will_keep_first_ast_node = true) xexp =
  let rec sugar_fun_ ?(will_keep_first_ast_node = false) ({ast= exp} as xexp)
      =
    let ctx = Exp exp in
    let {pexp_desc; pexp_loc; pexp_attributes} = exp in
    if will_keep_first_ast_node || List.is_empty pexp_attributes then
      match pexp_desc with
      | Pexp_fun (label, default, pattern, body) ->
          if not will_keep_first_ast_node then
            Cmts.relocate cmts ~src:pexp_loc ~before:pattern.ppat_loc
              ~after:body.pexp_loc ;
          let xargs, xbody = sugar_fun_ (sub_exp ~ctx body) in
          ( Val
              ( label
              , sub_pat ~ctx pattern
              , Option.map default ~f:(sub_exp ~ctx) )
            :: xargs
          , xbody )
      | Pexp_newtype (name, body) ->
          if not will_keep_first_ast_node then
            Cmts.relocate cmts ~src:pexp_loc ~before:body.pexp_loc
              ~after:body.pexp_loc ;
          let xargs, xbody = sugar_fun_ (sub_exp ~ctx body) in
          let xargs =
            match xargs with
            | Newtypes names :: xargs -> Newtypes (name :: names) :: xargs
            | xargs -> Newtypes [name] :: xargs
          in
          (xargs, xbody)
      | _ -> ([], xexp)
    else ([], xexp)
  in
  sugar_fun_ ~will_keep_first_ast_node xexp

let sugar_cl_fun ?(will_keep_first_ast_node = true) cmts xexp =
  let rec sugar_fun_ ?(will_keep_first_ast_node = false) ({ast= exp} as xexp)
      =
    let ctx = Cl exp in
    let {pcl_desc; pcl_loc; pcl_attributes} = exp in
    if will_keep_first_ast_node || List.is_empty pcl_attributes then
      match pcl_desc with
      | Pcl_fun (label, default, pattern, body) ->
          if not will_keep_first_ast_node then
            Cmts.relocate cmts ~src:pcl_loc ~before:pattern.ppat_loc
              ~after:body.pcl_loc ;
          let xargs, xbody = sugar_fun_ (sub_cl ~ctx body) in
          ( Val
              ( label
              , sub_pat ~ctx pattern
              , Option.map default ~f:(sub_exp ~ctx) )
            :: xargs
          , xbody )
      | _ -> ([], xexp)
    else ([], xexp)
  in
  sugar_fun_ ~will_keep_first_ast_node xexp

let sugar_infix cmts prec xexp =
  let assoc = Option.value_map prec ~default:Non ~f:assoc_of_prec in
  let rec sugar_infix_ ?(relocate = true) xop ((lbl, {ast= exp}) as xexp) =
    assert (Poly.(lbl = Nolabel)) ;
    let ctx = Exp exp in
    match (assoc, exp) with
    | Left, {pexp_desc= Pexp_apply (e0, [(l1, e1); (l2, e2)]); pexp_loc}
      when Poly.(prec = prec_ast (Exp exp)) ->
        let op_args1 = sugar_infix_ None (l1, sub_exp ~ctx e1) in
        let src = pexp_loc in
        let after = e2.pexp_loc in
        ( match op_args1 with
        | (Some {ast= {pexp_loc= before}}, _) :: _
         |(None, (_, {ast= {pexp_loc= before}}) :: _) :: _ ->
            if relocate then Cmts.relocate cmts ~src ~before ~after
        | _ ->
            if relocate then
              Cmts.relocate cmts ~src ~before:e0.pexp_loc ~after ) ;
        op_args1 @ [(Some (sub_exp ~ctx e0), [(l2, sub_exp ~ctx e2)])]
    | Right, {pexp_desc= Pexp_apply (e0, [(l1, e1); (l2, e2)]); pexp_loc}
      when Poly.(prec = prec_ast (Exp exp)) ->
        let op_args2 =
          sugar_infix_ (Some (sub_exp ~ctx e0)) (l2, sub_exp ~ctx e2)
        in
        let src = pexp_loc in
        let after = e1.pexp_loc in
        let before =
          match xop with Some {ast} -> ast.pexp_loc | None -> e1.pexp_loc
        in
        let may_relocate ~after =
          if relocate then Cmts.relocate cmts ~src ~before ~after
        in
        ( match List.last op_args2 with
        | Some (_, args2) -> (
          match List.last args2 with
          | Some (_, {ast= {pexp_loc= after}}) -> may_relocate ~after
          | None -> may_relocate ~after )
        | _ -> may_relocate ~after ) ;
        (xop, [(l1, sub_exp ~ctx e1)]) :: op_args2
    | _ -> [(xop, [xexp])]
  in
  sugar_infix_ None ~relocate:false (Nolabel, xexp)

let rec sugar_list_pat cmts pat =
  let ctx = Pat pat in
  let {ppat_desc; ppat_loc= src} = pat in
  match ppat_desc with
  | Ppat_construct ({txt= Lident "[]"; loc}, None) ->
      Cmts.relocate cmts ~src ~before:loc ~after:loc ;
      Some ([], loc)
  | Ppat_construct
      ( {txt= Lident "::"; loc}
      , Some {ppat_desc= Ppat_tuple [hd; tl]; ppat_loc; ppat_attributes= []}
      ) -> (
    match sugar_list_pat cmts tl with
    | Some (xtl, nil_loc) when List.is_empty tl.ppat_attributes ->
        Some (([src; loc; ppat_loc], sub_pat ~ctx hd) :: xtl, nil_loc)
    | _ -> None )
  | _ -> None

let sugar_list_exp cmts exp =
  let rec sugar_list_exp_ exp =
    let ctx = Exp exp in
    let {pexp_desc; pexp_loc= src} = exp in
    match pexp_desc with
    | Pexp_construct ({txt= Lident "[]"; loc}, None) ->
        Cmts.relocate cmts ~src ~before:loc ~after:loc ;
        Some ([], loc)
    | Pexp_construct
        ( {txt= Lident "::"; loc}
        , Some
            {pexp_desc= Pexp_tuple [hd; tl]; pexp_loc; pexp_attributes= []}
        ) -> (
      match sugar_list_exp_ tl with
      | Some (xtl, nil_loc) when List.is_empty tl.pexp_attributes ->
          Some (([src; loc; pexp_loc], sub_exp ~ctx hd) :: xtl, nil_loc)
      | _ -> None )
    | _ -> None
  in
  let r = sugar_list_exp_ exp in
  assert (Bool.equal (Option.is_some r) (is_sugared_list exp)) ;
  r

let sugar_infix_cons xexp =
  let rec sugar_infix_cons_ ({ast= exp} as xexp) =
    let ctx = Exp exp in
    let {pexp_desc; pexp_loc= l1} = exp in
    match pexp_desc with
    | Pexp_construct
        ( {txt= Lident "::"; loc= l2}
        , Some
            { pexp_desc= Pexp_tuple [hd; tl]
            ; pexp_loc= l3
            ; pexp_attributes= [] } ) -> (
      match sugar_infix_cons_ (sub_exp ~ctx tl) with
      | xtl when List.is_empty tl.pexp_attributes ->
          ([l1; l2; l3], sub_exp ~ctx hd) :: xtl
      | _ -> [([l1; l2; l3], sub_exp ~ctx hd); ([], sub_exp ~ctx tl)] )
    | _ -> [([], xexp)]
  in
  sugar_infix_cons_ xexp

let rec sugar_ite cmts ({ast= exp} as xexp) =
  let ctx = Exp exp in
  let {pexp_desc; pexp_loc; pexp_attributes} = exp in
  match pexp_desc with
  | Pexp_ifthenelse (cnd, thn, Some els) ->
      Cmts.relocate cmts ~src:pexp_loc ~before:cnd.pexp_loc
        ~after:els.pexp_loc ;
      (Some (sub_exp ~ctx cnd), sub_exp ~ctx thn, pexp_attributes)
      :: sugar_ite cmts (sub_exp ~ctx els)
  | Pexp_ifthenelse (cnd, thn, None) ->
      Cmts.relocate cmts ~src:pexp_loc ~before:cnd.pexp_loc
        ~after:thn.pexp_loc ;
      [(Some (sub_exp ~ctx cnd), sub_exp ~ctx thn, pexp_attributes)]
  | _ -> [(None, xexp, pexp_attributes)]

let sugar_sequence conf cmts width xexp =
  let rec sugar_sequence_ ?(allow_attribute = true) ({ast= exp} as xexp) =
    let ctx = Exp exp in
    let {pexp_desc; pexp_loc} = exp in
    match pexp_desc with
    | Pexp_sequence (e1, e2) ->
        Cmts.relocate cmts ~src:pexp_loc ~before:e1.pexp_loc
          ~after:e2.pexp_loc ;
        if (not allow_attribute) && not (List.is_empty exp.pexp_attributes)
        then [xexp]
        else if Ast.exposed_right_exp Ast.Let_match e1 then
          [sub_exp ~ctx e1; sub_exp ~ctx e2]
        else
          List.append
            (sugar_sequence_ ~allow_attribute:false (sub_exp ~ctx e1))
            (sugar_sequence_ ~allow_attribute:false (sub_exp ~ctx e2))
    | _ -> [xexp]
  in
  List.group (sugar_sequence_ xexp) ~break:(fun xexp1 xexp2 ->
      (not (is_simple conf width xexp1)) || not (is_simple conf width xexp2)
  )

let rec sugar_functor_type cmts ~for_functor_kw ({ast= mty} as xmty) =
  let ctx = Mty mty in
  match mty with
  | {pmty_desc= Pmty_functor (arg, arg_mty, body); pmty_loc; pmty_attributes}
    when for_functor_kw
         || (List.is_empty pmty_attributes && not (String.equal arg.txt "_"))
    ->
      let arg =
        if String.equal "*" arg.txt then {arg with txt= ""} else arg
      in
      Cmts.relocate cmts ~src:pmty_loc ~before:arg.loc ~after:body.pmty_loc ;
      let body = sub_mty ~ctx body in
      let xargs, xbody =
        match pmty_attributes with
        | [] -> sugar_functor_type cmts ~for_functor_kw body
        | _ -> ([], body)
      in
      ((arg, Option.map arg_mty ~f:(sub_mty ~ctx)) :: xargs, xbody)
  | _ -> ([], xmty)

let rec sugar_functor cmts ~for_functor_kw ({ast= me} as xme) =
  let ctx = Mod me in
  match me with
  | {pmod_desc= Pmod_functor (arg, arg_mt, body); pmod_loc; pmod_attributes}
    when for_functor_kw
         || (List.is_empty pmod_attributes && not (String.equal arg.txt "_"))
    ->
      let arg =
        if String.equal "*" arg.txt then {arg with txt= ""} else arg
      in
      Cmts.relocate cmts ~src:pmod_loc ~before:arg.loc ~after:body.pmod_loc ;
      let xarg_mt = Option.map arg_mt ~f:(sub_mty ~ctx) in
      let ctx = Mod body in
      let body = sub_mod ~ctx body in
      let xargs, xbody_me =
        match pmod_attributes with
        | [] -> sugar_functor cmts ~for_functor_kw body
        | _ -> ([], body)
      in
      ((arg, xarg_mt) :: xargs, xbody_me)
  | _ -> ([], xme)

let sugar_mod_with pmty =
  let rec sugar_mod_with_ ({ast= me} as xme) =
    let ctx = Mty me in
    match me with
    | {pmty_desc= Pmty_with (mt, wcs); pmty_attributes; pmty_loc} ->
        let args, rest =
          match pmty_attributes with
          | [] -> sugar_mod_with_ (sub_mty ~ctx mt)
          | _ -> ([], sub_mty ~ctx mt)
        in
        ((wcs, pmty_loc) :: args, rest)
    | _ -> ([], xme)
  in
  let l_rev, m = sugar_mod_with_ pmty in
  (List.rev l_rev, m)

let sugar_polynewtype cmts pat body =
  let ctx = Pat pat in
  match pat.ppat_desc with
  | Ppat_constraint (pat2, {ptyp_desc= Ptyp_poly (pvars, _typ)}) ->
      let rec sugar_polynewtype_ xpat pvars0 pvars body =
        let ctx = Exp body in
        match (pvars, body.pexp_desc) with
        | [], Pexp_constraint (exp, typ) ->
            Some (xpat, pvars0, sub_typ ~ctx typ, sub_exp ~ctx exp)
        | ( {txt= pvar; loc= loc1} :: pvars
          , Pexp_newtype ({txt= nvar; loc= loc2}, exp) )
          when String.equal pvar nvar ->
            Cmts.relocate cmts ~src:loc2 ~before:loc1 ~after:loc1 ;
            sugar_polynewtype_ xpat pvars0 pvars exp
        | _ -> None
      in
      Cmts.relocate cmts ~src:pat.ppat_loc ~before:pat2.ppat_loc
        ~after:pat2.ppat_loc ;
      sugar_polynewtype_ (sub_pat ~ctx pat2) pvars pvars body
  | _ -> None
