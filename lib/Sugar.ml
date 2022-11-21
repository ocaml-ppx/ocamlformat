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

open Migrate_ast
open Asttypes
open Ast
open Extended_ast

let rec or_pat ?(allow_attribute = true) cmts ({ast= pat; _} as xpat) =
  let ctx = Pat pat in
  match pat with
  | {ppat_desc= Ppat_or (pat1, pat2); ppat_loc; ppat_attributes= []; _} ->
      Cmts.relocate cmts ~src:ppat_loc ~before:pat1.ppat_loc
        ~after:pat2.ppat_loc ;
      or_pat ~allow_attribute:false cmts (sub_pat ~ctx pat1)
      @ or_pat ~allow_attribute:false cmts (sub_pat ~ctx pat2)
  | {ppat_desc= Ppat_or (pat1, pat2); ppat_loc; _} when allow_attribute ->
      Cmts.relocate cmts ~src:ppat_loc ~before:pat1.ppat_loc
        ~after:pat2.ppat_loc ;
      [sub_pat ~ctx pat1; sub_pat ~ctx pat2]
  | _ -> [xpat]

type arg_kind =
  | Val of arg_label * pattern xt * expression xt option
  | Newtypes of string loc list

let fun_ cmts ?(will_keep_first_ast_node = true) xexp =
  let rec fun_ ?(will_keep_first_ast_node = false) ({ast= exp; _} as xexp) =
    let ctx = Exp exp in
    let {pexp_desc; pexp_loc; pexp_attributes; _} = exp in
    if will_keep_first_ast_node || List.is_empty pexp_attributes then
      match pexp_desc with
      | Pexp_fun (label, default, pattern, body) ->
          if not will_keep_first_ast_node then
            Cmts.relocate cmts ~src:pexp_loc ~before:pattern.ppat_loc
              ~after:body.pexp_loc ;
          let xargs, xbody = fun_ (sub_exp ~ctx body) in
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
          let xargs, xbody = fun_ (sub_exp ~ctx body) in
          let xargs =
            match xargs with
            | Newtypes names :: xargs -> Newtypes (name :: names) :: xargs
            | xargs -> Newtypes [name] :: xargs
          in
          (xargs, xbody)
      | _ -> ([], xexp)
    else ([], xexp)
  in
  fun_ ~will_keep_first_ast_node xexp

let cl_fun ?(will_keep_first_ast_node = true) cmts xexp =
  let rec fun_ ?(will_keep_first_ast_node = false) ({ast= exp; _} as xexp) =
    let ctx = Cl exp in
    let {pcl_desc; pcl_loc; pcl_attributes; _} = exp in
    if will_keep_first_ast_node || List.is_empty pcl_attributes then
      match pcl_desc with
      | Pcl_fun (label, default, pattern, body) ->
          if not will_keep_first_ast_node then
            Cmts.relocate cmts ~src:pcl_loc ~before:pattern.ppat_loc
              ~after:body.pcl_loc ;
          let xargs, xbody = fun_ (sub_cl ~ctx body) in
          ( Val
              ( label
              , sub_pat ~ctx pattern
              , Option.map default ~f:(sub_exp ~ctx) )
            :: xargs
          , xbody )
      | _ -> ([], xexp)
    else ([], xexp)
  in
  fun_ ~will_keep_first_ast_node xexp

module Exp = struct
  let infix cmts prec xexp =
    let assoc = Option.value_map prec ~default:Assoc.Non ~f:Assoc.of_prec in
    let rec infix_ ?(relocate = true) xop xexp =
      let ctx = Exp xexp.ast in
      match (assoc, xexp.ast) with
      | ( Left
        , {pexp_desc= Pexp_infix ({txt= op; loc}, e1, e2); pexp_loc= src; _}
        )
        when Option.equal Prec.equal prec (prec_ast ctx) ->
          let op_args1 = infix_ None (sub_exp ~ctx e1) in
          let before =
            match op_args1 with
            | (Some {loc; _}, _) :: _ -> loc
            | (None, {ast= {pexp_loc; _}; _}) :: _ -> pexp_loc
            | _ -> loc
          in
          if relocate then Cmts.relocate cmts ~src ~before ~after:e2.pexp_loc ;
          op_args1 @ [(Some {txt= op; loc}, sub_exp ~ctx e2)]
      | ( Right
        , {pexp_desc= Pexp_infix ({txt= op; loc}, e1, e2); pexp_loc= src; _}
        )
        when Option.equal Prec.equal prec (prec_ast ctx) ->
          let op_args2 = infix_ (Some {txt= op; loc}) (sub_exp ~ctx e2) in
          let before =
            match xop with Some op -> op.loc | None -> e1.pexp_loc
          in
          let after =
            match List.last op_args2 with
            | Some (_, {ast= {pexp_loc; _}; _}) -> pexp_loc
            | None -> e1.pexp_loc
          in
          if relocate then Cmts.relocate cmts ~src ~before ~after ;
          (xop, sub_exp ~ctx e1) :: op_args2
      | _ -> [(xop, xexp)]
    in
    infix_ None ~relocate:false xexp
end

let sequence cmts xexp =
  let rec sequence_ ?(allow_attribute = true) ({ast= exp; _} as xexp) =
    let ctx = Exp exp in
    let {pexp_desc; pexp_loc; _} = exp in
    match pexp_desc with
    | Pexp_extension
        ( ext
        , PStr
            [ { pstr_desc=
                  Pstr_eval
                    ( ( { pexp_desc= Pexp_sequence (e1, e2)
                        ; pexp_attributes
                        ; _ } as exp )
                    , _ )
              ; pstr_loc } ] )
      when List.is_empty pexp_attributes
           && Source.extension_using_sugar ~name:ext ~payload:e1.pexp_loc ->
        let ctx = Exp exp in
        if (not allow_attribute) && not (List.is_empty exp.pexp_attributes)
        then [(None, xexp)]
        else (
          Cmts.relocate cmts ~src:pstr_loc ~before:e1.pexp_loc
            ~after:e2.pexp_loc ;
          Cmts.relocate cmts ~src:pexp_loc ~before:e1.pexp_loc
            ~after:e2.pexp_loc ;
          if Ast.exposed_right_exp Ast.Let_match e1 then
            [(None, sub_exp ~ctx e1); (Some ext, sub_exp ~ctx e2)]
          else
            let l1 = sequence_ ~allow_attribute:false (sub_exp ~ctx e1) in
            let l2 =
              match sequence_ ~allow_attribute:false (sub_exp ~ctx e2) with
              | [] -> []
              | (_, e2) :: l2 -> (Some ext, e2) :: l2
            in
            List.append l1 l2 )
    | Pexp_sequence (e1, e2) ->
        if (not allow_attribute) && not (List.is_empty exp.pexp_attributes)
        then [(None, xexp)]
        else (
          Cmts.relocate cmts ~src:pexp_loc ~before:e1.pexp_loc
            ~after:e2.pexp_loc ;
          if Ast.exposed_right_exp Ast.Let_match e1 then
            [(None, sub_exp ~ctx e1); (None, sub_exp ~ctx e2)]
          else
            List.append
              (sequence_ ~allow_attribute:false (sub_exp ~ctx e1))
              (sequence_ ~allow_attribute:false (sub_exp ~ctx e2)) )
    | _ -> [(None, xexp)]
  in
  sequence_ xexp

type functor_arg = Unit | Named of label option loc * module_type Ast.xt

(* The sugar is different when used with the [functor] keyword. The syntax
   M(A : A)(B : B) cannot handle [_] as module name. *)
let rec functor_type cmts ~for_functor_kw ~source_is_long
    ({ast= mty; _} as xmty) =
  let ctx = Mty mty in
  match mty with
  | {pmty_desc= Pmty_functor (fp, body); pmty_loc; pmty_attributes}
    when for_functor_kw
         || (List.is_empty pmty_attributes && not (source_is_long mty)) ->
      let functor_arg =
        match fp with
        | Named (arg, arg_mty) -> Named (arg, sub_mty ~ctx arg_mty)
        | Unit -> Unit
      in
      let body = sub_mty ~ctx body in
      let xargs, xbody =
        match pmty_attributes with
        | [] -> functor_type cmts ~for_functor_kw ~source_is_long body
        | _ -> ([], body)
      in
      (Location.mkloc functor_arg pmty_loc :: xargs, xbody)
  | _ -> ([], xmty)

(* The sugar is different when used with the [functor] keyword. The syntax
   M(A : A)(B : B) cannot handle [_] as module name. *)
let rec functor_ cmts ~for_functor_kw ~source_is_long ({ast= me; _} as xme) =
  let ctx = Mod me in
  match me with
  | {pmod_desc= Pmod_functor (fp, body); pmod_loc; pmod_attributes}
    when for_functor_kw
         || (List.is_empty pmod_attributes && not (source_is_long me)) ->
      let functor_arg =
        match fp with
        | Named (arg, arg_mt) -> Named (arg, sub_mty ~ctx arg_mt)
        | Unit -> Unit
      in
      let ctx = Mod body in
      let body = sub_mod ~ctx body in
      let xargs, xbody_me =
        match pmod_attributes with
        | [] -> functor_ cmts ~for_functor_kw ~source_is_long body
        | _ -> ([], body)
      in
      (Location.mkloc functor_arg pmod_loc :: xargs, xbody_me)
  | _ -> ([], xme)

let mod_with pmty =
  let rec mod_with_ ({ast= me; _} as xme) =
    let ctx = Mty me in
    match me with
    | {pmty_desc= Pmty_with (mt, wcs); pmty_attributes; pmty_loc} ->
        let args, rest = mod_with_ (sub_mty ~ctx mt) in
        ((wcs, pmty_loc, pmty_attributes) :: args, rest)
    | _ -> ([], xme)
  in
  let l_rev, m = mod_with_ pmty in
  (List.rev l_rev, m)

let rec polynewtype_ cmts pvars body relocs =
  let ctx = Exp body in
  match (pvars, body.pexp_desc) with
  | [], Pexp_constraint (exp, typ) ->
      let relocs = (body.pexp_loc, exp.pexp_loc) :: relocs in
      Some (sub_typ ~ctx typ, sub_exp ~ctx exp, relocs)
  | pvar :: pvars, Pexp_newtype (nvar, exp)
    when String.equal pvar.txt nvar.txt ->
      let relocs = (nvar.loc, pvar.loc) :: relocs in
      polynewtype_ cmts pvars exp relocs
  | _ -> None

(** [polynewtype cmts pat exp] returns expression of a type-constrained
    pattern [pat] with body [exp]. e.g.:

    {v
      let f: 'r 's. 'r 's t = fun (type r) -> fun (type s) -> (e : r s t)
    v}

    Can be rewritten as:

    {[
      let f : type r s. r s t = e
    ]} *)
let polynewtype cmts pat body =
  let ctx = Pat pat in
  match pat.ppat_desc with
  | Ppat_constraint (pat2, {ptyp_desc= Ptyp_poly (pvars, _); _}) -> (
    match polynewtype_ cmts pvars body [(pat.ppat_loc, pat2.ppat_loc)] with
    | Some (typ, exp, relocs) ->
        List.iter relocs ~f:(fun (src, dst) ->
            Cmts.relocate cmts ~src ~before:dst ~after:dst ) ;
        Some (sub_pat ~ctx pat2, pvars, typ, exp)
    | None -> None )
  | _ -> None

module Let_binding = struct
  type t =
    { lb_op: string loc
    ; lb_pat: pattern xt
    ; lb_typ:
        [ `Polynewtype of label loc list * core_type xt
        | `Coerce of core_type xt option * core_type xt
        | `Other of arg_kind list * core_type xt
        | `None of arg_kind list ]
    ; lb_exp: expression xt
    ; lb_pun: bool
    ; lb_attrs: attribute list
    ; lb_loc: Location.t }

  let type_cstr cmts ~ctx lb_pat lb_exp =
    let ({ast= pat; _} as xpat) =
      match (lb_pat.ppat_desc, lb_exp.pexp_desc) with
      (* recognize and undo the pattern of code introduced by
         ocaml/ocaml@fd0dc6a0fbf73323c37a73ea7e8ffc150059d6ff to fix
         https://caml.inria.fr/mantis/view.php?id=7344 *)
      | ( Ppat_constraint
            ( ({ppat_desc= Ppat_var _; _} as pat)
            , {ptyp_desc= Ptyp_poly ([], typ1); _} )
        , Pexp_constraint (_, typ2) )
        when equal_core_type typ1 typ2 ->
          Cmts.relocate cmts ~src:lb_pat.ppat_loc ~before:pat.ppat_loc
            ~after:pat.ppat_loc ;
          sub_pat ~ctx:(Pat lb_pat) pat
      | ( Ppat_constraint (pat, {ptyp_desc= Ptyp_poly ([], typ1); _})
        , Pexp_coerce (_, _, typ2) )
        when equal_core_type typ1 typ2 ->
          Cmts.relocate cmts ~src:lb_pat.ppat_loc ~before:pat.ppat_loc
            ~after:pat.ppat_loc ;
          sub_pat ~ctx:(Pat lb_pat) pat
      | _ -> sub_pat ~ctx lb_pat
    in
    let pat_is_extension {ppat_desc; _} =
      match ppat_desc with Ppat_extension _ -> true | _ -> false
    in
    let ({ast= body; _} as xbody) = sub_exp ~ctx lb_exp in
    if
      (not (List.is_empty xbody.ast.pexp_attributes)) || pat_is_extension pat
    then (xpat, `None [], xbody)
    else
      match polynewtype cmts pat body with
      | Some (xpat, pvars, xtyp, xbody) ->
          (xpat, `Polynewtype (pvars, xtyp), xbody)
      | None -> (
          let xpat =
            match xpat.ast.ppat_desc with
            | Ppat_constraint (p, {ptyp_desc= Ptyp_poly ([], _); _}) ->
                sub_pat ~ctx:xpat.ctx p
            | _ -> xpat
          in
          let xargs, ({ast= body; _} as xbody) =
            match pat with
            | {ppat_desc= Ppat_var _; ppat_attributes= []; _} ->
                fun_ cmts ~will_keep_first_ast_node:false xbody
            | _ -> ([], xbody)
          in
          let ctx = Exp body in
          match (body.pexp_desc, pat.ppat_desc) with
          | ( Pexp_constraint
                ( ({pexp_desc= Pexp_pack _; pexp_attributes= []; _} as exp)
                , ({ptyp_desc= Ptyp_package _; ptyp_attributes= []; _} as typ)
                )
            , _ )
            when Source.type_constraint_is_first typ exp.pexp_loc ->
              Cmts.relocate cmts ~src:body.pexp_loc ~before:exp.pexp_loc
                ~after:exp.pexp_loc ;
              (xpat, `Other (xargs, sub_typ ~ctx typ), sub_exp ~ctx exp)
          | ( Pexp_constraint
                ({pexp_desc= Pexp_pack _; _}, {ptyp_desc= Ptyp_package _; _})
            , _ )
           |Pexp_constraint _, Ppat_constraint _ ->
              (xpat, `None xargs, xbody)
          | Pexp_constraint (exp, typ), _
            when Source.type_constraint_is_first typ exp.pexp_loc ->
              Cmts.relocate cmts ~src:body.pexp_loc ~before:exp.pexp_loc
                ~after:exp.pexp_loc ;
              (xpat, `Other (xargs, sub_typ ~ctx typ), sub_exp ~ctx exp)
          (* The type constraint is always printed before the declaration for
             functions, for other value bindings we preserve its position. *)
          | Pexp_constraint (exp, typ), _ when not (List.is_empty xargs) ->
              Cmts.relocate cmts ~src:body.pexp_loc ~before:exp.pexp_loc
                ~after:exp.pexp_loc ;
              (xpat, `Other (xargs, sub_typ ~ctx typ), sub_exp ~ctx exp)
          | Pexp_coerce (exp, typ1, typ2), _
            when Source.type_constraint_is_first typ2 exp.pexp_loc ->
              Cmts.relocate cmts ~src:body.pexp_loc ~before:exp.pexp_loc
                ~after:exp.pexp_loc ;
              let typ1 = Option.map typ1 ~f:(sub_typ ~ctx) in
              (xpat, `Coerce (typ1, sub_typ ~ctx typ2), sub_exp ~ctx exp)
          | _ -> (xpat, `None xargs, xbody) )

  let of_value_binding cmts ~ctx ~first vb =
    let pat, typ, exp = type_cstr cmts ~ctx vb.pvb_pat vb.pvb_expr in
    { lb_op= Location.{txt= (if first then "let" else "and"); loc= none}
    ; lb_pat= pat
    ; lb_typ= typ
    ; lb_exp= exp
    ; lb_pun= false
    ; lb_attrs= vb.pvb_attributes
    ; lb_loc= vb.pvb_loc }

  let of_value_bindings cmts ~ctx =
    List.mapi ~f:(fun i -> of_value_binding cmts ~ctx ~first:(i = 0))

  let of_binding_ops cmts ~ctx bos =
    List.map bos ~f:(fun bo ->
        let pat, typ, exp = type_cstr cmts ~ctx bo.pbop_pat bo.pbop_exp in
        { lb_op= bo.pbop_op
        ; lb_pat= pat
        ; lb_typ= typ
        ; lb_exp= exp
        ; lb_pun=
            ( match (pat.ast.ppat_desc, exp.ast.pexp_desc) with
            | Ppat_var {txt= v; _}, Pexp_ident {txt= Lident e; _} ->
                String.equal v e
            | _ -> false )
        ; lb_attrs= []
        ; lb_loc= bo.pbop_loc } )
end
