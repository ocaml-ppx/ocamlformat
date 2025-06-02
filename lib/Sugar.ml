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

let mk_function_param {Location.loc_start; _} {Location.loc_end; _} p =
  let pparam_loc = {Location.loc_start; loc_end; loc_ghost= true} in
  {pparam_desc= p; pparam_loc}

let check_local_attr_and_reloc_cmts cmts attrs loc =
  match
    List.partition_tf attrs ~f:(fun attr ->
        Conf.is_jane_street_local_annotation "local" ~test:attr.attr_name.txt )
  with
  | [local_attr], rest ->
      Cmts.relocate_all_to_after cmts ~src:local_attr.attr_loc ~after:loc ;
      (rest, true)
  | _, _ -> (attrs, false)

(* This function pulls apart an arrow type, pulling out local attributes into
   bools and producing a context without those attributes. This addresses the
   problem that we need to remove the local attributes so that they can be
   printed specially, and that the context needs to be updated to reflect
   this to pass some internal ocamlformat sanity checks. It's not the
   cleanest solution in a vacuum, but is perhaps the one that will cause the
   fewest merge conflicts in the future. *)
let decompose_arrow cmts ctx ctl (ct2, m2) =
  let pull_out_local ap =
    let ptyp_attributes, local =
      check_local_attr_and_reloc_cmts cmts ap.pap_type.ptyp_attributes
        ap.pap_type.ptyp_loc
    in
    ({ap with pap_type= {ap.pap_type with ptyp_attributes}}, local)
  in
  let args = List.map ~f:pull_out_local ctl in
  let ((res_ap, _) as res) =
    let ptyp_attributes, local =
      check_local_attr_and_reloc_cmts cmts ct2.ptyp_attributes ct2.ptyp_loc
    in
    let ap =
      { pap_label= Nolabel
      ; pap_loc= ct2.ptyp_loc
      ; pap_type= {ct2 with ptyp_attributes}
      ; pap_modes= m2 }
    in
    (ap, local)
  in
  let ctx_typ =
    Ptyp_arrow (List.map ~f:fst args, res_ap.pap_type, res_ap.pap_modes)
  in
  let ctx =
    match ctx with
    | Typ cty -> Typ {cty with ptyp_desc= ctx_typ}
    | _ -> assert false
  in
  (args, res, ctx)

let fun_ cmts ?(will_keep_first_ast_node = true) xexp =
  let rec fun_ ?(will_keep_first_ast_node = false) ({ast= exp; _} as xexp) =
    let ctx = Exp exp in
    let {pexp_desc; pexp_loc; pexp_attributes; _} = exp in
    if will_keep_first_ast_node || List.is_empty pexp_attributes then
      match pexp_desc with
      | Pexp_fun (p, body) ->
          if not will_keep_first_ast_node then
            Cmts.relocate cmts ~src:pexp_loc ~before:p.pparam_loc
              ~after:body.pexp_loc ;
          let xargs, xbody = fun_ (sub_exp ~ctx body) in
          (p :: xargs, xbody)
      | Pexp_newtype (name, body) ->
          if not will_keep_first_ast_node then
            Cmts.relocate cmts ~src:pexp_loc ~before:body.pexp_loc
              ~after:body.pexp_loc ;
          let xargs, xbody = fun_ (sub_exp ~ctx body) in
          let xargs =
            match xargs with
            | {pparam_desc= Pparam_newtype names; pparam_loc} :: xargs ->
                let param = Pparam_newtype (name :: names) in
                mk_function_param pexp_loc pparam_loc param :: xargs
            | xargs ->
                let param = Pparam_newtype [name] in
                mk_function_param pexp_loc pexp_loc param :: xargs
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
          let before = pattern.ppat_loc and after = body.pcl_loc in
          if not will_keep_first_ast_node then
            Cmts.relocate cmts ~src:pcl_loc ~before ~after ;
          let xargs, xbody = fun_ (sub_cl ~ctx body) in
          let islocal, pattern =
            match
              check_local_attr_and_reloc_cmts cmts pattern.ppat_attributes
                pattern.ppat_loc
            with
            | _, false -> (false, pattern)
            | ppat_attributes, true -> (true, {pattern with ppat_attributes})
          in
          let param = Pparam_val (islocal, label, default, pattern) in
          (mk_function_param before after param :: xargs, xbody)
      | _ -> ([], xexp)
    else ([], xexp)
  in
  fun_ ~will_keep_first_ast_node xexp

let get_jkind_of_legacy_attr attr =
  match (attr.attr_name.txt, attr.attr_payload) with
  | ("ocaml.immediate64" | "immediate64"), PStr [] ->
      Some (Abbreviation (Location.mknoloc "immediate64"))
  | ("ocaml.immediate" | "immediate"), PStr [] ->
      Some (Abbreviation (Location.mknoloc "immediate"))
  | _ -> None

let rewrite_type_declaration_imm_attr_to_jkind_annot cmts decl =
  let immediate_attrs, remaining_attrs =
    decl.ptype_attributes
    |> List.partition_map ~f:(fun attr ->
           match get_jkind_of_legacy_attr attr with
           | Some jkind -> First (jkind, attr)
           | None -> Second attr )
  in
  match (decl.ptype_jkind, immediate_attrs) with
  | None, [(jkind, attr)] ->
      (* We only do this rewrite if (1.) there's no jkind annotation already
         present and (2.) only one immediate attribute is attached *)
      let ptype_jkind = Some Location.(mknoloc jkind) in
      Cmts.relocate_all_to_before cmts ~src:attr.attr_name.loc
        ~before:decl.ptype_loc ;
      Cmts.relocate_all_to_before cmts ~src:attr.attr_loc
        ~before:decl.ptype_loc ;
      {decl with ptype_attributes= remaining_attrs; ptype_jkind}
  | _ -> decl

module Exp = struct
  let infix cmts prec xexp =
    let assoc = Option.value_map prec ~default:Assoc.Non ~f:Assoc.of_prec in
    let rec infix_ ?(child_expr = true) xop xexp =
      let ctx = Exp xexp.ast in
      match (assoc, xexp.ast) with
      | _, {pexp_attributes= _ :: _; _} when child_expr ->
          (* Avoid dropping attributes on child expressions, e.g. [(a + b)
             [@attr] + c] *)
          [(xop, xexp)]
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
          if child_expr then
            Cmts.relocate cmts ~src ~before ~after:e2.pexp_loc ;
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
          if child_expr then Cmts.relocate cmts ~src ~before ~after ;
          (xop, sub_exp ~ctx e1) :: op_args2
      | _ -> [(xop, xexp)]
    in
    infix_ None ~child_expr:false xexp
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

module Let_binding = struct
  type t =
    { lb_op: string loc
    ; lb_pat: pattern xt
    ; lb_args: function_param list
    ; lb_typ: value_constraint option
    ; lb_modes: modes
    ; lb_exp: expression xt
    ; lb_pun: bool
    ; lb_attrs: attribute list
    ; lb_local: bool
    ; lb_modes_binding: modes
    ; lb_loc: Location.t }

  let split_annot cmts xargs ({ast= body; _} as xbody) =
    let ctx = Exp body in
    match body.pexp_desc with
    | Pexp_constraint (exp, Some typ, modes)
      when Source.type_constraint_is_first typ exp.pexp_loc ->
        Cmts.relocate cmts ~src:body.pexp_loc ~before:exp.pexp_loc
          ~after:exp.pexp_loc ;
        let exp_ctx =
          (* The type constraint is moved to the pattern, so we need to
             replace the context from [Pexp_constraint] to [Pexp_fun]. This
             won't be necessary once the normalization is moved to
             [Extended_ast]. *)
          let pat = Ast_helper.Pat.any () in
          let param =
            { pparam_desc= Pparam_val (false, Nolabel, None, pat)
            ; pparam_loc= pat.ppat_loc }
          in
          Exp (Ast_helper.Exp.fun_ param exp)
        in
        ( xargs
        , Some (Pvc_constraint {locally_abstract_univars= []; typ})
        , modes
        , sub_exp ~ctx:exp_ctx exp )
    (* The type constraint is always printed before the declaration for
       functions, for other value bindings we preserve its position. *)
    | Pexp_constraint (exp, Some typ, modes) when not (List.is_empty xargs)
      ->
        Cmts.relocate cmts ~src:body.pexp_loc ~before:exp.pexp_loc
          ~after:exp.pexp_loc ;
        ( xargs
        , Some (Pvc_constraint {locally_abstract_univars= []; typ})
        , modes
        , sub_exp ~ctx exp )
    | Pexp_constraint (exp, None, modes) when not (List.is_empty xargs) ->
        Cmts.relocate cmts ~src:body.pexp_loc ~before:exp.pexp_loc
          ~after:exp.pexp_loc ;
        (xargs, None, modes, sub_exp ~ctx exp)
    | Pexp_coerce (exp, typ1, typ2)
      when Source.type_constraint_is_first typ2 exp.pexp_loc ->
        Cmts.relocate cmts ~src:body.pexp_loc ~before:exp.pexp_loc
          ~after:exp.pexp_loc ;
        ( xargs
        , Some (Pvc_coercion {ground= typ1; coercion= typ2})
        , []
        , sub_exp ~ctx exp )
    | _ -> (xargs, None, [], xbody)

  let split_fun_args cmts xpat xbody =
    let xargs, xbody =
      match xpat.ast with
      | {ppat_desc= Ppat_var _; ppat_attributes= []; _} ->
          fun_ cmts ~will_keep_first_ast_node:false xbody
      | _ -> ([], xbody)
    in
    match (xbody.ast.pexp_desc, xpat.ast.ppat_desc) with
    | Pexp_constraint _, Ppat_constraint _ -> (xargs, None, [], xbody)
    | _ -> split_annot cmts xargs xbody

  let type_cstr cmts ~ctx pvb_pat pvb_expr =
    let lb_pat = sub_pat ~ctx pvb_pat and lb_exp = sub_exp ~ctx pvb_expr in
    let ({ast= pat; _} as xpat) = sub_pat ~ctx lb_pat.ast in
    let pat_is_extension {ppat_desc; _} =
      match ppat_desc with Ppat_extension _ -> true | _ -> false
    in
    let xbody = sub_exp ~ctx lb_exp.ast in
    let pat, xargs, typ, mode, exp =
      if
        (not (List.is_empty xbody.ast.pexp_attributes))
        || pat_is_extension pat
      then (xpat, [], None, [], xbody)
      else
        let xpat =
          match xpat.ast.ppat_desc with
          | Ppat_constraint (p, Some {ptyp_desc= Ptyp_poly ([], _); _}, [])
            ->
              sub_pat ~ctx:xpat.ctx p
          | _ -> xpat
        in
        let xargs, typ, mode, xbody = split_fun_args cmts xpat xbody in
        (xpat, xargs, typ, mode, xbody)
    in
    (pat, xargs, typ, mode, exp)

  let should_desugar_args pat typ =
    match (pat.ast, typ) with
    | {ppat_desc= Ppat_var _; ppat_attributes= []; _}, None -> true
    | _ -> false

  let of_let_binding cmts ~ctx ~first
      { pvb_pat
      ; pvb_expr
      ; pvb_constraint
      ; pvb_is_pun
      ; pvb_attributes
      ; pvb_loc
      ; pvb_modes
      ; pvb_local } =
    let lb_pat = sub_pat ~ctx pvb_pat
    and lb_exp = sub_exp ~ctx pvb_expr
    and lb_typ = pvb_constraint in
    let (lb_args, lb_typ, lb_modes, lb_exp), lb_modes_binding =
      if should_desugar_args lb_pat lb_typ then
        (split_fun_args cmts lb_pat lb_exp, pvb_modes)
      else (([], lb_typ, pvb_modes, lb_exp), [])
    in
    { lb_op= Location.{txt= (if first then "let" else "and"); loc= none}
    ; lb_pat
    ; lb_args
    ; lb_typ
    ; lb_modes
    ; lb_exp
    ; lb_pun= pvb_is_pun
    ; lb_attrs= pvb_attributes
    ; lb_local= pvb_local
    ; lb_modes_binding
    ; lb_loc= pvb_loc }

  let of_let_bindings cmts ~ctx =
    List.mapi ~f:(fun i -> of_let_binding cmts ~ctx ~first:(i = 0))

  let of_binding_ops cmts ~ctx bos =
    List.map bos ~f:(fun bo ->
        let lb_pat, lb_args, lb_typ, lb_modes, lb_exp =
          type_cstr cmts ~ctx bo.pbop_pat bo.pbop_exp
        in
        { lb_op= bo.pbop_op
        ; lb_pat
        ; lb_args
        ; lb_typ
        ; lb_modes
        ; lb_exp
        ; lb_pun= bo.pbop_is_pun
        ; lb_attrs= []
        ; lb_local= false
        ; lb_modes_binding= []
        ; lb_loc= bo.pbop_loc } )
end
