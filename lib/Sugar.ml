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
    ; lb_args: expr_function_param list
    ; lb_typ: value_constraint option
    ; lb_body: function_body xt
    ; lb_pun: bool
    ; lb_attrs: ext_attrs
    ; lb_loc: Location.t }

  let of_let_binding ~ctx ~first vb =
    { lb_op= Location.{txt= (if first then "let" else "and"); loc= none}
    ; lb_pat= sub_pat ~ctx vb.pvb_pat
    ; lb_args= vb.pvb_args
    ; lb_typ= vb.pvb_constraint
    ; lb_body= sub_fun_body ~ctx vb.pvb_body
    ; lb_pun= vb.pvb_is_pun
    ; lb_attrs= vb.pvb_attributes
    ; lb_loc= vb.pvb_loc }

  let of_let_bindings ~ctx =
    List.mapi ~f:(fun i -> of_let_binding ~ctx ~first:(i = 0))

  let of_binding_ops bos =
    List.map bos ~f:(fun bo ->
        let ctx = Bo bo in
        { lb_op= bo.pbop_op
        ; lb_pat= sub_pat ~ctx bo.pbop_pat
        ; lb_args= bo.pbop_args
        ; lb_typ= bo.pbop_typ
        ; lb_body= sub_fun_body ~ctx (Pfunction_body bo.pbop_exp)
        ; lb_pun= bo.pbop_is_pun
        ; lb_attrs= Ast_helper.Attr.empty_ext_attrs
        ; lb_loc= bo.pbop_loc } )
end
