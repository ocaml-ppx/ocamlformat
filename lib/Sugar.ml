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
      | _ -> ([], xexp)
    else ([], xexp)
  in
  fun_ ~will_keep_first_ast_node xexp

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
    ; lb_exp: expression xt
    ; lb_pun: bool
    ; lb_attrs: ext_attrs
    ; lb_loc: Location.t }

  let of_let_binding ~ctx ~first vb =
    { lb_op= Location.{txt= (if first then "let" else "and"); loc= none}
    ; lb_pat= sub_pat ~ctx vb.pvb_pat
    ; lb_args= vb.pvb_args
    ; lb_typ= vb.pvb_constraint
    ; lb_exp= sub_exp ~ctx vb.pvb_expr
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
        ; lb_exp= sub_exp ~ctx bo.pbop_exp
        ; lb_pun= bo.pbop_is_pun
        ; lb_attrs= Ast_helper.Attr.empty_ext_attrs
        ; lb_loc= bo.pbop_loc } )
end
