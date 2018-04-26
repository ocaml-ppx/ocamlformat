(**********************************************************************
 *                                                                    *
 *                            OCamlFormat                             *
 *                                                                    *
 *  Copyright (c) 2017-present, Facebook, Inc.  All rights reserved.  *
 *                                                                    *
 *  This source code is licensed under the MIT license found in the   *
 *  LICENSE file in the root directory of this source tree.           *
 *                                                                    *
 **********************************************************************)

(** Normalize abstract syntax trees *)

open Migrate_ast
open Asttypes
open Parsetree
open Ast_helper

let mapper =
  (* remove locations *)
  let location _ _ = Location.none in
  let attribute (m: Ast_mapper.mapper) attr =
    match attr with
    | ( {txt= ("ocaml.doc" | "ocaml.text") as txt; loc}
      , PStr
          [ { pstr_desc=
                Pstr_eval
                  ( { pexp_desc= Pexp_constant (Pconst_string (doc, None))
                    ; pexp_loc
                    ; pexp_attributes }
                  , [] )
            ; pstr_loc } ] ) ->
        (* normalize consecutive whitespace chars to a single space *)
        let doc' =
          String.concat ~sep:" "
            (List.filter ~f:(Fn.non String.is_empty)
               (String.split_on_chars doc
                  ~on:['\t'; '\n'; '\011'; '\012'; '\r'; ' ']))
        in
        ( {txt; loc= m.location m loc}
        , m.payload m
            (PStr
               [ { pstr_desc=
                     Pstr_eval
                       ( { pexp_desc=
                             Pexp_constant (Pconst_string (doc', None))
                         ; pexp_loc= m.location m pexp_loc
                         ; pexp_attributes= m.attributes m pexp_attributes
                         }
                       , [] )
                 ; pstr_loc= m.location m pstr_loc } ]) )
    | attr -> Ast_mapper.default_mapper.attribute m attr
  in
  (* sort attributes *)
  let attributes (m: Ast_mapper.mapper) atrs =
    Ast_mapper.default_mapper.attributes m
      (List.sort ~compare:Poly.compare atrs)
  in
  let expr (m: Ast_mapper.mapper) exp =
    let {pexp_desc; pexp_loc; pexp_attributes} = exp in
    match pexp_desc with
    (* convert [(f x) y] to [f x y] *)
    | Pexp_apply ({pexp_desc= Pexp_apply (f, xs)}, ys) ->
        m.expr m
          (Exp.apply ~loc:pexp_loc ~attrs:pexp_attributes f (xs @ ys))
    (* convert [~- int_const] to [-int_const] *)
    | Pexp_apply
        ( { pexp_desc= Pexp_ident {txt= Lident "~-"}
          ; pexp_loc
          ; pexp_attributes= atrs0 }
        , [ ( _
            , { pexp_desc= Pexp_constant (Pconst_integer (lit, suf))
              ; pexp_attributes= atrs1 } ) ] ) ->
        m.expr m
          (Exp.constant ~loc:pexp_loc ~attrs:(atrs0 @ atrs1)
             (Pconst_integer ("-" ^ lit, suf)))
    (* convert [~-] ident to [-] *)
    | Pexp_apply
        ( ( { pexp_desc= Pexp_ident ({txt= Lident "~-"} as lid)
            ; pexp_loc
            ; pexp_attributes } as e1 )
        , e1N ) ->
        m.expr m
          (Exp.apply ~loc:pexp_loc ~attrs:pexp_attributes
             {e1 with pexp_desc= Pexp_ident {lid with txt= Lident "-"}}
             e1N)
    (* convert [(c1; c2); c3] to [c1; (c2; c3)] *)
    | Pexp_sequence
        ({pexp_desc= Pexp_sequence (e1, e2); pexp_attributes= []}, e3) ->
        m.expr m
          (Exp.sequence e1 (Exp.sequence ~attrs:pexp_attributes e2 e3))
    | _ -> Ast_mapper.default_mapper.expr m exp
  in
  let pat (m: Ast_mapper.mapper) pat =
    let {ppat_desc; ppat_loc= loc1; ppat_attributes= attrs1} = pat in
    (* normalize nested or patterns *)
    match ppat_desc with
    | Ppat_or
        ( pat1
        , { ppat_desc= Ppat_or (pat2, pat3)
          ; ppat_loc= loc2
          ; ppat_attributes= attrs2 } ) ->
        m.pat m
          (Pat.or_ ~loc:loc1 ~attrs:attrs1
             (Pat.or_ ~loc:loc2 ~attrs:attrs2 pat1 pat2)
             pat3)
    | _ -> Ast_mapper.default_mapper.pat m pat
  in
  let value_binding (m: Ast_mapper.mapper) vb =
    let { pvb_pat= {ppat_desc; ppat_loc; ppat_attributes}
        ; pvb_expr
        ; pvb_loc
        ; pvb_attributes } =
      vb
    in
    match (ppat_desc, pvb_expr.pexp_desc) with
    (* recognize and undo the pattern of code introduced by
       ocaml/ocaml@fd0dc6a0fbf73323c37a73ea7e8ffc150059d6ff to fix
       https://caml.inria.fr/mantis/view.php?id=7344 *)
    | ( Ppat_constraint
          (({ppat_desc= Ppat_var _} as p0), {ptyp_desc= Ptyp_poly ([], t0)})
      , Pexp_constraint (e0, t1) )
      when Poly.equal t0 t1 ->
        m.value_binding m
          (Vb.mk ~loc:pvb_loc ~attrs:pvb_attributes p0
             (Exp.constraint_ ~loc:ppat_loc ~attrs:ppat_attributes e0 t0))
    (* convert [let (x : t) = e] to [let x = (e : t)] *)
    | Ppat_constraint (p0, t0), _ ->
        m.value_binding m
          (Vb.mk ~loc:pvb_loc ~attrs:pvb_attributes p0
             (Exp.constraint_ ~loc:ppat_loc ~attrs:ppat_attributes pvb_expr
                t0))
    | _ -> Ast_mapper.default_mapper.value_binding m vb
  in
  let structure_item (m: Ast_mapper.mapper) (si: structure_item) =
    match si.pstr_desc with
    | Pstr_eval ({pexp_desc= Pexp_extension e}, []) ->
        let e = m.extension m e in
        let pstr_loc = m.location m si.pstr_loc in
        {pstr_desc= Pstr_extension (e, []); pstr_loc}
    | _ -> Ast_mapper.default_mapper.structure_item m si
  in
  { Ast_mapper.default_mapper with
    location
  ; attribute
  ; attributes
  ; expr
  ; pat
  ; value_binding
  ; structure_item }

let impl = map_structure mapper

let intf = map_signature mapper

let equal_impl ast1 ast2 = Poly.equal (impl ast1) (impl ast2)

let equal_intf ast1 ast2 = Poly.equal (intf ast1) (intf ast2)
