(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         Alain Frisch, LexiFi                           *)
(*                                                                        *)
(*   Copyright 2012 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* A generic Parsetree mapping class *)

(*
[@@@ocaml.warning "+9"]
  (* Ensure that record patterns don't miss any field. *)
*)

open Parsetree
open Ast_helper
open Location

module String = Misc.Stdlib.String

type mapper = {
  arg_label: mapper -> Asttypes.arg_label -> Asttypes.arg_label;
  attribute: mapper -> attribute -> attribute;
  attributes: mapper -> attribute list -> attribute list;
  ext_attrs: mapper -> ext_attrs -> ext_attrs;
  binding_op: mapper -> binding_op -> binding_op;
  case: mapper -> case -> case;
  cases: mapper -> case list -> case list;
  class_declaration: mapper -> class_declaration -> class_declaration;
  class_description: mapper -> class_description -> class_description;
  class_expr: mapper -> class_expr -> class_expr;
  class_field: mapper -> class_field -> class_field;
  class_signature: mapper -> class_signature -> class_signature;
  class_structure: mapper -> class_structure -> class_structure;
  class_type: mapper -> class_type -> class_type;
  class_type_declaration: mapper -> class_type_declaration
                          -> class_type_declaration;
  class_type_field: mapper -> class_type_field -> class_type_field;
  constant: mapper -> constant -> constant;
  constructor_declaration: mapper -> constructor_declaration
                           -> constructor_declaration;
  expr: mapper -> expression -> expression;
  extension: mapper -> extension -> extension;
  extension_constructor: mapper -> extension_constructor
                         -> extension_constructor;
  include_declaration: mapper -> include_declaration -> include_declaration;
  include_description: mapper -> include_description -> include_description;
  label_declaration: mapper -> label_declaration -> label_declaration;
  location: mapper -> Location.t -> Location.t;
  module_binding: mapper -> module_binding -> module_binding;
  module_declaration: mapper -> module_declaration -> module_declaration;
  module_substitution: mapper -> module_substitution -> module_substitution;
  module_expr: mapper -> module_expr -> module_expr;
  module_type: mapper -> module_type -> module_type;
  module_type_declaration: mapper -> module_type_declaration
                           -> module_type_declaration;
  open_declaration: mapper -> open_declaration -> open_declaration;
  open_description: mapper -> open_description -> open_description;
  pat: mapper -> pattern -> pattern;
  payload: mapper -> payload -> payload;
  signature: mapper -> signature -> signature;
  signature_item: mapper -> signature_item -> signature_item;
  structure: mapper -> structure -> structure;
  structure_item: mapper -> structure_item -> structure_item;
  typ: mapper -> core_type -> core_type;
  type_declaration: mapper -> type_declaration -> type_declaration;
  type_extension: mapper -> type_extension -> type_extension;
  type_exception: mapper -> type_exception -> type_exception;
  type_kind: mapper -> type_kind -> type_kind;
  value_binding: mapper -> value_binding -> value_binding;
  value_bindings: mapper -> value_bindings -> value_bindings;
  value_description: mapper -> value_description -> value_description;
  with_constraint: mapper -> with_constraint -> with_constraint;
  directive_argument: mapper -> directive_argument -> directive_argument;
  toplevel_directive: mapper -> toplevel_directive -> toplevel_directive;
  toplevel_phrase: mapper -> toplevel_phrase -> toplevel_phrase;
  repl_phrase: mapper -> repl_phrase -> repl_phrase;
}

let map_fst f (x, y) = (f x, y)
(*let map_snd f (x, y) = (x, f y)*)
let map_tuple f1 f2 (x, y) = (f1 x, f2 y)
let map_tuple3 f1 f2 f3 (x, y, z) = (f1 x, f2 y, f3 z)
let map_opt f = function None -> None | Some x -> Some (f x)

let map_loc sub {loc; txt} = {loc = sub.location sub loc; txt}

let map_type_var sub (n, l) = map_loc sub n, map_opt (map_loc sub) l

let variant_var sub x =
  {loc = sub.location sub x.loc; txt= map_loc sub x.txt}

let map_package_type sub (lid, l) =
  (map_loc sub lid), (List.map (map_tuple (map_loc sub) (sub.typ sub)) l)

let map_arg_label sub = function
  | Asttypes.Nolabel -> Asttypes.Nolabel
  | Labelled x -> Labelled (map_loc sub x)
  | Optional x -> Optional (map_loc sub x)

module Flag = struct
  open Asttypes

  let map_obj_closed sub = function
    | OClosed -> OClosed
    | OOpen loc -> OOpen (sub.location sub loc)

  let map_private sub = function
    | Private loc -> Private (sub.location sub loc)
    | Public -> Public

  let map_mutable sub = function
    | Mutable loc -> Mutable (sub.location sub loc)
    | Immutable -> Immutable

  let map_virtual sub = function
    | Virtual loc -> Virtual (sub.location sub loc)
    | Concrete -> Concrete

  let map_private_virtual sub { pv_priv; pv_virt } =
    let pv_priv = map_opt (sub.location sub) pv_priv in
    let pv_virt = map_opt (sub.location sub) pv_virt in
    { pv_priv; pv_virt }

  let map_mutable_virtual sub { mv_mut; mv_virt } =
    let mv_mut = map_opt (sub.location sub) mv_mut in
    let mv_virt = map_opt (sub.location sub) mv_virt in
    { mv_mut; mv_virt }
end

module C = struct
  (* Constants *)

  let map sub { pconst_desc; pconst_loc } =
    let loc = sub.location sub pconst_loc in
    let desc =
      match pconst_desc with
      | Pconst_integer _
      | Pconst_char _
      | Pconst_float _ ->
          pconst_desc
      | Pconst_string (s, loc, quotation_delimiter) ->
          Pconst_string (s, sub.location sub loc, quotation_delimiter)

    (* Jane Street extension *)
      | Pconst_unboxed_integer _
      | Pconst_unboxed_float _
          -> pconst_desc
    (* End Jane Street extension *)
    in
    Const.mk ~loc desc
end

module T = struct
  (* Type expressions for the core language *)

  let row_field sub {
      prf_desc;
      prf_loc;
      prf_attributes;
    } =
    let loc = sub.location sub prf_loc in
    let attrs = sub.attributes sub prf_attributes in
    let desc = match prf_desc with
      | Rtag (l, b, tl) -> Rtag (variant_var sub l, b, List.map (sub.typ sub) tl)
      | Rinherit t -> Rinherit (sub.typ sub t)
    in
    Rf.mk ~loc ~attrs desc

  let object_field sub {
      pof_desc;
      pof_loc;
      pof_attributes;
    } =
    let loc = sub.location sub pof_loc in
    let attrs = sub.attributes sub pof_attributes in
    let desc = match pof_desc with
      | Otag (l, t) -> Otag (map_loc sub l, sub.typ sub t)
      | Oinherit t -> Oinherit (sub.typ sub t)
    in
    Of.mk ~loc ~attrs desc

  let map_arrow_param sub {pap_label; pap_loc; pap_type} =
    let pap_label = sub.arg_label sub pap_label in
    let pap_loc = sub.location sub pap_loc in
    let pap_type = sub.typ sub pap_type in
    {pap_label; pap_loc; pap_type}

  let map sub {ptyp_desc = desc; ptyp_loc = loc; ptyp_attributes = attrs} =
    let open Typ in
    let loc = sub.location sub loc in
    let attrs = sub.attributes sub attrs in
    match desc with
    | Ptyp_any -> any ~loc ~attrs ()
    | Ptyp_var s -> var ~loc ~attrs (map_type_var sub s)
    | Ptyp_arrow (params, t2) ->
        arrow ~loc ~attrs (List.map (map_arrow_param sub) params)
          (sub.typ sub t2)
    | Ptyp_tuple tyl ->
        tuple ~loc ~attrs (List.map (fun (lbl, t) -> map_opt (map_loc sub) lbl, sub.typ sub t) tyl)
    | Ptyp_constr (lid, tl) ->
        constr ~loc ~attrs (map_loc sub lid) (List.map (sub.typ sub) tl)
    | Ptyp_object (l, o) ->
        object_ ~loc ~attrs (List.map (object_field sub) l)
          (Flag.map_obj_closed sub o)
    | Ptyp_class (lid, tl) ->
        class_ ~loc ~attrs (map_loc sub lid) (List.map (sub.typ sub) tl)
    | Ptyp_alias (t, s) -> alias ~loc ~attrs (sub.typ sub t) (map_type_var sub s)
    | Ptyp_variant (rl, b, ll) ->
        variant ~loc ~attrs (List.map (row_field sub) rl) b
          (map_opt (List.map (variant_var sub)) ll)
    | Ptyp_poly (sl, t) -> poly ~loc ~attrs
                             (List.map (map_type_var sub) sl) (sub.typ sub t)
    | Ptyp_package pt ->
        let lid, l = map_package_type sub pt in
        package ~loc ~attrs lid l
    | Ptyp_extension x -> extension ~loc ~attrs (sub.extension sub x)

    (* Jane Street extension *)
    | Ptyp_constr_unboxed (lid, tl) ->
        constr_unboxed ~loc ~attrs (map_loc sub lid) (List.map (sub.typ sub) tl)
    (* End Jane Street extension *)

  let map_type_declaration sub
      {ptype_name; ptype_params; ptype_cstrs;
       ptype_kind;
       ptype_private;
       ptype_manifest;
       ptype_attributes;
       ptype_loc;
       ptype_layout;
      } =
    let loc = sub.location sub ptype_loc in
    let attrs = sub.attributes sub ptype_attributes in
    Type.mk ~loc ~attrs (map_loc sub ptype_name)
      ?layout:(ptype_layout)
      ~params:(List.map (map_fst (sub.typ sub)) ptype_params)
      ~priv:(Flag.map_private sub ptype_private)
      ~cstrs:(List.map
                (map_tuple3 (sub.typ sub) (sub.typ sub) (sub.location sub))
                ptype_cstrs)
      ~kind:(sub.type_kind sub ptype_kind)
      ?manifest:(map_opt (sub.typ sub) ptype_manifest)

  let map_type_kind sub = function
    | Ptype_abstract -> Ptype_abstract
    | Ptype_variant l ->
        Ptype_variant (List.map (sub.constructor_declaration sub) l)
    | Ptype_record l -> Ptype_record (List.map (sub.label_declaration sub) l)
    | Ptype_open -> Ptype_open

  let map_constructor_arguments sub = function
    | Pcstr_tuple l -> Pcstr_tuple (List.map (sub.typ sub) l)
    | Pcstr_record (loc, l) ->
        let loc = sub.location sub loc in
        let l = List.map (sub.label_declaration sub) l in
        Pcstr_record (loc, l)

  let map_type_extension sub
      {ptyext_path; ptyext_params;
       ptyext_constructors;
       ptyext_private;
       ptyext_loc;
       ptyext_attributes} =
    let loc = sub.location sub ptyext_loc in
    let attrs = sub.attributes sub ptyext_attributes in
    Te.mk ~loc ~attrs
      (map_loc sub ptyext_path)
      (List.map (sub.extension_constructor sub) ptyext_constructors)
      ~params:(List.map (map_fst (sub.typ sub)) ptyext_params)
      ~priv:(Flag.map_private sub ptyext_private)

  let map_type_exception sub
      {ptyexn_constructor; ptyexn_loc; ptyexn_attributes} =
    let loc = sub.location sub ptyexn_loc in
    let attrs = sub.attributes sub ptyexn_attributes in
    Te.mk_exception ~loc ~attrs
      (sub.extension_constructor sub ptyexn_constructor)

  let map_extension_constructor_kind sub = function
      Pext_decl(vars, ctl, cto) ->
        Pext_decl(List.map (map_type_var sub) vars,
                  map_constructor_arguments sub ctl,
                  map_opt (sub.typ sub) cto)
    | Pext_rebind li ->
        Pext_rebind (map_loc sub li)

  let map_extension_constructor sub
      {pext_name;
       pext_kind;
       pext_loc;
       pext_attributes} =
    let loc = sub.location sub pext_loc in
    let attrs = sub.attributes sub pext_attributes in
    Te.constructor ~loc ~attrs
      (map_loc sub pext_name)
      (map_extension_constructor_kind sub pext_kind)

end

module CT = struct
  (* Type expressions for the class language *)

  let map sub {pcty_loc = loc; pcty_desc = desc; pcty_attributes = attrs} =
    let open Cty in
    let loc = sub.location sub loc in
    let attrs = sub.attributes sub attrs in
    match desc with
    | Pcty_constr (lid, tys) ->
        constr ~loc ~attrs (map_loc sub lid) (List.map (sub.typ sub) tys)
    | Pcty_signature x -> signature ~loc ~attrs (sub.class_signature sub x)
    | Pcty_arrow (params, ct) ->
        arrow ~loc ~attrs (List.map (T.map_arrow_param sub) params)
          (sub.class_type sub ct)
    | Pcty_extension x -> extension ~loc ~attrs (sub.extension sub x)
    | Pcty_open (o, ct) ->
        open_ ~loc ~attrs (sub.open_description sub o) (sub.class_type sub ct)

  let map_field sub {pctf_desc = desc; pctf_loc = loc; pctf_attributes = attrs}
    =
    let open Ctf in
    let loc = sub.location sub loc in
    let attrs = sub.attributes sub attrs in
    match desc with
    | Pctf_inherit ct -> inherit_ ~loc ~attrs (sub.class_type sub ct)
    | Pctf_val (s, mv, t) ->
        val_ ~loc ~attrs (map_loc sub s) (Flag.map_mutable_virtual sub mv)
          (sub.typ sub t)
    | Pctf_method (s, pv, t) ->
        method_ ~loc ~attrs (map_loc sub s) (Flag.map_private_virtual sub pv)
          (sub.typ sub t)
    | Pctf_constraint (t1, t2) ->
        constraint_ ~loc ~attrs (sub.typ sub t1) (sub.typ sub t2)
    | Pctf_attribute x -> attribute ~loc (sub.attribute sub x)
    | Pctf_extension x -> extension ~loc ~attrs (sub.extension sub x)

  let map_signature sub {pcsig_self; pcsig_fields} =
    Csig.mk
      (map_opt (sub.typ sub) pcsig_self)
      (List.map (sub.class_type_field sub) pcsig_fields)
end

let map_functor_param sub {loc; txt} =
  let loc = sub.location sub loc in
  let txt =
    match txt with
    | Unit -> Unit
    | Named (s, mt) -> Named (map_loc sub s, sub.module_type sub mt)
  in
  {loc; txt}

module MT = struct
  (* Type expressions for the module language *)

  let map sub {pmty_desc = desc; pmty_loc = loc; pmty_attributes = attrs} =
    let open Mty in
    let loc = sub.location sub loc in
    let attrs = sub.attributes sub attrs in
    match desc with
    | Pmty_ident s -> ident ~loc ~attrs (map_loc sub s)
    | Pmty_alias s -> alias ~loc ~attrs (map_loc sub s)
    | Pmty_signature sg -> signature ~loc ~attrs (sub.signature sub sg)
    | Pmty_functor (params, mt) ->
        functor_ ~loc ~attrs
          (List.map (map_functor_param sub) params)
          (sub.module_type sub mt)
    | Pmty_gen (arg_loc, mt) ->
        gen ~loc ~attrs
          (sub.location sub arg_loc)
          (sub.module_type sub mt)
    | Pmty_with (mt, l) ->
        with_ ~loc ~attrs (sub.module_type sub mt)
          (List.map (sub.with_constraint sub) l)
    | Pmty_typeof me -> typeof_ ~loc ~attrs (sub.module_expr sub me)
    | Pmty_extension x -> extension ~loc ~attrs (sub.extension sub x)
    | Pmty_strengthen (mt,s) ->
        strengthen ~loc ~attrs
          (sub.module_type sub mt)
          (map_loc sub s)

  let map_with_constraint sub = function
    | Pwith_type (lid, d) ->
        Pwith_type (map_loc sub lid, sub.type_declaration sub d)
    | Pwith_module (lid, lid2) ->
        Pwith_module (map_loc sub lid, map_loc sub lid2)
    | Pwith_modtype (lid, mty) ->
        Pwith_modtype (map_loc sub lid, sub.module_type sub mty)
    | Pwith_typesubst (lid, d) ->
        Pwith_typesubst (map_loc sub lid, sub.type_declaration sub d)
    | Pwith_modsubst (s, lid) ->
        Pwith_modsubst (map_loc sub s, map_loc sub lid)
    | Pwith_modtypesubst (lid, mty) ->
        Pwith_modtypesubst (map_loc sub lid, sub.module_type sub mty)

  let map_signature_item sub {psig_desc = desc; psig_loc = loc} =
    let open Sig in
    let loc = sub.location sub loc in
    match desc with
    | Psig_value vd -> value ~loc (sub.value_description sub vd)
    | Psig_type (rf, l) ->
        type_ ~loc rf (List.map (sub.type_declaration sub) l)
    | Psig_typesubst l ->
        type_subst ~loc (List.map (sub.type_declaration sub) l)
    | Psig_typext te -> type_extension ~loc (sub.type_extension sub te)
    | Psig_exception ed -> exception_ ~loc (sub.type_exception sub ed)
    | Psig_module x -> module_ ~loc (sub.module_declaration sub x)
    | Psig_modsubst x -> mod_subst ~loc (sub.module_substitution sub x)
    | Psig_recmodule l ->
        rec_module ~loc (List.map (sub.module_declaration sub) l)
    | Psig_modtype x -> modtype ~loc (sub.module_type_declaration sub x)
    | Psig_modtypesubst x ->
        modtype_subst ~loc (sub.module_type_declaration sub x)
    | Psig_open x -> open_ ~loc (sub.open_description sub x)
    | Psig_include x -> include_ ~loc (sub.include_description sub x)
    | Psig_class l -> class_ ~loc (List.map (sub.class_description sub) l)
    | Psig_class_type l ->
        class_type ~loc (List.map (sub.class_type_declaration sub) l)
    | Psig_extension (x, attrs) ->
        let attrs = sub.attributes sub attrs in
        extension ~loc ~attrs (sub.extension sub x)
    | Psig_attribute x -> attribute ~loc (sub.attribute sub x)
end


module M = struct
  (* Value expressions for the module language *)

  let map sub {pmod_loc = loc; pmod_desc = desc; pmod_attributes = attrs} =
    let open Mod in
    let loc = sub.location sub loc in
    let attrs = sub.attributes sub attrs in
    match desc with
    | Pmod_ident x -> ident ~loc ~attrs (map_loc sub x)
    | Pmod_structure str -> structure ~loc ~attrs (sub.structure sub str)
    | Pmod_functor (params, body) ->
        functor_ ~loc ~attrs
          (List.map (map_functor_param sub) params)
          (sub.module_expr sub body)
    | Pmod_apply (m1, m2) ->
        apply ~loc ~attrs (sub.module_expr sub m1) (sub.module_expr sub m2)
    | Pmod_apply_unit (me, lc) ->
        apply_unit ~loc ~attrs (sub.module_expr sub me) (sub.location sub lc)
    | Pmod_constraint (m, mty) ->
        constraint_ ~loc ~attrs (sub.module_expr sub m)
                    (sub.module_type sub mty)
    | Pmod_unpack (e, ty1, ty2) ->
        unpack ~loc ~attrs
          (sub.expr sub e)
          (map_opt (map_package_type sub) ty1)
          (map_opt (map_package_type sub) ty2)
    | Pmod_extension x -> extension ~loc ~attrs (sub.extension sub x)
    | Pmod_hole -> hole ~loc ~attrs ()

  let map_structure_item sub {pstr_loc = loc; pstr_desc = desc} =
    let open Str in
    let loc = sub.location sub loc in
    match desc with
    | Pstr_eval (x, attrs) ->
        let attrs = sub.attributes sub attrs in
        eval ~loc ~attrs (sub.expr sub x)
    | Pstr_value lbs -> value ~loc (sub.value_bindings sub lbs)
    | Pstr_primitive vd -> primitive ~loc (sub.value_description sub vd)
    | Pstr_type (rf, l) -> type_ ~loc rf (List.map (sub.type_declaration sub) l)
    | Pstr_typext te -> type_extension ~loc (sub.type_extension sub te)
    | Pstr_exception ed -> exception_ ~loc (sub.type_exception sub ed)
    | Pstr_module x -> module_ ~loc (sub.module_binding sub x)
    | Pstr_recmodule l -> rec_module ~loc (List.map (sub.module_binding sub) l)
    | Pstr_modtype x -> modtype ~loc (sub.module_type_declaration sub x)
    | Pstr_open x -> open_ ~loc (sub.open_declaration sub x)
    | Pstr_class l -> class_ ~loc (List.map (sub.class_declaration sub) l)
    | Pstr_class_type l ->
        class_type ~loc (List.map (sub.class_type_declaration sub) l)
    | Pstr_include x -> include_ ~loc (sub.include_declaration sub x)
    | Pstr_extension (x, attrs) ->
        let attrs = sub.attributes sub attrs in
        extension ~loc ~attrs (sub.extension sub x)
    | Pstr_attribute x -> attribute ~loc (sub.attribute sub x)
end

module E = struct
  (* Value expressions for the core language *)

  let map_function_param sub { pparam_loc = loc; pparam_desc = desc } =
    let loc = sub.location sub loc in
    let desc =
      match desc with
      | Pparam_val (islocal, lab, def, p) ->
          Pparam_val
            (islocal,
             sub.arg_label sub lab,
             map_opt (sub.expr sub) def,
             sub.pat sub p)
      | Pparam_newtype ty ->
          Pparam_newtype (List.map (map_type_var sub) ty)
    in
    { pparam_loc = loc; pparam_desc = desc }

  let map_constraint sub c =
    match c with
    | Pconstraint ty -> Pconstraint (sub.typ sub ty)
    | Pcoerce (ty1, ty2) -> Pcoerce (map_opt (sub.typ sub) ty1, sub.typ sub ty2)

  let map_if_branch sub {if_cond; if_body; if_attrs} =
    let if_cond = sub.expr sub if_cond in
    let if_body = sub.expr sub if_body in
    let if_attrs = sub.attributes sub if_attrs in
    { if_cond; if_body; if_attrs }

  let map sub {pexp_loc = loc; pexp_desc = desc; pexp_attributes = attrs} =
    let open Exp in
    let loc = sub.location sub loc in
    let attrs = sub.attributes sub attrs in
    match desc with
    | Pexp_ident x -> ident ~loc ~attrs (map_loc sub x)
    | Pexp_constant x -> constant ~loc ~attrs (sub.constant sub x)
    | Pexp_let (lbs, e) ->
        let_ ~loc ~attrs (sub.value_bindings sub lbs)
          (sub.expr sub e)
    | Pexp_fun (p, e) ->
        fun_ ~loc ~attrs
          (map_function_param sub p)
          (sub.expr sub e)
    | Pexp_function pel -> function_ ~loc ~attrs (sub.cases sub pel)
    | Pexp_apply (e, l) ->
        apply ~loc ~attrs
          (sub.expr sub e)
          (List.map (map_tuple (sub.arg_label sub) (sub.expr sub)) l)
    | Pexp_match (e, pel) ->
        match_ ~loc ~attrs (sub.expr sub e) (sub.cases sub pel)
    | Pexp_try (e, pel) -> try_ ~loc ~attrs (sub.expr sub e) (sub.cases sub pel)
    | Pexp_tuple el ->
        tuple ~loc ~attrs (List.map (fun (lbl, e) -> map_opt (map_loc sub) lbl, sub.expr sub e) el)
    | Pexp_construct (lid, arg) ->
        construct ~loc ~attrs (map_loc sub lid) (map_opt (sub.expr sub) arg)
    | Pexp_variant (lab, eo) ->
        variant ~loc ~attrs (variant_var sub lab) (map_opt (sub.expr sub) eo)
    | Pexp_record (l, eo) ->
        let fields =
          List.map
            (map_tuple3
               (map_loc sub)
               (map_opt (map_constraint sub))
               (map_opt (sub.expr sub)))
            l
        in
        record ~loc ~attrs fields (map_opt (sub.expr sub) eo)
    | Pexp_field (e, lid) ->
        field ~loc ~attrs (sub.expr sub e) (map_loc sub lid)
    | Pexp_setfield (e1, lid, e2) ->
        setfield ~loc ~attrs (sub.expr sub e1) (map_loc sub lid)
          (sub.expr sub e2)
    | Pexp_array el -> array ~loc ~attrs (List.map (sub.expr sub) el)
    | Pexp_list el -> list ~loc ~attrs (List.map (sub.expr sub) el)
    | Pexp_ifthenelse (eN, e2) ->
        ifthenelse ~loc ~attrs (List.map (map_if_branch sub) eN)
          (map_opt (sub.expr sub) e2)
    | Pexp_sequence (e1, e2) ->
        sequence ~loc ~attrs (sub.expr sub e1) (sub.expr sub e2)
    | Pexp_while (e1, e2) ->
        while_ ~loc ~attrs (sub.expr sub e1) (sub.expr sub e2)
    | Pexp_for (p, e1, e2, d, e3) ->
        for_ ~loc ~attrs (sub.pat sub p) (sub.expr sub e1) (sub.expr sub e2) d
          (sub.expr sub e3)
    | Pexp_coerce (e, t1, t2) ->
        coerce ~loc ~attrs (sub.expr sub e) (map_opt (sub.typ sub) t1)
          (sub.typ sub t2)
    | Pexp_constraint (e, t) ->
        constraint_ ~loc ~attrs (sub.expr sub e) (sub.typ sub t)
    | Pexp_send (e, s) ->
        send ~loc ~attrs (sub.expr sub e) (map_loc sub s)
    | Pexp_new lid -> new_ ~loc ~attrs (map_loc sub lid)
    | Pexp_setinstvar (s, e) ->
        setinstvar ~loc ~attrs (map_loc sub s) (sub.expr sub e)
    | Pexp_indexop_access {pia_lhs; pia_kind; pia_paren; pia_rhs} ->
        let pia_kind =
          match pia_kind with
          | Builtin idx -> Builtin (sub.expr sub idx)
          | Dotop (path, op, idx) ->
              Dotop(map_opt (map_loc sub) path, op, List.map (sub.expr sub) idx)
        in
        indexop_access ~loc ~attrs (sub.expr sub pia_lhs) pia_kind pia_paren
          (map_opt (sub.expr sub) pia_rhs)
    | Pexp_override sel ->
        override ~loc ~attrs
          (List.map (map_tuple (map_loc sub) (sub.expr sub)) sel)
    | Pexp_letmodule (s, args, me, e) ->
        letmodule ~loc ~attrs (map_loc sub s)
          (List.map (map_functor_param sub) args)
          (sub.module_expr sub me)
          (sub.expr sub e)
    | Pexp_letexception (cd, e) ->
        letexception ~loc ~attrs
          (sub.extension_constructor sub cd)
          (sub.expr sub e)
    | Pexp_assert e -> assert_ ~loc ~attrs (sub.expr sub e)
    | Pexp_lazy e -> lazy_ ~loc ~attrs (sub.expr sub e)
    | Pexp_poly (e, t) ->
        poly ~loc ~attrs (sub.expr sub e) (map_opt (sub.typ sub) t)
    | Pexp_object cls -> object_ ~loc ~attrs (sub.class_structure sub cls)
    | Pexp_newtype (s, e) ->
        newtype ~loc ~attrs (map_type_var sub s) (sub.expr sub e)
    | Pexp_pack (me, pt) ->
        pack ~loc ~attrs
          (sub.module_expr sub me)
          (map_opt (map_package_type sub) pt)
    | Pexp_open (o, e) -> open_ ~loc ~attrs (map_loc sub o) (sub.expr sub e)
    | Pexp_letopen (o, e) ->
        letopen ~loc ~attrs (sub.open_declaration sub o) (sub.expr sub e)
    | Pexp_letop {let_; ands; body} ->
        letop ~loc ~attrs (sub.binding_op sub let_)
          (List.map (sub.binding_op sub) ands) (sub.expr sub body)
    | Pexp_extension x -> extension ~loc ~attrs (sub.extension sub x)
    | Pexp_unreachable -> unreachable ~loc ~attrs ()
    | Pexp_hole -> hole ~loc ~attrs ()
    | Pexp_beginend e -> beginend ~loc ~attrs (sub.expr sub e)
    | Pexp_parens e -> parens ~loc ~attrs (sub.expr sub e)
    | Pexp_cons l -> cons ~loc ~attrs (List.map (sub.expr sub) l)
    | Pexp_prefix (op, e) ->
        prefix ~loc ~attrs (map_loc sub op) (sub.expr sub e)
    | Pexp_infix (op, e1, e2) ->
        infix ~loc ~attrs (map_loc sub op) (sub.expr sub e1) (sub.expr sub e2)

  let map_binding_op sub {pbop_op; pbop_pat; pbop_exp; pbop_is_pun; pbop_loc} =
    let open Exp in
    let op = map_loc sub pbop_op in
    let pat = sub.pat sub pbop_pat in
    let exp = sub.expr sub pbop_exp in
    let loc = sub.location sub pbop_loc in
    binding_op op pat exp pbop_is_pun loc

end

module PVB = struct
  let map_value_bindings sub { pvbs_bindings; pvbs_rec; pvbs_extension } =
    let pvbs_bindings = List.map (sub.value_binding sub) pvbs_bindings in
    let pvbs_extension = map_opt (map_loc sub) pvbs_extension in
    { pvbs_bindings; pvbs_rec; pvbs_extension }
end

module P = struct
  (* Patterns *)

  let map sub {ppat_desc = desc; ppat_loc = loc; ppat_attributes = attrs} =
    let open Pat in
    let loc = sub.location sub loc in
    let attrs = sub.attributes sub attrs in
    match desc with
    | Ppat_any -> any ~loc ~attrs ()
    | Ppat_var s -> var ~loc ~attrs (map_loc sub s)
    | Ppat_alias (p, s) -> alias ~loc ~attrs (sub.pat sub p) (map_loc sub s)
    | Ppat_constant c -> constant ~loc ~attrs (sub.constant sub c)
    | Ppat_interval (c1, c2) ->
        interval ~loc ~attrs (sub.constant sub c1) (sub.constant sub c2)
    | Ppat_tuple (pl, oc) ->
        tuple ~loc ~attrs (List.map (fun (lbl, p) -> map_opt (map_loc sub) lbl, sub.pat sub p) pl) oc
    | Ppat_construct (l, p) ->
        construct ~loc ~attrs (map_loc sub l)
          (map_opt
             (fun (vl, p) -> List.map (map_loc sub) vl, sub.pat sub p)
             p)
    | Ppat_variant (l, p) ->
        variant ~loc ~attrs (variant_var sub l) (map_opt (sub.pat sub) p)
    | Ppat_record (lpl, cf) ->
        let fields =
          List.map
            (map_tuple3
               (map_loc sub)
               (map_opt (sub.typ sub))
               (map_opt (sub.pat sub)))
            lpl
        in
        record ~loc ~attrs fields (Flag.map_obj_closed sub cf)
    | Ppat_array pl -> array ~loc ~attrs (List.map (sub.pat sub) pl)
    | Ppat_list pl -> list ~loc ~attrs (List.map (sub.pat sub) pl)
    | Ppat_or pl -> or_ ~loc ~attrs (List.map (sub.pat sub) pl)
    | Ppat_constraint (p, t) ->
        constraint_ ~loc ~attrs (sub.pat sub p) (sub.typ sub t)
    | Ppat_type s -> type_ ~loc ~attrs (map_loc sub s)
    | Ppat_lazy p -> lazy_ ~loc ~attrs (sub.pat sub p)
    | Ppat_unpack (s, pt) ->
        unpack ~loc ~attrs (map_loc sub s) (map_opt (map_package_type sub) pt)
    | Ppat_open (lid,p) -> open_ ~loc ~attrs (map_loc sub lid) (sub.pat sub p)
    | Ppat_exception p -> exception_ ~loc ~attrs (sub.pat sub p)
    | Ppat_extension x -> extension ~loc ~attrs (sub.extension sub x)
    | Ppat_cons pl -> cons ~loc ~attrs (List.map (sub.pat sub) pl)
end

module CE = struct
  (* Value expressions for the class language *)

  let map sub {pcl_loc = loc; pcl_desc = desc; pcl_attributes = attrs} =
    let open Cl in
    let loc = sub.location sub loc in
    let attrs = sub.attributes sub attrs in
    match desc with
    | Pcl_constr (lid, tys) ->
        constr ~loc ~attrs (map_loc sub lid) (List.map (sub.typ sub) tys)
    | Pcl_structure s ->
        structure ~loc ~attrs (sub.class_structure sub s)
    | Pcl_fun (lab, e, p, ce) ->
        fun_ ~loc ~attrs
          (sub.arg_label sub lab)
          (map_opt (sub.expr sub) e)
          (sub.pat sub p)
          (sub.class_expr sub ce)
    | Pcl_apply (ce, l) ->
        apply ~loc ~attrs (sub.class_expr sub ce)
          (List.map (map_tuple (sub.arg_label sub) (sub.expr sub)) l)
    | Pcl_let (lbs, ce) ->
        let_ ~loc ~attrs (sub.value_bindings sub lbs)
          (sub.class_expr sub ce)
    | Pcl_constraint (ce, ct) ->
        constraint_ ~loc ~attrs (sub.class_expr sub ce) (sub.class_type sub ct)
    | Pcl_extension x -> extension ~loc ~attrs (sub.extension sub x)
    | Pcl_open (o, ce) ->
        open_ ~loc ~attrs (sub.open_description sub o) (sub.class_expr sub ce)

  let map_kind sub = function
    | Cfk_concrete (o, e) -> Cfk_concrete (o, sub.expr sub e)
    | Cfk_virtual t -> Cfk_virtual (sub.typ sub t)

  let map_field sub {pcf_desc = desc; pcf_loc = loc; pcf_attributes = attrs} =
    let open Cf in
    let loc = sub.location sub loc in
    let attrs = sub.attributes sub attrs in
    match desc with
    | Pcf_inherit (o, ce, s) ->
        inherit_ ~loc ~attrs o (sub.class_expr sub ce)
          (map_opt (map_loc sub) s)
    | Pcf_val (s, mv, k) ->
        val_ ~loc ~attrs (map_loc sub s) (Flag.map_mutable_virtual sub mv)
          (map_kind sub k)
    | Pcf_method (s, pv, k) ->
        method_ ~loc ~attrs (map_loc sub s) (Flag.map_private_virtual sub pv)
          (map_kind sub k)
    | Pcf_constraint (t1, t2) ->
        constraint_ ~loc ~attrs (sub.typ sub t1) (sub.typ sub t2)
    | Pcf_initializer e -> initializer_ ~loc ~attrs (sub.expr sub e)
    | Pcf_attribute x -> attribute ~loc (sub.attribute sub x)
    | Pcf_extension x -> extension ~loc ~attrs (sub.extension sub x)

  let map_structure sub {pcstr_self; pcstr_fields} =
    {
      pcstr_self = map_opt (sub.pat sub) pcstr_self;
      pcstr_fields = List.map (sub.class_field sub) pcstr_fields;
    }

  let class_infos sub f {pci_virt; pci_params = pl; pci_name; pci_expr;
                         pci_loc; pci_attributes} =
    let loc = sub.location sub pci_loc in
    let attrs = sub.attributes sub pci_attributes in
    Ci.mk ~loc ~attrs
     ~virt:(Flag.map_virtual sub pci_virt)
     ~params:(List.map (map_fst (sub.typ sub)) pl)
      (map_loc sub pci_name)
      (f pci_expr)
end

(* Now, a generic AST mapper, to be extended to cover all kinds and
   cases of the OCaml grammar.  The default behavior of the mapper is
   the identity. *)

let default_mapper =
  {
    arg_label = map_arg_label;
    constant = C.map;
    structure = (fun this l -> List.map (this.structure_item this) l);
    structure_item = M.map_structure_item;
    module_expr = M.map;
    signature = (fun this l -> List.map (this.signature_item this) l);
    signature_item = MT.map_signature_item;
    module_type = MT.map;
    with_constraint = MT.map_with_constraint;
    class_declaration =
      (fun this -> CE.class_infos this (this.class_expr this));
    class_expr = CE.map;
    class_field = CE.map_field;
    class_structure = CE.map_structure;
    class_type = CT.map;
    class_type_field = CT.map_field;
    class_signature = CT.map_signature;
    class_type_declaration =
      (fun this -> CE.class_infos this (this.class_type this));
    class_description =
      (fun this -> CE.class_infos this (this.class_type this));
    type_declaration = T.map_type_declaration;
    type_kind = T.map_type_kind;
    typ = T.map;
    type_extension = T.map_type_extension;
    type_exception = T.map_type_exception;
    extension_constructor = T.map_extension_constructor;
    value_description =
      (fun this {pval_name; pval_type; pval_prim; pval_loc;
                 pval_attributes} ->
        Val.mk
          (map_loc this pval_name)
          (this.typ this pval_type)
          ~attrs:(this.attributes this pval_attributes)
          ~loc:(this.location this pval_loc)
          ~prim:(List.map (map_loc this) pval_prim)
      );

    pat = P.map;
    expr = E.map;
    binding_op = E.map_binding_op;

    module_declaration =
      (fun this {pmd_name; pmd_args; pmd_type; pmd_ext_attrs; pmd_loc} ->
         Md.mk
           (map_loc this pmd_name)
           (List.map (map_functor_param this) pmd_args)
           (this.module_type this pmd_type)
           ~attrs:(this.ext_attrs this pmd_ext_attrs)
           ~loc:(this.location this pmd_loc)
      );

    module_substitution =
      (fun this
        { pms_name; pms_manifest; pms_ext_attrs;
           pms_loc } ->
         Ms.mk
           (map_loc this pms_name)
           (map_loc this pms_manifest)
           ~attrs:(this.ext_attrs this pms_ext_attrs)
           ~loc:(this.location this pms_loc)
      );

    module_type_declaration =
      (fun this {pmtd_name; pmtd_type; pmtd_ext_attrs; pmtd_loc} ->
         Mtd.mk
           (map_loc this pmtd_name)
           ?typ:(map_opt (this.module_type this) pmtd_type)
           ~attrs:(this.ext_attrs this pmtd_ext_attrs)
           ~loc:(this.location this pmtd_loc)
      );

    module_binding =
      (fun this {pmb_name; pmb_args; pmb_expr; pmb_ext_attrs; pmb_loc} ->
         Mb.mk (map_loc this pmb_name)
           (List.map (map_functor_param this) pmb_args)
           (this.module_expr this pmb_expr)
           ~attrs:(this.ext_attrs this pmb_ext_attrs)
           ~loc:(this.location this pmb_loc)
      );

    open_declaration =
      (fun this {popen_expr; popen_override; popen_attributes; popen_loc} ->
         Opn.mk (this.module_expr this popen_expr)
           ~override:popen_override
           ~loc:(this.location this popen_loc)
           ~attrs:(this.attributes this popen_attributes)
      );

    open_description =
      (fun this {popen_expr; popen_override; popen_attributes; popen_loc} ->
         Opn.mk (map_loc this popen_expr)
           ~override:popen_override
           ~loc:(this.location this popen_loc)
           ~attrs:(this.attributes this popen_attributes)
      );

    include_description =
      (fun this {pincl_mod; pincl_attributes; pincl_loc} ->
         Incl.mk (this.module_type this pincl_mod)
           ~loc:(this.location this pincl_loc)
           ~attrs:(this.attributes this pincl_attributes)
      );

    include_declaration =
      (fun this {pincl_mod; pincl_attributes; pincl_loc} ->
         Incl.mk (this.module_expr this pincl_mod)
           ~loc:(this.location this pincl_loc)
           ~attrs:(this.attributes this pincl_attributes)
      );

    value_binding =
      (fun this {pvb_pat; pvb_expr; pvb_constraint; pvb_is_pun; pvb_attributes; pvb_loc} ->
         let map_ct (ct:Parsetree.value_constraint) = match ct with
           | Pvc_constraint {locally_abstract_univars=vars; typ} ->
               Pvc_constraint
                 { locally_abstract_univars = List.map (map_type_var this) vars;
                   typ = this.typ this typ
                 }
           | Pvc_coercion { ground; coercion } ->
               Pvc_coercion {
                 ground = Option.map (this.typ this) ground;
                 coercion = this.typ this coercion
               }
         in
         Vb.mk
           (this.pat this pvb_pat)
           (this.expr this pvb_expr)
           ?value_constraint:(Option.map map_ct pvb_constraint)
           ~is_pun:pvb_is_pun
           ~loc:(this.location this pvb_loc)
           ~attrs:(this.attributes this pvb_attributes)
      );
    value_bindings = PVB.map_value_bindings;

    constructor_declaration =
      (fun this {pcd_name; pcd_vars; pcd_args;
                 pcd_res; pcd_loc; pcd_attributes} ->
        Type.constructor
          (map_loc this pcd_name)
          ~vars:(List.map (map_type_var this) pcd_vars)
          ~args:(T.map_constructor_arguments this pcd_args)
          ?res:(map_opt (this.typ this) pcd_res)
          ~loc:(this.location this pcd_loc)
          ~attrs:(this.attributes this pcd_attributes)
      );

    label_declaration =
      (fun this {pld_name; pld_type; pld_loc; pld_mutable; pld_attributes} ->
         Type.field
           (map_loc this pld_name)
           (this.typ this pld_type)
           ~mut:(Flag.map_mutable this pld_mutable)
           ~loc:(this.location this pld_loc)
           ~attrs:(this.attributes this pld_attributes)
      );

    cases = (fun this l -> List.map (this.case this) l);
    case =
      (fun this {pc_lhs; pc_guard; pc_rhs} ->
         {
           pc_lhs = this.pat this pc_lhs;
           pc_guard = map_opt (this.expr this) pc_guard;
           pc_rhs = this.expr this pc_rhs;
         }
      );



    location = (fun _this l -> l);

    extension = (fun this (s, e) -> (map_loc this s, this.payload this e));
    attribute = (fun this a ->
      {
        attr_name = map_loc this a.attr_name;
        attr_payload = this.payload this a.attr_payload;
        attr_loc = this.location this a.attr_loc
      }
    );
    attributes = (fun this l -> List.map (this.attribute this) l);
    ext_attrs = (fun this e ->
        {
          attrs_extension = map_opt (map_loc this) e.attrs_extension;
          attrs_before = this.attributes this e.attrs_before;
          attrs_after = this.attributes this e.attrs_after;
        });
    payload =
      (fun this -> function
         | PStr x -> PStr (this.structure this x)
         | PSig x -> PSig (this.signature this x)
         | PTyp x -> PTyp (this.typ this x)
         | PPat (x, g) -> PPat (this.pat this x, map_opt (this.expr this) g)
      );

    directive_argument =
      (fun this a ->
         { pdira_desc= a.pdira_desc
         ; pdira_loc= this.location this a.pdira_loc} );

    toplevel_directive =
      (fun this d ->
         { pdir_name= map_loc this d.pdir_name
         ; pdir_arg= map_opt (this.directive_argument this) d.pdir_arg
         ; pdir_loc= this.location this d.pdir_loc } );

    toplevel_phrase =
      (fun this -> function
         | Ptop_def s -> Ptop_def (this.structure this s)
         | Ptop_dir d -> Ptop_dir (this.toplevel_directive this d) );

    repl_phrase =
      (fun this p ->
         { prepl_phrase= this.toplevel_phrase this p.prepl_phrase
         ; prepl_output= p.prepl_output } );
  }
