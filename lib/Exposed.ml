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

open Extended_ast

module Left_angle = struct
  let rec core_type typ =
    match typ.ptyp_desc with
    | Ptyp_arrow (t :: _, _, _) -> core_type t.pap_type
    | Ptyp_tuple l -> (
      match List.hd_exn l with
      | Some _, _ -> false
      | None, typ -> core_type typ )
    | Ptyp_object _ -> true
    | Ptyp_alias (typ, _) -> core_type typ
    | _ -> false
end

module Right_angle = struct
  let list ~elt l = match List.last l with None -> false | Some x -> elt x

  let rec core_type = function
    | {ptyp_attributes= _ :: _; _} -> false
    | {ptyp_desc; _} -> (
      match ptyp_desc with
      | Ptyp_arrow (_, t, []) -> core_type t
      | Ptyp_tuple l -> (
        match List.last_exn l with
        | Some _, _ -> false
        | None, typ -> core_type typ )
      | Ptyp_object _ -> true
      | _ -> false )

  let constructor_arguments = function
    | Pcstr_record _ -> false
    | Pcstr_tuple args -> (
      match List.last args with
      | Some {pca_modalities= _ :: _; _} ->
          (* Modalities are the right-most part of a construct argument:

             type a = A of t * u @@ modality *)
          false
      | Some {pca_type= {ptyp_desc= Ptyp_arrow _; _}; _} ->
          (* Arrows are wrapped in parens in this position:

             type a = A of (t -> <..>) *)
          false
      | Some {pca_type; _} -> core_type pca_type
      | None -> false )

  let extension_constructor = function
    | {pext_attributes= _ :: _; _} -> false
    | {pext_kind; _} -> (
      match pext_kind with
      | Pext_rebind _ -> false
      | Pext_decl (_, _, Some _result) -> false
      | Pext_decl (_, args, None) -> constructor_arguments args )

  let constructor_declaration = function
    | {pcd_attributes= _ :: _; _} -> false
    | {pcd_res= Some _; _} -> false
    | {pcd_args= args; _} -> constructor_arguments args

  let rec jkind ({txt= jk; _} : jkind_annotation Asttypes.loc) =
    match jk with
    | Default | Abbreviation _ | Mod _ -> false
    | With (_, t, _) | Kind_of t -> core_type t
    | Product jks -> list ~elt:jkind jks

  let type_declaration = function
    | {ptype_attributes= _ :: _; _} -> false
    | {ptype_jkind= Some jk; _} -> jkind jk
    | {ptype_cstrs= _ :: _ as cstrs; _} ->
        (* type a = ... constraint left = < ... > *)
        list ~elt:(fun (_left, right, _loc) -> core_type right) cstrs
    | { ptype_kind=
          Ptype_open | Ptype_record _ | Ptype_record_unboxed_product _
      ; _ } ->
        false
    | {ptype_kind= Ptype_abstract; ptype_manifest= None; _} -> false
    | {ptype_kind= Ptype_abstract; ptype_manifest= Some manifest; _} ->
        (* type a = < ... > *)
        core_type manifest
    | {ptype_kind= Ptype_variant cdecls; _} ->
        (* type a = ... | C of < ... > *)
        list ~elt:constructor_declaration cdecls

  let type_extension = function
    | {ptyext_attributes= _ :: _; _} -> false
    (* type a += A of ... * ... * < ... > *)
    | {ptyext_constructors; _} ->
        list ~elt:extension_constructor ptyext_constructors

  let label_declaration = function
    | {pld_attributes= _ :: _; _} -> false
    | {pld_type; _} -> core_type pld_type

  let row_field = function
    | {prf_attributes= _ :: _; _} -> false
    | {prf_desc= Rinherit _; _} -> false
    | {prf_desc= Rtag (_, _, cs); _} -> (
      match List.last cs with None -> false | Some x -> core_type x )

  (* exception C of ... * ... * < ... > *)
  let type_exception = function
    | {ptyexn_attributes= _ :: _; _} -> false
    | {ptyexn_constructor; _} -> extension_constructor ptyexn_constructor

  (* val x : < ... > *)
  let value_description = function
    | {pval_attributes= _ :: _; _} -> false
    | {pval_prim= _ :: _; _} -> false
    | {pval_type= ct; _} -> core_type ct

  let structure_item {pstr_desc; pstr_loc= _} =
    match pstr_desc with
    | Pstr_type (_recflag, typedecls) -> list ~elt:type_declaration typedecls
    | Pstr_typext te -> type_extension te
    | Pstr_exception te -> type_exception te
    | Pstr_primitive vd -> value_description vd
    | Pstr_kind_abbrev (_, jk) -> jkind jk
    | Pstr_module _ | Pstr_recmodule _ | Pstr_modtype _ | Pstr_open _
     |Pstr_class _ | Pstr_class_type _ | Pstr_include _ | Pstr_attribute _
     |Pstr_extension _ | Pstr_value _ | Pstr_eval _ ->
        false

  let signature_item {psig_desc; psig_loc= _} =
    match psig_desc with
    | Psig_value vd -> value_description vd
    | Psig_type (_recflag, typedecls) -> list ~elt:type_declaration typedecls
    | Psig_typesubst typedecls -> list ~elt:type_declaration typedecls
    | Psig_typext te -> type_extension te
    | Psig_exception te -> type_exception te
    | Psig_kind_abbrev (_, jk) -> jkind jk
    | Psig_module _ | Psig_modsubst _ | Psig_recmodule _ | Psig_modtype _
     |Psig_modtypesubst _ | Psig_open _ | Psig_include _ | Psig_class _
     |Psig_class_type _ | Psig_attribute _ | Psig_extension _ ->
        false

  let payload = function
    | PStr items -> list ~elt:structure_item items
    | PSig {psg_items; _} -> list ~elt:signature_item psg_items
    | PTyp t -> core_type t
    | PPat _ -> false
end

module Right_square = struct
  let rec expression exp =
    match exp.pexp_attributes with
    | _ :: _ -> true
    | [] -> (
      match exp.pexp_desc with
      | Pexp_let (_, exp) -> expression exp
      | Pexp_function cases -> case (List.last_exn cases)
      | Pexp_fun (_, exp) -> expression exp
      | Pexp_apply (_, args) -> expression (snd (List.last_exn args))
      | Pexp_match (_, (_ :: _ as cases)) -> case (List.last_exn cases)
      | Pexp_try (_, (_ :: _ as cases)) -> case (List.last_exn cases)
      | Pexp_list _ -> true
      | Pexp_cons elems -> expression (List.last_exn elems)
      | Pexp_construct ({txt= Lident "[]"; loc= _}, None) -> true
      | Pexp_construct (_, Some exp) -> expression exp
      | Pexp_variant (_, Some exp) -> expression exp
      | Pexp_array _ -> true
      | Pexp_ifthenelse (if_branch, None) ->
          let last = (List.last_exn if_branch).if_body in
          expression last
      | Pexp_ifthenelse (_, Some exp) -> expression exp
      | Pexp_sequence (_, exp) -> expression exp
      | Pexp_setinstvar (_, exp) -> expression exp
      | Pexp_letmodule (_, _, _, _, exp) -> expression exp
      | Pexp_letexception (_, exp) -> expression exp
      | Pexp_assert exp -> expression exp
      | Pexp_lazy exp -> expression exp
      | Pexp_poly (exp, None) -> expression exp
      | Pexp_newtype (_, exp) -> expression exp
      | Pexp_open (_, exp) -> expression exp
      | Pexp_letop {let_= _; ands= _; body} -> expression body
      | Pexp_extension _ -> true
      | Pexp_stack exp -> expression exp
      | Pexp_list_comprehension _ -> true
      | Pexp_array_comprehension _ -> true
      | _ -> false )

  and case {pc_lhs= _; pc_guard= _; pc_rhs} = expression pc_rhs

  and core_type t =
    match t.ptyp_desc with
    | Ptyp_extension _ -> true
    | Ptyp_variant _ -> true
    | Ptyp_poly (_, t) -> core_type t
    | Ptyp_arrow (_, t, []) -> core_type t
    | _ -> false
end
