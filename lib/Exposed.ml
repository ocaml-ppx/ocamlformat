open Migrate_ast
open Parsetree

module Left = struct
  let rec core_type typ =
    match typ.ptyp_desc with
    | Ptyp_arrow (_, t, _) -> core_type t
    | Ptyp_tuple l -> core_type (List.hd_exn l)
    | Ptyp_object _ -> true
    | Ptyp_alias (typ, _) -> core_type typ
    | _ -> false
end

module Right = struct
  let list ~elt l = match List.last l with None -> false | Some x -> elt x

  let rec core_type = function
    | {ptyp_attributes= _ :: _; _} -> false
    | {ptyp_desc; _} -> (
      match ptyp_desc with
      | Ptyp_arrow (_, _, t) -> core_type t
      | Ptyp_tuple l -> core_type (List.last_exn l)
      | Ptyp_object _ -> true
      | _ -> false )

  let extension_constructor = function
    | {pext_kind= Pext_decl (Pcstr_tuple args, None); pext_attributes= []; _}
      -> (
      match List.rev args with
      | last :: _ -> core_type last
      | [] ->
          assert false (* Pext_decl (Pcstr_tuple [], None) does not occur *)
      )
    | _ -> false

  let constructor_declaration = function
    | {pcd_attributes= _ :: _; _} -> false
    | {pcd_res= Some _; _} -> false
    | {pcd_args= Pcstr_record _; _} -> false
    | {pcd_args= Pcstr_tuple args; _} -> list ~elt:core_type args

  let type_declaration = function
    | {ptype_attributes= _ :: _; _} -> false
    | {ptype_cstrs= _ :: _ as cstrs; _} ->
        (* type a = ... constraint left = < ... > *)
        list ~elt:(fun (_left, right, _loc) -> core_type right) cstrs
    | {ptype_kind= Ptype_open | Ptype_record _; _} -> false
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
    | Psig_module _ | Psig_modsubst _ | Psig_recmodule _ | Psig_modtype _
     |Psig_open _ | Psig_include _ | Psig_class _ | Psig_class_type _
     |Psig_attribute _ | Psig_extension _ ->
        false

  let payload = function
    | PStr items -> list ~elt:structure_item items
    | PSig items -> list ~elt:signature_item items
    | PTyp t -> core_type t
    | PPat _ -> false
end
