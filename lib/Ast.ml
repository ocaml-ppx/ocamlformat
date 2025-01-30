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

(** Abstract syntax tree term *)

open Migrate_ast
open Extended_ast

type cmt_checker =
  { cmts_before: Location.t -> bool
  ; cmts_within: Location.t -> bool
  ; cmts_after: Location.t -> bool }

let cmts_between s {cmts_before; cmts_after; _} loc1 loc2 =
  (cmts_after loc1 && Source.ends_line s loc1) || cmts_before loc2

let ( init
    , register_reset
    , leading_nested_match_parens
    , parens_ite
    , ocaml_version
    , ocp_indent_compat ) =
  let l = ref [] in
  let leading_nested_match_parens = ref false in
  let parens_ite = ref false in
  let ocaml_version = ref Ocaml_version.sys_version in
  let ocp_indent_compat = ref false in
  let register f = l := f :: !l in
  let init (conf : Conf.t) =
    leading_nested_match_parens :=
      conf.fmt_opts.leading_nested_match_parens.v ;
    parens_ite := conf.fmt_opts.parens_ite.v ;
    ocaml_version := conf.opr_opts.ocaml_version.v ;
    ocp_indent_compat := conf.fmt_opts.ocp_indent_compat.v ;
    List.iter !l ~f:(fun f -> f ())
  in
  ( init
  , register
  , leading_nested_match_parens
  , parens_ite
  , ocaml_version
  , ocp_indent_compat )

(** [fit_margin c x] returns [true] if and only if [x] does not exceed 1/3 of
    the margin. *)
let fit_margin (c : Conf.t) x = x * 3 < c.fmt_opts.margin.v

(** [longident_fit_margin c x] returns [true] if and only if [x] does not
    exceed 2/3 of the margin. *)
let longident_fit_margin (c : Conf.t) x = x * 3 < c.fmt_opts.margin.v * 2

let longident_is_simple c x =
  let rec length x =
    match x with
    | Longident.Lident x -> String.length x
    | Ldot (x, y) -> length x + 1 + String.length y
    | Lapply (x, y) -> length x + length y + 3
  in
  longident_fit_margin c (length x)

(** 'Classes' of expressions which are parenthesized differently. *)
type cls = Let_match | Match | Non_apply | Sequence | Then | ThenElse

(** Predicates recognizing special symbol identifiers. *)

module Token = struct
  let is_infix = function
    | Parser.AMPERAMPER | AMPERSAND | ANDOP _ | AT | ATAT | BAR | BARBAR
     |COLON | COLONCOLON | COLONEQUAL | DOTDOT | DOTOP _ | EQUAL | GREATER
     |HASHOP _ | INFIXOP0 _ | INFIXOP1 _ | INFIXOP2 _ | INFIXOP3 _
     |INFIXOP4 _ | LESS | LESSMINUS | LETOP _ | MINUS | MINUSDOT
     |MINUSGREATER | PERCENT | PLUS | PLUSDOT | PLUSEQ | SLASH | STAR ->
        true
    | _ -> false
end

module Attr = struct
  module Key = struct
    type t = Regular | Item | Floating

    let to_string = function
      | Regular -> "@"
      | Item -> "@@"
      | Floating -> "@@@"
  end

  let is_doc = function
    | {attr_name= {Location.txt= "ocaml.doc" | "ocaml.text"; _}; _} -> true
    | _ -> false
end

module Ext = struct
  module Key = struct
    type t = Regular | Item

    let to_string = function Regular -> "%" | Item -> "%%"
  end
end

module Ext_attrs = struct
  let has_doc ea =
    List.exists ~f:Attr.is_doc ea.attrs_before
    || List.exists ~f:Attr.is_doc ea.attrs_after
end

module Exp = struct
  let location x = x.pexp_loc

  let test_id ~f = function
    | {pexp_desc= Pexp_ident {txt= i; _}; _} -> f i
    | _ -> false

  let is_prefix = test_id ~f:Std_longident.is_prefix

  let is_infix = test_id ~f:Std_longident.is_infix

  let is_monadic_binding = test_id ~f:Std_longident.is_monadic_binding

  let is_symbol = test_id ~f:Std_longident.is_symbol

  let is_sequence exp =
    match exp.pexp_desc with
    | Pexp_sequence _ -> true
    | Pexp_extension
        ( ext
        , PStr
            [ { pstr_desc=
                  Pstr_eval (({pexp_desc= Pexp_sequence _; _} as e), [])
              ; _ } ] )
      when Source.extension_using_sugar ~name:ext ~payload:e.pexp_loc ->
        true
    | _ -> false

  let has_trailing_attributes {pexp_desc; pexp_attributes; _} =
    match pexp_desc with
    | Pexp_fun _ | Pexp_function _ | Pexp_ifthenelse _ | Pexp_match _
     |Pexp_newtype _ | Pexp_try _ ->
        false
    | _ -> List.exists pexp_attributes ~f:(Fn.non Attr.is_doc)

  let rec is_trivial exp =
    match exp.pexp_desc with
    | Pexp_constant {pconst_desc= Pconst_string (_, _, None); _} -> true
    | Pexp_constant _ | Pexp_field _ | Pexp_unboxed_field _ | Pexp_ident _
     |Pexp_send _ ->
        true
    | Pexp_construct (_, exp) -> Option.for_all exp ~f:is_trivial
    | Pexp_prefix (_, e) -> is_trivial e
    | Pexp_apply
        ({pexp_desc= Pexp_ident {txt= Lident "not"; _}; _}, [(_, e1)]) ->
        is_trivial e1
    | Pexp_variant (_, None) -> true
    | Pexp_array (_, []) | Pexp_list [] -> true
    | Pexp_array (_, [x]) | Pexp_list [x] -> is_trivial x
    | _ -> false

  let rec exposed_left e =
    match e.pexp_desc with
    | Pexp_prefix _ -> true
    | Pexp_apply (op, _) -> exposed_left op
    | Pexp_field (e, _) | Pexp_unboxed_field (e, _) -> exposed_left e
    | _ -> false

  (** [mem_cls cls exp] holds if [exp] is in the named class of expressions
      [cls]. *)
  let mem_cls cls ast =
    match (ast, cls) with
    | {pexp_desc= Pexp_ifthenelse (_, None); _}, (Non_apply | ThenElse)
     |{pexp_desc= Pexp_ifthenelse _; _}, Non_apply
     |( {pexp_desc= Pexp_sequence _; _}
      , (Non_apply | Sequence | Then | ThenElse) )
     |( { pexp_desc=
            ( Pexp_function _ | Pexp_match _ | Pexp_try _
            | Pexp_fun (_, {pexp_desc= Pexp_constraint _; _}) )
        ; _ }
      , (Match | Let_match | Non_apply) )
     |( { pexp_desc=
            ( Pexp_fun _ | Pexp_let _ | Pexp_letop _ | Pexp_letexception _
            | Pexp_letmodule _ | Pexp_newtype _ | Pexp_open _
            | Pexp_letopen _ )
        ; _ }
      , (Let_match | Non_apply) ) ->
        true
    | _ -> false

  (* Jane Street: This is meant to be true if the expression can be parsed by
     the [simple_expr] production in the parser.

     It's OK if this is conservative (returning false for simple exprs will
     just result in extra parens) but bad if it returns true for a non-simple
     expr. *)
  let rec is_simple_in_parser exp =
    match exp.pexp_desc with
    | Pexp_indexop_access {pia_rhs= None; _}
     |Pexp_new _ | Pexp_object _ | Pexp_ident _ | Pexp_constant _
     |Pexp_construct (_, None)
     |Pexp_variant (_, None)
     |Pexp_override _ | Pexp_open _ | Pexp_extension _ | Pexp_hole
     |Pexp_record _ | Pexp_record_unboxed_product _ | Pexp_array _
     |Pexp_list _ | Pexp_list_comprehension _ | Pexp_array_comprehension _
     |Pexp_unboxed_tuple _ ->
        true
    | Pexp_prefix (_, e)
     |Pexp_field (e, _)
     |Pexp_unboxed_field (e, _)
     |Pexp_send (e, _) ->
        is_simple_in_parser e
    | Pexp_infix ({txt; _}, e1, e2) ->
        String.length txt > 0
        && Char.(String.get txt 0 = '#')
        && is_simple_in_parser e1 && is_simple_in_parser e2
    | Pexp_indexop_access {pia_rhs= Some _; _}
     |Pexp_construct (_, Some _)
     |Pexp_variant (_, Some _)
     |Pexp_unreachable | Pexp_let _ | Pexp_function _ | Pexp_fun _
     |Pexp_apply _ | Pexp_match _ | Pexp_try _ | Pexp_tuple _
     |Pexp_setfield _ | Pexp_ifthenelse _ | Pexp_sequence _ | Pexp_while _
     |Pexp_for _ | Pexp_constraint _ | Pexp_coerce _ | Pexp_setinstvar _
     |Pexp_letmodule _ | Pexp_letexception _ | Pexp_assert _ | Pexp_lazy _
     |Pexp_poly _ | Pexp_newtype _ | Pexp_pack _ | Pexp_letopen _
     |Pexp_letop _ | Pexp_stack _ | Pexp_beginend _ | Pexp_parens _
     |Pexp_cons _ ->
        false
end

module Pat = struct
  let location x = x.ppat_loc

  let is_any = function {ppat_desc= Ppat_any; _} -> true | _ -> false

  let is_simple {ppat_desc; _} =
    match ppat_desc with
    | Ppat_any | Ppat_constant _ | Ppat_var _
     |Ppat_variant (_, None)
     |Ppat_construct (_, None) ->
        true
    | (Ppat_variant (_, Some p) | Ppat_construct (_, Some ([], p)))
      when is_any p ->
        true
    | Ppat_cons pl when List.for_all pl ~f:is_any -> true
    | _ -> false

  (* Jane Street: This is meant to be true if the pattern was parsed by the
     [simple_pattern] production in the parser, which is different than the
     above [is_simple] function used for something else.

     It's OK if this is conservative (returning false for simple patterns
     will just result in extra parens) but bad if it returns true for a
     non-simple pattern. *)
  let rec is_simple_in_parser {ppat_desc; _} =
    match ppat_desc with
    | Ppat_var _ | Ppat_record _ | Ppat_record_unboxed_product _
     |Ppat_list _ | Ppat_array _ | Ppat_any | Ppat_constant _
     |Ppat_construct (_, None)
     |Ppat_variant (_, None)
     |Ppat_type _ | Ppat_extension _ | Ppat_unboxed_tuple _ ->
        true
    | Ppat_open (_, p) -> is_simple_in_parser p
    | Ppat_construct (_, Some _)
     |Ppat_variant (_, Some _)
     |Ppat_alias _ | Ppat_interval _ | Ppat_tuple _ | Ppat_or _
     |Ppat_constraint (_, _, _)
     |Ppat_lazy _
     |Ppat_unpack (_, _)
     |Ppat_exception _ | Ppat_cons _ ->
        false

  let has_trailing_attributes {ppat_desc; ppat_attributes; _} =
    match ppat_desc with
    | Ppat_construct (_, None)
     |Ppat_constant _ | Ppat_any | Ppat_var _
     |Ppat_variant (_, None)
     |Ppat_record _ | Ppat_record_unboxed_product _ | Ppat_array _
     |Ppat_list _ | Ppat_type _ | Ppat_unpack _ | Ppat_extension _
     |Ppat_open _ | Ppat_interval _ ->
        false
    | _ -> List.exists ppat_attributes ~f:(Fn.non Attr.is_doc)
end

let doc_atrs ?(acc = []) atrs =
  let docs, rev_atrs =
    List.fold atrs ~init:(acc, []) ~f:(fun (docs, rev_atrs) atr ->
        let open Asttypes in
        match atr with
        | { attr_name=
              { txt= ("ocaml.doc" | "ocaml.text") as txt
              ; loc= {loc_ghost= true; _} }
          ; attr_payload=
              PStr
                [ { pstr_desc=
                      Pstr_eval
                        ( { pexp_desc=
                              Pexp_constant
                                {pconst_desc= Pconst_string (doc, _, None); _}
                          ; pexp_loc= loc
                          ; pexp_attributes= []
                          ; _ }
                        , [] )
                  ; _ } ]
          ; _ } -> (
          match (txt, docs) with
          | "ocaml.doc", (_, false) :: _ ->
              (* cannot put two doc comment next to each other *)
              (docs, atr :: rev_atrs)
          | _ ->
              ( ({txt= doc; loc}, String.equal "ocaml.text" txt) :: docs
              , rev_atrs ) )
        | _ -> (docs, atr :: rev_atrs) )
  in
  let docs = match docs with [] -> None | l -> Some (List.rev l) in
  (docs, List.rev rev_atrs)

let rec mty_is_simple x =
  match x.pmty_desc with
  | Pmty_ident _ | Pmty_alias _ | Pmty_signature [] -> true
  | Pmty_signature (_ :: _)
   |Pmty_with (_, _ :: _ :: _)
   |Pmty_extension _
   |Pmty_functor (_, _)
   |Pmty_strengthen _ ->
      false
  | Pmty_gen (_, t) -> mty_is_simple t
  | Pmty_typeof e -> mod_is_simple e
  | Pmty_with (t, ([] | [_])) -> mty_is_simple t

and mod_is_simple x =
  match x.pmod_desc with
  | Pmod_ident _ | Pmod_unpack _ | Pmod_structure [] | Pmod_hole -> true
  | Pmod_structure (_ :: _) | Pmod_extension _ | Pmod_functor (_, _) -> false
  | Pmod_constraint (e, t) -> mod_is_simple e && mty_is_simple t
  | Pmod_apply (a, b) -> mod_is_simple a && mod_is_simple b
  | Pmod_apply_unit (a, _) -> mod_is_simple a

module Mty = struct
  let is_simple = mty_is_simple

  let has_trailing_attributes {pmty_attributes; _} =
    List.exists pmty_attributes ~f:(Fn.non Attr.is_doc)
end

module Mod = struct
  let is_simple = mod_is_simple

  let has_trailing_attributes {pmod_attributes; _} =
    List.exists pmod_attributes ~f:(Fn.non Attr.is_doc)
end

module Cty = struct
  let rec is_simple x =
    match x.pcty_desc with
    | Pcty_constr _ | Pcty_signature {pcsig_fields= []; _} -> true
    | Pcty_signature {pcsig_fields= _ :: _; _}
     |Pcty_open _ | Pcty_extension _ ->
        false
    | Pcty_arrow (_, t) -> is_simple t
end

module Cl = struct
  let rec is_simple x =
    match x.pcl_desc with
    | Pcl_constr _ | Pcl_structure {pcstr_fields= []; _} -> true
    | Pcl_structure {pcstr_fields= _ :: _; _}
     |Pcl_let _ | Pcl_open _ | Pcl_extension _ ->
        false
    | Pcl_apply (e, _) | Pcl_fun (_, _, _, e) -> is_simple e
    | Pcl_constraint (e, t) -> is_simple e && Cty.is_simple t

  (** [mem_cls cls cl] holds if [cl] is in the named class of expressions
      [cls]. *)
  let mem_cls cls ast =
    match (ast, cls) with
    | {pcl_desc= Pcl_fun _; _}, Non_apply -> true
    | _ -> false
end

module Tyd = struct
  let is_simple x =
    match x.ptype_kind with
    | Ptype_abstract | Ptype_open -> true
    | Ptype_variant _ | Ptype_record _ | Ptype_record_unboxed_product _ ->
        false
end

module Structure_item = struct
  let has_doc itm =
    match itm.pstr_desc with
    | Pstr_attribute atr -> Attr.is_doc atr
    (* one attribute list *)
    | Pstr_eval (_, atrs)
     |Pstr_value {pvbs_bindings= {pvb_attributes= atrs; _} :: _; _}
     |Pstr_primitive {pval_attributes= atrs; _}
     |Pstr_type (_, {ptype_attributes= atrs; _} :: _)
     |Pstr_typext {ptyext_attributes= atrs; _}
     |Pstr_recmodule ({pmb_expr= {pmod_attributes= atrs; _}; _} :: _)
     |Pstr_open {popen_attributes= atrs; _}
     |Pstr_extension (_, atrs)
     |Pstr_class_type ({pci_attributes= atrs; _} :: _)
     |Pstr_class ({pci_attributes= atrs; _} :: _) ->
        List.exists ~f:Attr.is_doc atrs
    | Pstr_exception
        { ptyexn_attributes= atrs1
        ; ptyexn_constructor= {pext_attributes= atrs2; _}
        ; _ }
     |Pstr_include
        {pincl_mod= {pmod_attributes= atrs1; _}; pincl_attributes= atrs2; _}
      ->
        List.exists ~f:Attr.is_doc atrs1 || List.exists ~f:Attr.is_doc atrs2
    | Pstr_modtype {pmtd_ext_attrs; _} -> Ext_attrs.has_doc pmtd_ext_attrs
    | Pstr_module {pmb_ext_attrs; pmb_expr= {pmod_attributes; _}; _} ->
        Ext_attrs.has_doc pmb_ext_attrs
        || List.exists ~f:Attr.is_doc pmod_attributes
    | Pstr_value {pvbs_bindings= []; _}
     |Pstr_type (_, [])
     |Pstr_kind_abbrev _
     |Pstr_recmodule []
     |Pstr_class_type []
     |Pstr_class [] ->
        false

  let is_simple (itm, (c : Conf.t)) =
    match c.fmt_opts.module_item_spacing.v with
    | `Compact | `Preserve ->
        Location.is_single_line itm.pstr_loc c.fmt_opts.margin.v
    | `Sparse -> (
      match itm.pstr_desc with
      | Pstr_include {pincl_mod= me; _} | Pstr_module {pmb_expr= me; _} ->
          let rec is_simple_mod me =
            match me.pmod_desc with
            | Pmod_apply (me1, me2) -> is_simple_mod me1 && is_simple_mod me2
            | Pmod_functor (_, me) | Pmod_apply_unit (me, _) ->
                is_simple_mod me
            | Pmod_ident i -> longident_is_simple c i.txt
            | _ -> false
          in
          is_simple_mod me
      | Pstr_open {popen_expr= {pmod_desc= Pmod_ident i; _}; _} ->
          longident_is_simple c i.txt
      | _ -> false )

  let rec allow_adjacent (itmI, cI) (itmJ, cJ) =
    match
      Conf.
        (cI.fmt_opts.module_item_spacing.v, cJ.fmt_opts.module_item_spacing.v)
    with
    | `Compact, `Compact -> (
      match (itmI.pstr_desc, itmJ.pstr_desc) with
      | Pstr_eval _, Pstr_eval _
       |Pstr_value _, Pstr_value _
       |Pstr_primitive _, Pstr_primitive _
       |(Pstr_type _ | Pstr_typext _), (Pstr_type _ | Pstr_typext _)
       |Pstr_exception _, Pstr_exception _
       |( (Pstr_module _ | Pstr_recmodule _ | Pstr_open _ | Pstr_include _)
        , (Pstr_module _ | Pstr_recmodule _ | Pstr_open _ | Pstr_include _) )
       |Pstr_modtype _, Pstr_modtype _
       |Pstr_class _, Pstr_class _
       |Pstr_class_type _, Pstr_class_type _
       |Pstr_attribute _, Pstr_attribute _ ->
          true
      | ( Pstr_extension ((_, PStr [n1]), _attrs1)
        , Pstr_extension ((_, PStr [n2]), _attrs2) ) ->
          allow_adjacent (n1, cI) (n2, cJ)
      | Pstr_extension _, Pstr_extension _ -> true
      | _ -> false )
    | _ -> true

  let break_between s cc (i1, c1) (i2, c2) =
    cmts_between s cc i1.pstr_loc i2.pstr_loc
    || has_doc i1 || has_doc i2
    ||
    match
      Conf.
        (c1.fmt_opts.module_item_spacing.v, c2.fmt_opts.module_item_spacing.v)
    with
    | `Preserve, `Preserve ->
        Source.empty_line_between s i1.pstr_loc.loc_end i2.pstr_loc.loc_start
    | _ ->
        (not (is_simple (i1, c1)))
        || (not (is_simple (i2, c2)))
        || not (allow_adjacent (i1, c1) (i2, c2))
end

module Signature_item = struct
  let has_doc itm =
    match itm.psig_desc with
    | Psig_attribute atr -> Attr.is_doc atr
    (* one attribute list *)
    | Psig_value {pval_attributes= atrs; _}
     |Psig_type (_, {ptype_attributes= atrs; _} :: _)
     |Psig_typesubst ({ptype_attributes= atrs; _} :: _)
     |Psig_typext {ptyext_attributes= atrs; _}
     |Psig_open {popen_attributes= atrs; _}
     |Psig_extension (_, atrs)
     |Psig_class_type ({pci_attributes= atrs; _} :: _)
     |Psig_class ({pci_attributes= atrs; _} :: _) ->
        List.exists ~f:Attr.is_doc atrs
    (* two attribute list *)
    | Psig_modtype {pmtd_ext_attrs= ea; _}
     |Psig_modtypesubst {pmtd_ext_attrs= ea; _}
     |Psig_modsubst {pms_ext_attrs= ea; _} ->
        Ext_attrs.has_doc ea
    | Psig_include
        ( {pincl_mod= {pmty_attributes= atrs1; _}; pincl_attributes= atrs2; _}
        , _ )
     |Psig_exception
        { ptyexn_attributes= atrs1
        ; ptyexn_constructor= {pext_attributes= atrs2; _}
        ; _ } ->
        List.exists ~f:Attr.is_doc atrs1 || List.exists ~f:Attr.is_doc atrs2
    | Psig_recmodule
        ({pmd_type= {pmty_attributes= atrs; _}; pmd_ext_attrs= ea; _} :: _)
     |Psig_module {pmd_ext_attrs= ea; pmd_type= {pmty_attributes= atrs; _}; _}
      ->
        Ext_attrs.has_doc ea || (List.exists ~f:Attr.is_doc) atrs
    | Psig_type (_, [])
     |Psig_typesubst []
     |Psig_kind_abbrev (_, _)
     |Psig_recmodule []
     |Psig_class_type []
     |Psig_class [] ->
        false

  let is_simple (itm, (c : Conf.t)) =
    match c.fmt_opts.module_item_spacing.v with
    | `Compact | `Preserve ->
        Location.is_single_line itm.psig_loc c.fmt_opts.margin.v
    | `Sparse -> (
      match itm.psig_desc with
      | Psig_open {popen_expr= i; _}
       |Psig_module {pmd_type= {pmty_desc= Pmty_alias i; _}; _}
       |Psig_modsubst {pms_manifest= i; _} ->
          longident_is_simple c i.txt
      | _ -> false )

  let allow_adjacent (itmI, cI) (itmJ, cJ) =
    match
      Conf.
        (cI.fmt_opts.module_item_spacing.v, cJ.fmt_opts.module_item_spacing.v)
    with
    | `Compact, `Compact -> (
      match (itmI.psig_desc, itmJ.psig_desc) with
      | Psig_value _, Psig_value _
       |( (Psig_type _ | Psig_typesubst _ | Psig_typext _)
        , (Psig_type _ | Psig_typesubst _ | Psig_typext _) )
       |Psig_exception _, Psig_exception _
       |( ( Psig_module _ | Psig_modsubst _ | Psig_recmodule _ | Psig_open _
          | Psig_include _ )
        , ( Psig_module _ | Psig_modsubst _ | Psig_recmodule _ | Psig_open _
          | Psig_include _ ) )
       |Psig_modtype _, Psig_modtype _
       |Psig_class _, Psig_class _
       |Psig_class_type _, Psig_class_type _
       |Psig_attribute _, Psig_attribute _
       |Psig_extension _, Psig_extension _ ->
          true
      | _ -> false )
    | _ -> true

  let break_between s cc (i1, c1) (i2, c2) =
    cmts_between s cc i1.psig_loc i2.psig_loc
    || has_doc i1 || has_doc i2
    ||
    match
      Conf.
        (c1.fmt_opts.module_item_spacing.v, c2.fmt_opts.module_item_spacing.v)
    with
    | `Preserve, `Preserve ->
        Source.empty_line_between s i1.psig_loc.loc_end i2.psig_loc.loc_start
    | _ ->
        (not (is_simple (i1, c1)))
        || (not (is_simple (i2, c2)))
        || not (allow_adjacent (i1, c1) (i2, c2))
end

module Lb = struct
  let has_doc itm = List.exists ~f:Attr.is_doc itm.pvb_attributes

  let is_simple (i, (c : Conf.t)) =
    Poly.(c.fmt_opts.module_item_spacing.v = `Compact)
    && Location.is_single_line i.pvb_loc c.fmt_opts.margin.v

  let break_between s cc (i1, c1) (i2, c2) =
    cmts_between s cc i1.pvb_loc i2.pvb_loc
    || has_doc i1 || has_doc i2
    || (not (is_simple (i1, c1)))
    || not (is_simple (i2, c2))
end

module Mb = struct
  let has_doc itm = Ext_attrs.has_doc itm.pmb_ext_attrs

  let is_simple (i, (c : Conf.t)) =
    Poly.(c.fmt_opts.module_item_spacing.v = `Compact)
    && Location.is_single_line i.pmb_loc c.fmt_opts.margin.v

  let break_between s cc (i1, c1) (i2, c2) =
    cmts_between s cc i1.pmb_loc i2.pmb_loc
    || has_doc i1 || has_doc i2
    || (not (is_simple (i1, c1)))
    || not (is_simple (i2, c2))
end

module Md = struct
  let has_doc itm = Ext_attrs.has_doc itm.pmd_ext_attrs

  let is_simple (i, (c : Conf.t)) =
    Poly.(c.fmt_opts.module_item_spacing.v = `Compact)
    && Location.is_single_line i.pmd_loc c.fmt_opts.margin.v

  let break_between s cc (i1, c1) (i2, c2) =
    cmts_between s cc i1.pmd_loc i2.pmd_loc
    || has_doc i1 || has_doc i2
    || (not (is_simple (i1, c1)))
    || not (is_simple (i2, c2))
end

module Td = struct
  let has_doc itm = List.exists ~f:Attr.is_doc itm.ptype_attributes

  let is_simple (i, (c : Conf.t)) =
    match c.fmt_opts.module_item_spacing.v with
    | `Compact | `Preserve ->
        Location.is_single_line i.ptype_loc c.fmt_opts.margin.v
    | `Sparse -> false

  let break_between s cc (i1, c1) (i2, c2) =
    cmts_between s cc i1.ptype_loc i2.ptype_loc
    || has_doc i1 || has_doc i2
    ||
    match
      Conf.
        (c1.fmt_opts.module_item_spacing.v, c2.fmt_opts.module_item_spacing.v)
    with
    | `Preserve, `Preserve ->
        Source.empty_line_between s i1.ptype_loc.loc_end
          i2.ptype_loc.loc_start
    | _ -> (not (is_simple (i1, c1))) || not (is_simple (i2, c2))
end

module Class_field = struct
  let has_doc itm =
    List.exists ~f:Attr.is_doc itm.pcf_attributes
    ||
    match itm.pcf_desc with
    | Pcf_attribute atr -> Attr.is_doc atr
    | _ -> false

  let is_simple (itm, (c : Conf.t)) =
    match c.fmt_opts.module_item_spacing.v with
    | `Compact | `Preserve ->
        Location.is_single_line itm.pcf_loc c.fmt_opts.margin.v
    | `Sparse -> false

  let break_between s cc (i1, c1) (i2, c2) =
    cmts_between s cc i1.pcf_loc i2.pcf_loc
    || has_doc i1 || has_doc i2
    ||
    match
      Conf.
        (c1.fmt_opts.module_item_spacing.v, c2.fmt_opts.module_item_spacing.v)
    with
    | `Preserve, `Preserve ->
        Source.empty_line_between s i1.pcf_loc.loc_end i2.pcf_loc.loc_start
    | _ -> (not (is_simple (i1, c1))) || not (is_simple (i2, c2))
end

module Class_type_field = struct
  let has_doc itm =
    List.exists ~f:Attr.is_doc itm.pctf_attributes
    ||
    match itm.pctf_desc with
    | Pctf_attribute atr -> Attr.is_doc atr
    | _ -> false

  let is_simple (itm, (c : Conf.t)) =
    match c.fmt_opts.module_item_spacing.v with
    | `Compact | `Preserve ->
        Location.is_single_line itm.pctf_loc c.fmt_opts.margin.v
    | `Sparse -> false

  let break_between s cc (i1, c1) (i2, c2) =
    cmts_between s cc i1.pctf_loc i2.pctf_loc
    || has_doc i1 || has_doc i2
    ||
    match
      Conf.
        (c1.fmt_opts.module_item_spacing.v, c2.fmt_opts.module_item_spacing.v)
    with
    | `Preserve, `Preserve ->
        Source.empty_line_between s i1.pctf_loc.loc_end i2.pctf_loc.loc_start
    | _ -> (not (is_simple (i1, c1))) || not (is_simple (i2, c2))
end

type toplevel_item =
  [`Item of structure_item | `Directive of toplevel_directive]

(** Ast terms of various forms. *)
module T = struct
  type t =
    | Pld of payload
    | Typ of core_type
    | Td of type_declaration
    | Kab of kind_abbreviation
    | Tyv of ty_var
    | Cty of class_type
    | Pat of pattern
    | Exp of expression
    | Fp of function_param
    | Vc of value_constraint
    | Lb of value_binding
    | Mb of module_binding
    | Md of module_declaration
    | Cl of class_expr
    | Mty of module_type
    | Mod of module_expr
    | Sig of signature_item
    | Str of structure_item
    | Clf of class_field
    | Ctf of class_type_field
    | Tli of toplevel_item
    | Jkd of jkind_annotation
    | Top
    | Rep

  let dump fs = function
    | Pld l -> Format.fprintf fs "Pld:@\n%a" Printast.payload l
    | Typ t -> Format.fprintf fs "Typ:@\n%a" Printast.core_type t
    | Td t -> Format.fprintf fs "Td:@\n%a" Printast.type_declaration t
    | Kab k -> Format.fprintf fs "Kab:@\n%a" (Printast.kind_abbreviation 0) k
    | Tyv v -> Format.fprintf fs "Tyv:@\n%a" (Printast.typevar 0) v
    | Pat p -> Format.fprintf fs "Pat:@\n%a" Printast.pattern p
    | Exp e -> Format.fprintf fs "Exp:@\n%a" Printast.expression e
    | Fp p -> Format.fprintf fs "Fp:@\n%a" Printast.function_param p
    | Vc c -> Format.fprintf fs "Vc:@\n%a" Printast.value_constraint c
    | Lb b -> Format.fprintf fs "Lb:@\n%a" Printast.value_binding b
    | Mb m -> Format.fprintf fs "Mb:@\n%a" Printast.module_binding m
    | Md m -> Format.fprintf fs "Md:@\n%a" Printast.module_declaration m
    | Cl cl -> Format.fprintf fs "Cl:@\n%a" Printast.class_expr cl
    | Mty mt -> Format.fprintf fs "Mty:@\n%a" Printast.module_type mt
    | Cty cty -> Format.fprintf fs "Cty:@\n%a" Printast.class_type cty
    | Mod m -> Format.fprintf fs "Mod:@\n%a" Printast.module_expr m
    | Sig s -> Format.fprintf fs "Sig:@\n%a" Printast.signature_item s
    | Str s | Tli (`Item s) ->
        Format.fprintf fs "Str:@\n%a" Printast.structure_item s
    | Clf clf -> Format.fprintf fs "Clf:@\n%a@\n" Printast.class_field clf
    | Ctf ctf ->
        Format.fprintf fs "Ctf:@\n%a@\n" Printast.class_type_field ctf
    | Tli (`Directive d) ->
        Format.fprintf fs "Dir:@\n%a" Printast.top_phrase (Ptop_dir d)
    | Jkd jkd ->
        Format.fprintf fs "Jkd:@\n%a" (Printast.jkind_annotation 0) jkd
    | Top -> Format.pp_print_string fs "Top"
    | Rep -> Format.pp_print_string fs "Rep"
end

include T

let is_top = function Top -> true | _ -> false

let attrs_of_ext_attrs ea = ea.attrs_before @ ea.attrs_after

let attributes = function
  | Pld _ -> []
  | Typ x -> x.ptyp_attributes
  | Td x -> x.ptype_attributes
  | Kab _ -> []
  | Tyv _ -> []
  | Cty x -> x.pcty_attributes
  | Pat x -> x.ppat_attributes
  | Exp x -> x.pexp_attributes
  | Fp _ -> []
  | Vc _ -> []
  | Lb x -> x.pvb_attributes
  | Mb x -> attrs_of_ext_attrs x.pmb_ext_attrs
  | Md x -> attrs_of_ext_attrs x.pmd_ext_attrs
  | Cl x -> x.pcl_attributes
  | Mty x -> x.pmty_attributes
  | Mod x -> x.pmod_attributes
  | Sig _ -> []
  | Str _ -> []
  | Clf x -> x.pcf_attributes
  | Ctf x -> x.pctf_attributes
  | Top -> []
  | Tli _ -> []
  | Jkd _ -> []
  | Rep -> []

let location = function
  | Pld _ -> Location.none
  | Typ x -> x.ptyp_loc
  | Td x -> x.ptype_loc
  | Kab _ -> Location.none
  | Tyv _ -> Location.none
  | Cty x -> x.pcty_loc
  | Pat x -> x.ppat_loc
  | Exp x -> x.pexp_loc
  | Fp x -> x.pparam_loc
  | Vc _ -> Location.none
  | Lb x -> x.pvb_loc
  | Mb x -> x.pmb_loc
  | Md x -> x.pmd_loc
  | Cl x -> x.pcl_loc
  | Mty x -> x.pmty_loc
  | Mod x -> x.pmod_loc
  | Sig x -> x.psig_loc
  | Str x -> x.pstr_loc
  | Clf x -> x.pcf_loc
  | Ctf x -> x.pctf_loc
  | Tli (`Item x) -> x.pstr_loc
  | Tli (`Directive x) -> x.pdir_loc
  | Jkd _ -> Location.none
  | Top -> Location.none
  | Rep -> Location.none

let break_between_modules s cc (i1, c1) (i2, c2) =
  let has_doc itm = List.exists ~f:Attr.is_doc (attributes itm) in
  let is_simple (itm, (c : Conf.t)) =
    Location.is_single_line (location itm) c.fmt_opts.margin.v
  in
  cmts_between s cc (location i1) (location i2)
  || has_doc i1 || has_doc i2
  || (not (is_simple (i1, c1)))
  || not (is_simple (i2, c2))

let break_between s cc (i1, c1) (i2, c2) =
  match (i1, i2) with
  | Str i1, Str i2 -> Structure_item.break_between s cc (i1, c1) (i2, c2)
  | Sig i1, Sig i2 -> Signature_item.break_between s cc (i1, c1) (i2, c2)
  | Lb i1, Lb i2 -> Lb.break_between s cc (i1, c1) (i2, c2)
  | Mb i1, Mb i2 -> Mb.break_between s cc (i1, c1) (i2, c2)
  | Md i1, Md i2 -> Md.break_between s cc (i1, c1) (i2, c2)
  | Mty _, Mty _ -> break_between_modules s cc (i1, c1) (i2, c2)
  | Mod _, Mod _ -> break_between_modules s cc (i1, c1) (i2, c2)
  | Tli (`Item i1), Tli (`Item i2) ->
      Structure_item.break_between s cc (i1, c1) (i2, c2)
  | Tli (`Directive _), Tli (`Directive _) | Tli _, Tli _ ->
      true (* always break between an item and a directive *)
  | Clf i1, Clf i2 -> Class_field.break_between s cc (i1, c1) (i2, c2)
  | Ctf i1, Ctf i2 -> Class_type_field.break_between s cc (i1, c1) (i2, c2)
  | Td i1, Td i2 -> Td.break_between s cc (i1, c1) (i2, c2)
  | _ -> assert false

(** Term-in-context, [{ctx; ast}] records that [ast] is (considered to be) an
    immediate sub-term of [ctx] as assumed by the operations in
    [Requires_sub_terms]. *)
module rec In_ctx : sig
  type 'a xt = private {ctx: T.t; ast: 'a}

  val sub_ast : ctx:T.t -> T.t -> T.t xt

  val sub_typ : ctx:T.t -> core_type -> core_type xt

  val sub_td : ctx:T.t -> type_declaration -> type_declaration xt

  val sub_cty : ctx:T.t -> class_type -> class_type xt

  val sub_pat : ctx:T.t -> pattern -> pattern xt

  val sub_exp : ctx:T.t -> expression -> expression xt

  val sub_cl : ctx:T.t -> class_expr -> class_expr xt

  val sub_cf : ctx:T.t -> class_field -> class_field xt

  val sub_ctf : ctx:t -> class_type_field -> class_type_field xt

  val sub_mty : ctx:T.t -> module_type -> module_type xt

  val sub_mod : ctx:T.t -> module_expr -> module_expr xt

  val sub_md : ctx:T.t -> module_declaration -> module_declaration xt

  val sub_mb : ctx:T.t -> module_binding -> module_binding xt

  val sub_sig : ctx:T.t -> signature_item -> signature_item xt

  val sub_str : ctx:T.t -> structure_item -> structure_item xt
end = struct
  open Requires_sub_terms

  type 'a xt = {ctx: T.t; ast: 'a}

  let sub_ast ~ctx ast = {ctx; ast}

  let sub_typ ~ctx typ = check parenze_typ {ctx; ast= typ}

  let sub_td ~ctx td = {ctx; ast= td}

  let sub_cty ~ctx cty = {ctx; ast= cty}

  let sub_pat ~ctx pat = check parenze_pat {ctx; ast= pat}

  let sub_exp ~ctx exp = check parenze_exp {ctx; ast= exp}

  let sub_cl ~ctx cl = {ctx; ast= cl}

  let sub_cf ~ctx cf = {ctx; ast= cf}

  let sub_ctf ~ctx ctf = {ctx; ast= ctf}

  let sub_mty ~ctx mty = {ctx; ast= mty}

  let sub_mod ~ctx mod_ = {ctx; ast= mod_}

  let sub_md ~ctx md = {ctx; ast= md}

  let sub_mb ~ctx mb = {ctx; ast= mb}

  let sub_sig ~ctx sig_ = {ctx; ast= sig_}

  let sub_str ~ctx str = {ctx; ast= str}
end

(** Operations determining precedence and necessary parenthesization of terms
    based on their super-terms. *)
and Requires_sub_terms : sig
  val is_simple :
    Conf.t -> (expression In_ctx.xt -> int) -> expression In_ctx.xt -> bool

  val exposed_right_exp : cls -> expression -> bool

  val prec_ast : T.t -> Prec.t option

  val parenze_typ : core_type In_ctx.xt -> bool

  val parenze_mty : module_type In_ctx.xt -> bool

  val parenze_mod : module_expr In_ctx.xt -> bool

  val parenze_cty : class_type In_ctx.xt -> bool

  val parenze_cl : class_expr In_ctx.xt -> bool

  val parenze_pat : pattern In_ctx.xt -> bool

  val parenze_exp : expression In_ctx.xt -> bool

  val parenze_nested_exp : expression In_ctx.xt -> bool
end = struct
  open In_ctx

  (* This module uses physical equality extensively to detect sub-terms. *)

  let ( == ) = Base.phys_equal

  let dump ctx ast fs =
    Format.fprintf fs "ast: %a@\nctx: %a@\n" T.dump ast T.dump ctx

  let assert_no_raise ~f ~dump x =
    assert (
      try
        ignore (f x) ;
        true
      with exc ->
        let bt = Stdlib.Printexc.get_backtrace () in
        dump x Format.err_formatter ;
        Format.eprintf "%s%!" bt ;
        raise exc )

  (** Predicates to check the claimed sub-term relation. *)

  let check_typ {ctx; ast= typ} =
    let f tI = typ == tI in
    let fst_f (tI, _) = typ == tI in
    let snd_f (_, tI) = typ == tI in
    let check_cstr = function
      | Pcstr_tuple t1N -> List.exists t1N ~f:(fun carg -> f carg.pca_type)
      | Pcstr_record (_, ld1N) ->
          List.exists ld1N ~f:(fun {pld_type; _} -> typ == pld_type)
    in
    let check_ext {pext_kind; _} =
      match pext_kind with
      | Pext_decl (_, cstr, t0) -> check_cstr cstr || Option.exists t0 ~f
      | _ -> false
    in
    let check_typext {ptyext_params; ptyext_constructors; _} =
      List.exists ptyext_params ~f:fst_f
      || List.exists ptyext_constructors ~f:check_ext
    in
    let check_typexn {ptyexn_constructor; _} =
      check_ext ptyexn_constructor
    in
    let check_class_type l =
      List.exists l ~f:(fun {pci_expr= {pcty_desc; _}; pci_params; _} ->
          List.exists pci_params ~f:(fun (t, _) -> t == typ)
          ||
          match pcty_desc with
          | Pcty_constr (_, l) -> List.exists l ~f:(fun x -> x == typ)
          | Pcty_arrow (t, _) ->
              List.exists t ~f:(fun x -> x.pap_type == typ)
          | _ -> false )
    in
    let check_value_constraint = function
      | Pvc_constraint {typ= typ'; _} -> typ' == typ
      | Pvc_coercion {ground; coercion} ->
          coercion == typ || Option.exists ground ~f:(fun x -> x == typ)
    in
    let check_pvb pvb =
      Option.exists pvb.pvb_constraint ~f:check_value_constraint
    in
    let check_let_bindings lbs =
      List.exists lbs.pvbs_bindings ~f:check_pvb
    in
    match ctx with
    | Pld (PTyp t1) -> assert (typ == t1)
    | Pld _ -> assert false
    | Typ ctx -> (
      match ctx.ptyp_desc with
      | Ptyp_extension _ -> ()
      | Ptyp_any | Ptyp_var _ -> assert false
      | Ptyp_alias (t1, _) | Ptyp_poly (_, t1) -> assert (typ == t1)
      | Ptyp_arrow (t, t2, _) ->
          assert (List.exists t ~f:(fun x -> typ == x.pap_type) || typ == t2)
      | Ptyp_tuple t1N | Ptyp_unboxed_tuple t1N ->
          assert (List.exists t1N ~f:(fun (_, t) -> f t))
      | Ptyp_constr (_, t1N) -> assert (List.exists t1N ~f)
      | Ptyp_variant (r1N, _, _) ->
          assert (
            List.exists r1N ~f:(function
              | {prf_desc= Rtag (_, _, t1N); _} -> List.exists t1N ~f
              | {prf_desc= Rinherit t1; _} -> typ == t1 ) )
      | Ptyp_package (_, it1N, _) -> assert (List.exists it1N ~f:snd_f)
      | Ptyp_object (fields, _) ->
          assert (
            List.exists fields ~f:(function
              | {pof_desc= Otag (_, t1); _} -> typ == t1
              | {pof_desc= Oinherit t1; _} -> typ == t1 ) )
      | Ptyp_class (_, l) -> assert (List.exists l ~f)
      | Ptyp_constr_unboxed (_, t1N) -> assert (List.exists t1N ~f) )
    | Td {ptype_params; ptype_cstrs; ptype_kind; ptype_manifest; _} ->
        assert (
          List.exists ptype_params ~f:fst_f
          || List.exists ptype_cstrs ~f:(fun (t1, t2, _) ->
                 typ == t1 || typ == t2 )
          || ( match ptype_kind with
             | Ptype_variant cd1N ->
                 List.exists cd1N ~f:(fun {pcd_args; pcd_res; _} ->
                     check_cstr pcd_args || Option.exists pcd_res ~f )
             | Ptype_record ld1N | Ptype_record_unboxed_product ld1N ->
                 List.exists ld1N ~f:(fun {pld_type; _} -> typ == pld_type)
             | _ -> false )
          || Option.exists ptype_manifest ~f )
    | Kab _ -> assert false
    | Tyv _ -> assert false
    | Cty {pcty_desc; _} ->
        assert (
          match pcty_desc with
          | Pcty_constr (_, l) -> List.exists l ~f
          | Pcty_arrow (t, _) ->
              List.exists t ~f:(fun x -> x.pap_type == typ)
          | Pcty_open _ -> false
          | Pcty_extension _ -> false
          | Pcty_signature {pcsig_self; _} -> Option.exists pcsig_self ~f )
    | Pat ctx -> (
      match ctx.ppat_desc with
      | Ppat_constraint (_, Some t1, _) -> assert (typ == t1)
      | Ppat_extension (_, PTyp t) -> assert (typ == t)
      | Ppat_unpack (_, Some (_, l, _)) ->
          assert (List.exists l ~f:(fun (_, t) -> typ == t))
      | Ppat_record (l, _) | Ppat_record_unboxed_product (l, _) ->
          assert (List.exists l ~f:(fun (_, t, _) -> Option.exists t ~f))
      | _ -> assert false )
    | Exp ctx -> (
      match ctx.pexp_desc with
      | Pexp_pack (_, Some (_, it1N, _)) -> assert (List.exists it1N ~f:snd_f)
      | Pexp_constraint (_, Some t1, _)
       |Pexp_coerce (_, None, t1)
       |Pexp_poly (_, Some t1)
       |Pexp_extension (_, PTyp t1) ->
          assert (typ == t1)
      | Pexp_coerce (_, Some t1, t2) -> assert (typ == t1 || typ == t2)
      | Pexp_letexception (ext, _) -> assert (check_ext ext)
      | Pexp_object _ -> assert false
      | Pexp_record (en1, _) | Pexp_record_unboxed_product (en1, _) ->
          assert (
            List.exists en1 ~f:(fun (_, c, _) ->
                Option.exists c ~f:(function
                  | Pconstraint t -> f t
                  | Pcoerce (t1, t2) -> Option.exists t1 ~f || f t2 ) ) )
      | Pexp_let (lbs, _) -> assert (check_let_bindings lbs)
      | _ -> assert false )
    | Fp _ -> assert false
    | Vc c -> assert (check_value_constraint c)
    | Lb _ -> assert false
    | Mb _ -> assert false
    | Md _ -> assert false
    | Cl {pcl_desc; _} ->
        assert (
          match pcl_desc with
          | Pcl_constr (_, l) -> List.exists l ~f
          | Pcl_constraint _ -> false
          | Pcl_let (lbs, _) -> check_let_bindings lbs
          | Pcl_apply _ -> false
          | Pcl_fun _ -> false
          | Pcl_open _ -> false
          | Pcl_extension _ -> false
          | Pcl_structure _ -> false )
    | Mty _ -> assert false
    | Mod ctx -> (
      match ctx.pmod_desc with
      | Pmod_unpack (_, ty1, ty2) ->
          let f (_, cstrs, _) = List.exists cstrs ~f:(fun (_, x) -> f x) in
          assert (Option.exists ty1 ~f || Option.exists ty2 ~f)
      | _ -> assert false )
    | Sig ctx -> (
      match ctx.psig_desc with
      | Psig_value {pval_type= t1; _} -> assert (typ == t1)
      | Psig_type (_, _) -> assert false
      | Psig_typesubst _ -> assert false
      | Psig_typext typext -> assert (check_typext typext)
      | Psig_exception ext -> assert (check_typexn ext)
      | Psig_class_type l -> assert (check_class_type l)
      | Psig_class l -> assert (check_class_type l)
      | _ -> assert false )
    | Str ctx -> (
      match ctx.pstr_desc with
      | Pstr_primitive {pval_type= t1; _} -> assert (typ == t1)
      | Pstr_type (_, _) -> assert false
      | Pstr_typext typext -> assert (check_typext typext)
      | Pstr_exception ext -> assert (check_typexn ext)
      | Pstr_class l ->
          assert (
            List.exists l ~f:(fun {pci_expr= {pcl_desc; _}; pci_params; _} ->
                List.exists pci_params ~f:(fun (t, _) -> t == typ)
                ||
                match pcl_desc with
                | Pcl_constr (_, l) -> List.exists l ~f:(fun x -> x == typ)
                | _ -> false ) )
      | Pstr_class_type l -> assert (check_class_type l)
      | Pstr_extension ((_, PTyp t), _) -> assert (t == typ)
      | Pstr_extension (_, _) -> assert false
      | Pstr_value {pvbs_bindings; _} ->
          assert (List.exists pvbs_bindings ~f:check_pvb)
      | _ -> assert false )
    | Clf {pcf_desc; _} ->
        assert (
          match pcf_desc with
          | Pcf_inherit (_, _, _) -> false
          | Pcf_val (_, _, Cfk_virtual t) -> typ == t
          | Pcf_val
              ( _
              , _
              , Cfk_concrete
                  (_, {pexp_desc= Pexp_constraint (_, Some t, _); _}) ) ->
              typ == t
          | Pcf_val (_, _, Cfk_concrete _) -> false
          | Pcf_method (_, _, Cfk_virtual t) -> typ == t
          | Pcf_method
              ( _
              , _
              , Cfk_concrete
                  (_, {pexp_desc= Pexp_constraint (_, Some t, _); _}) ) ->
              typ == t
          | Pcf_method
              (_, _, Cfk_concrete (_, {pexp_desc= Pexp_poly (e, topt); _}))
            ->
              let rec loop = function
                | {pexp_desc= Pexp_newtype (_, e); _} -> loop e
                | {pexp_desc= Pexp_constraint (_, Some t, _); _} -> t == typ
                | {pexp_desc= Pexp_fun (_, e); _} -> loop e
                | _ -> false
              in
              (match topt with None -> false | Some t -> typ == t)
              || loop e
          | Pcf_method (_, _, Cfk_concrete _) -> false
          | Pcf_constraint (t1, t2) -> t1 == typ || t2 == typ
          | Pcf_initializer _ | Pcf_attribute _ | Pcf_extension _ -> false )
    | Ctf {pctf_desc; _} ->
        assert (
          match pctf_desc with
          | Pctf_constraint (t1, t2) -> t1 == typ || t2 == typ
          | Pctf_val (_, _, t) -> t == typ
          | Pctf_method (_, _, t) -> t == typ
          | Pctf_inherit _ -> false
          | Pctf_attribute _ -> false
          | Pctf_extension _ -> false )
    | Jkd j ->
        assert (
          match j with
          | Kind_of t | With (_, t) -> t == typ
          | Default | Abbreviation _ | Mod _ | Product _ -> false )
    | Top | Tli _ | Rep -> assert false

  let assert_check_typ xtyp =
    let dump {ctx; ast= typ} = dump ctx (Typ typ) in
    assert_no_raise ~f:check_typ ~dump xtyp

  let check_cty {ctx; ast= cty} =
    let check_class_type l =
      List.exists l ~f:(fun {pci_expr; _} ->
          let rec loop x =
            x == cty
            ||
            match x.pcty_desc with Pcty_arrow (_, x) -> loop x | _ -> false
          in
          loop pci_expr )
    in
    match (ctx : t) with
    | Exp _ -> assert false
    | Fp _ -> assert false
    | Vc _ -> assert false
    | Lb _ -> assert false
    | Mb _ -> assert false
    | Md _ -> assert false
    | Pld _ -> assert false
    | Str ctx -> (
      match ctx.pstr_desc with
      | Pstr_class_type l -> assert (check_class_type l)
      | Pstr_class l ->
          assert (
            List.exists l ~f:(fun {pci_expr; _} ->
                let rec loop x =
                  match x.pcl_desc with
                  | Pcl_fun (_, _, _, x) -> loop x
                  | Pcl_constraint (_, x) -> x == cty
                  | _ -> false
                in
                loop pci_expr ) )
      | _ -> assert false )
    | Sig ctx -> (
      match ctx.psig_desc with
      | Psig_class_type l -> assert (check_class_type l)
      | Psig_class l -> assert (check_class_type l)
      | _ -> assert false )
    | Cty {pcty_desc; _} -> (
      match pcty_desc with
      | Pcty_arrow (_, t) -> assert (t == cty)
      | Pcty_signature _ -> assert false
      | Pcty_open (_, t) -> assert (t == cty)
      | Pcty_constr _ -> assert false
      | Pcty_extension _ -> assert false )
    | Top -> assert false
    | Tli _ -> assert false
    | Typ _ -> assert false
    | Td _ -> assert false
    | Kab _ -> assert false
    | Tyv _ -> assert false
    | Pat _ -> assert false
    | Cl ctx ->
        assert (
          match ctx.pcl_desc with
          | Pcl_fun (_, _, _, _) -> false
          | Pcl_constr _ -> false
          | Pcl_structure _ -> false
          | Pcl_apply _ -> false
          | Pcl_let (_, _) -> false
          | Pcl_constraint (_, x) -> x == cty
          | Pcl_extension _ -> false
          | Pcl_open _ -> false )
    | Clf _ -> assert false
    | Ctf {pctf_desc; _} ->
        assert (
          match pctf_desc with
          | Pctf_inherit t -> t == cty
          | Pctf_val _ -> false
          | Pctf_method _ -> false
          | Pctf_constraint _ -> false
          | Pctf_attribute _ -> false
          | Pctf_extension _ -> false )
    | Jkd _ -> assert false
    | Mty _ -> assert false
    | Mod _ -> assert false
    | Rep -> assert false

  let assert_check_cty xcty =
    let dump {ctx; ast= cty} = dump ctx (Cty cty) in
    assert_no_raise ~f:check_cty ~dump xcty

  let check_cl {ctx; ast= cl} =
    match (ctx : t) with
    | Exp _ -> assert false
    | Fp _ -> assert false
    | Vc _ -> assert false
    | Lb _ -> assert false
    | Mb _ -> assert false
    | Md _ -> assert false
    | Pld _ -> assert false
    | Str ctx -> (
      match ctx.pstr_desc with
      | Pstr_class l ->
          assert (
            List.exists l ~f:(fun {pci_expr; _} ->
                let rec loop x =
                  cl == x
                  ||
                  match x.pcl_desc with
                  | Pcl_fun (_, _, _, x) -> loop x
                  | Pcl_constraint (x, _) -> loop x
                  | _ -> false
                in
                loop pci_expr ) )
      | _ -> assert false )
    | Sig _ -> assert false
    | Cty _ -> assert false
    | Top -> assert false
    | Tli _ -> assert false
    | Typ _ -> assert false
    | Td _ -> assert false
    | Kab _ -> assert false
    | Tyv _ -> assert false
    | Pat _ -> assert false
    | Cl {pcl_desc; _} ->
        assert (
          match pcl_desc with
          | Pcl_structure _ -> false
          | Pcl_fun (_, _, _, x) -> x == cl
          | Pcl_apply (x, _) -> x == cl
          | Pcl_let (_, x) -> x == cl
          | Pcl_constraint (x, _) -> x == cl
          | Pcl_open (_, x) -> x == cl
          | Pcl_constr _ -> false
          | Pcl_extension _ -> false )
    | Clf {pcf_desc; _} ->
        assert (
          match pcf_desc with Pcf_inherit (_, x, _) -> x == cl | _ -> false )
    | Ctf _ -> assert false
    | Mty _ -> assert false
    | Mod _ -> assert false
    | Rep -> assert false
    | Jkd _ -> assert false

  let assert_check_cl xcl =
    let dump {ctx; ast= cl} = dump ctx (Cl cl) in
    assert_no_raise ~f:check_cl ~dump xcl

  module Comprehension_child = struct
    type t = Expression of expression | Pattern of pattern
  end

  let check_comprehension {comp_body= body; clauses}
      (tgt : Comprehension_child.t) =
    let expression_is_child =
      match tgt with
      | Expression exp -> fun exp' -> exp' == exp
      | _ -> fun _ -> false
    in
    let pattern_is_child =
      match tgt with
      | Pattern pat -> fun pat' -> pat' == pat
      | _ -> fun _ -> false
    in
    expression_is_child body
    || List.exists clauses ~f:(function
         | For bindings ->
             List.exists bindings
               ~f:(fun {iterator; pattern; attributes= _} ->
                 pattern_is_child pattern
                 ||
                 match iterator with
                 | Range {start; stop; direction= _} ->
                     expression_is_child start || expression_is_child stop
                 | In seq -> expression_is_child seq )
         | When cond -> expression_is_child cond )

  let check_pat {ctx; ast= pat} =
    let check_extensions = function PPat (p, _) -> p == pat | _ -> false in
    let check_subpat ppat =
      ppat == pat
      ||
      match ppat.ppat_desc with
      | Ppat_constraint (p, _, _) -> p == pat
      | _ -> false
    in
    let check_bindings l =
      List.exists l ~f:(fun {pvb_pat; _} -> check_subpat pvb_pat)
    in
    let check_function_param param =
      match param.pparam_desc with
      | Pparam_val (_, _, _, p) -> p == pat
      | Pparam_newtype _ -> false
    in
    match ctx with
    | Pld (PPat (p1, _)) -> assert (p1 == pat)
    | Pld _ -> assert false
    | Typ ctx -> (
      match ctx.ptyp_desc with
      | Ptyp_extension (_, ext) -> assert (check_extensions ext)
      | _ -> assert false )
    | Td _ -> assert false
    | Kab _ -> assert false
    | Tyv _ -> assert false
    | Pat ctx -> (
        let f pI = pI == pat in
        match ctx.ppat_desc with
        | Ppat_array (_, p1N) | Ppat_list p1N | Ppat_cons p1N ->
            assert (List.exists p1N ~f)
        | Ppat_tuple (p1N, _) | Ppat_unboxed_tuple (p1N, _) ->
            assert (List.exists p1N ~f:(fun (_, p) -> f p))
        | Ppat_record (p1N, _) | Ppat_record_unboxed_product (p1N, _) ->
            assert (List.exists p1N ~f:(fun (_, _, x) -> Option.exists x ~f))
        | Ppat_or l -> assert (List.exists ~f:(fun p -> p == pat) l)
        | Ppat_alias (p1, _)
         |Ppat_constraint (p1, _, _)
         |Ppat_construct (_, Some (_, p1))
         |Ppat_exception p1
         |Ppat_lazy p1
         |Ppat_open (_, p1)
         |Ppat_variant (_, Some p1) ->
            assert (p1 == pat)
        | Ppat_extension (_, ext) -> assert (check_extensions ext)
        | Ppat_any | Ppat_constant _
         |Ppat_construct (_, None)
         |Ppat_interval _ | Ppat_type _ | Ppat_unpack _ | Ppat_var _
         |Ppat_variant (_, None) ->
            assert false )
    | Exp ctx -> (
      match ctx.pexp_desc with
      | Pexp_list_comprehension comp | Pexp_array_comprehension (_, comp) ->
          assert (check_comprehension comp (Pattern pat))
      | Pexp_apply _ | Pexp_array _ | Pexp_list _ | Pexp_assert _
       |Pexp_coerce _ | Pexp_constant _ | Pexp_constraint _
       |Pexp_construct _ | Pexp_field _ | Pexp_unboxed_field _
       |Pexp_ident _ | Pexp_ifthenelse _ | Pexp_lazy _
       |Pexp_letexception _ | Pexp_letmodule _ | Pexp_new _
       |Pexp_newtype _ | Pexp_open _ | Pexp_override _ | Pexp_pack _
       |Pexp_poly _ | Pexp_record _ | Pexp_record_unboxed_product _
       |Pexp_send _ | Pexp_sequence _ | Pexp_setfield _ | Pexp_setinstvar _
       |Pexp_tuple _ | Pexp_unboxed_tuple _ | Pexp_unreachable
       |Pexp_variant _ | Pexp_while _ | Pexp_hole | Pexp_beginend _
       |Pexp_parens _ | Pexp_cons _ | Pexp_letopen _
       |Pexp_indexop_access _ | Pexp_prefix _ | Pexp_infix _ | Pexp_stack _
        ->
          assert false
      | Pexp_extension (_, ext) -> assert (check_extensions ext)
      | Pexp_object {pcstr_self; _} ->
          assert (Option.exists ~f:(fun self_ -> self_ == pat) pcstr_self)
      | Pexp_let ({pvbs_bindings; _}, _) ->
          assert (check_bindings pvbs_bindings)
      | Pexp_letop {let_; ands; _} ->
          let f {pbop_pat; _} = check_subpat pbop_pat in
          assert (f let_ || List.exists ~f ands)
      | Pexp_function cases | Pexp_match (_, cases) | Pexp_try (_, cases) ->
          assert (
            List.exists cases ~f:(function
              | {pc_lhs; _} when pc_lhs == pat -> true
              | _ -> false ) )
      | Pexp_for (p, _, _, _, _) -> assert (p == pat)
      | Pexp_fun (p, _) -> assert (check_function_param p) )
    | Fp ctx -> assert (check_function_param ctx)
    | Vc _ -> assert false
    | Lb x -> assert (x.pvb_pat == pat)
    | Mb _ -> assert false
    | Md _ -> assert false
    | Cl ctx ->
        assert (
          match ctx.pcl_desc with
          | Pcl_fun (_, _, p, _) -> p == pat
          | Pcl_constr _ -> false
          | Pcl_structure {pcstr_self; _} ->
              Option.exists ~f:(fun self_ -> self_ == pat) pcstr_self
          | Pcl_apply _ -> false
          | Pcl_let ({pvbs_bindings; _}, _) -> check_bindings pvbs_bindings
          | Pcl_constraint _ -> false
          | Pcl_extension (_, ext) -> check_extensions ext
          | Pcl_open _ -> false )
    | Cty _ -> assert false
    | Mty _ | Mod _ | Sig _ -> assert false
    | Str str -> (
      match str.pstr_desc with
      | Pstr_value {pvbs_bindings; _} -> assert (check_bindings pvbs_bindings)
      | Pstr_extension ((_, ext), _) -> assert (check_extensions ext)
      | _ -> assert false )
    | Clf {pcf_desc; _} ->
        assert (
          match pcf_desc with
          | Pcf_initializer _ -> false
          | Pcf_val (_, _, _) -> false
          | Pcf_method (_, _, _) -> false
          | Pcf_extension (_, PPat (p, _)) -> p == pat
          | Pcf_extension (_, _) -> false
          | Pcf_inherit _ -> false
          | Pcf_constraint _ -> false
          | Pcf_attribute _ -> false )
    | Ctf _ -> assert false
    | Jkd _ | Top | Tli _ | Rep -> assert false

  let assert_check_pat xpat =
    let dump {ctx; ast= pat} = dump ctx (Pat pat) in
    assert_no_raise ~f:check_pat ~dump xpat

  let check_exp {ctx; ast= exp} =
    let check_extensions = function
      | PPat (_, Some e) -> e == exp
      | PStr [{pstr_desc= Pstr_eval (e, _); _}] -> e == exp
      | _ -> false
    in
    let check_function_param param =
      match param.pparam_desc with
      | Pparam_val (_, _, e, _) -> Option.exists e ~f:(fun x -> x == exp)
      | Pparam_newtype _ -> false
    in
    match ctx with
    | Pld (PPat (_, Some e1)) -> assert (e1 == exp)
    | Pld _ -> assert false
    | Exp ctx -> (
        let f eI = eI == exp in
        let snd_f (_, eI) = eI == exp in
        match ctx.pexp_desc with
        | Pexp_list_comprehension comp | Pexp_array_comprehension (_, comp)
          ->
            assert (check_comprehension comp (Expression exp))
        | Pexp_extension (_, ext) -> assert (check_extensions ext)
        | Pexp_constant _ | Pexp_ident _ | Pexp_new _ | Pexp_pack _
         |Pexp_unreachable | Pexp_hole ->
            assert false
        | Pexp_object _ -> assert false
        | Pexp_let ({pvbs_bindings; _}, e) ->
            assert (
              List.exists pvbs_bindings ~f:(fun {pvb_expr; _} ->
                  pvb_expr == exp )
              || e == exp )
        | Pexp_letop {let_; ands; body} ->
            let f {pbop_exp; _} = pbop_exp == exp in
            assert (f let_ || List.exists ~f ands || body == exp)
        | (Pexp_match (e, _) | Pexp_try (e, _)) when e == exp -> ()
        | Pexp_function cases | Pexp_match (_, cases) | Pexp_try (_, cases)
          ->
            assert (
              List.exists cases ~f:(function
                | {pc_guard= Some g; _} when g == exp -> true
                | {pc_rhs; _} when pc_rhs == exp -> true
                | _ -> false ) )
        | Pexp_fun (param, body) ->
            assert (check_function_param param || body == exp)
        | Pexp_indexop_access {pia_lhs; pia_kind= Builtin idx; pia_rhs; _} ->
            assert (
              pia_lhs == exp || idx == exp
              || Option.value_map pia_rhs ~default:false ~f )
        | Pexp_indexop_access
            {pia_lhs; pia_kind= Dotop (_, _, idx); pia_rhs; _} ->
            assert (
              pia_lhs == exp || List.exists ~f idx
              || Option.value_map pia_rhs ~default:false ~f )
        | Pexp_prefix (_, e) -> assert (f e)
        | Pexp_infix (_, e1, e2) -> assert (f e1 || f e2)
        | Pexp_apply (e0, e1N) ->
            (* FAIL *)
            assert (e0 == exp || List.exists e1N ~f:snd_f)
        | Pexp_tuple e1N | Pexp_unboxed_tuple e1N ->
            assert (List.exists e1N ~f:(fun (_, e) -> f e))
        | Pexp_array (_, e1N) | Pexp_list e1N | Pexp_cons e1N ->
            assert (List.exists e1N ~f)
        | Pexp_construct (_, e) | Pexp_variant (_, e) ->
            assert (Option.exists e ~f)
        | Pexp_record (e1N, e0) | Pexp_record_unboxed_product (e1N, e0) ->
            assert (
              Option.exists e0 ~f
              || List.exists e1N ~f:(fun (_, _, e) -> Option.exists e ~f) )
        | Pexp_assert e
         |Pexp_beginend e
         |Pexp_parens e
         |Pexp_constraint (e, _, _)
         |Pexp_stack e
         |Pexp_coerce (e, _, _)
         |Pexp_field (e, _)
         |Pexp_unboxed_field (e, _)
         |Pexp_lazy e
         |Pexp_letexception (_, e)
         |Pexp_letmodule (_, _, _, e)
         |Pexp_newtype (_, e)
         |Pexp_open (_, e)
         |Pexp_letopen (_, e)
         |Pexp_poly (e, _)
         |Pexp_send (e, _)
         |Pexp_setinstvar (_, e) ->
            assert (e == exp)
        | Pexp_sequence (e1, e2) -> assert (e1 == exp || e2 == exp)
        | Pexp_setfield (e1, _, e2) | Pexp_while (e1, e2) ->
            assert (e1 == exp || e2 == exp)
        | Pexp_ifthenelse (eN, e) ->
            assert (
              List.exists eN ~f:(fun x -> f x.if_cond || f x.if_body)
              || Option.exists e ~f )
        | Pexp_for (_, e1, e2, _, e3) ->
            assert (e1 == exp || e2 == exp || e3 == exp)
        | Pexp_override e1N -> assert (List.exists e1N ~f:snd_f) )
    | Fp ctx -> assert (check_function_param ctx)
    | Vc _ -> assert false
    | Lb x -> assert (x.pvb_expr == exp)
    | Mb _ -> assert false
    | Md _ -> assert false
    | Str str -> (
      match str.pstr_desc with
      | Pstr_eval (e0, _) -> assert (e0 == exp)
      | Pstr_value {pvbs_bindings; _} ->
          assert (
            List.exists pvbs_bindings ~f:(fun {pvb_expr; _} ->
                pvb_expr == exp ) )
      | Pstr_extension ((_, ext), _) -> assert (check_extensions ext)
      | Pstr_primitive _ | Pstr_type _ | Pstr_typext _ | Pstr_kind_abbrev _
       |Pstr_exception _ | Pstr_module _ | Pstr_recmodule _
       |Pstr_modtype _ | Pstr_open _ | Pstr_class _ | Pstr_class_type _
       |Pstr_include _ | Pstr_attribute _ ->
          assert false )
    | Mod {pmod_desc= Pmod_unpack (e1, _, _); _} -> assert (e1 == exp)
    | Cl ctx ->
        let rec loop ctx =
          match ctx.pcl_desc with
          | Pcl_fun (_, eopt, _, e) ->
              Option.exists eopt ~f:(fun e -> e == exp) || loop e
          | Pcl_constr _ -> false
          | Pcl_structure _ -> false
          | Pcl_apply (_, l) -> List.exists l ~f:(fun (_, e) -> e == exp)
          | Pcl_let ({pvbs_bindings; _}, _) ->
              List.exists pvbs_bindings ~f:(fun {pvb_expr; _} ->
                  pvb_expr == exp )
          | Pcl_constraint _ -> false
          | Pcl_extension _ -> false
          | Pcl_open _ -> false
        in
        assert (loop ctx)
    | Cty _ -> assert false
    | Ctf _ -> assert false
    | Clf {pcf_desc; _} ->
        assert (
          match pcf_desc with
          | Pcf_initializer e -> e == exp
          | Pcf_val (_, _, Cfk_concrete (_, e)) ->
              let rec loop x =
                x == exp
                ||
                match x with
                | {pexp_desc= Pexp_constraint (e, _, _); _} -> loop e
                | _ -> false
              in
              loop e
          | Pcf_val (_, _, Cfk_virtual _) -> false
          | Pcf_method
              (_, _, Cfk_concrete (_, {pexp_desc= Pexp_poly (e, _); _}))
           |Pcf_method (_, _, Cfk_concrete (_, e)) ->
              let rec loop x =
                x == exp
                ||
                match x with
                | {pexp_desc= Pexp_newtype (_, e); _} -> loop e
                | {pexp_desc= Pexp_constraint (e, _, _); _} -> loop e
                | {pexp_desc= Pexp_fun (_, e); _} -> loop e
                | _ -> false
              in
              loop e
          | Pcf_method (_, _, Cfk_virtual _) -> false
          | Pcf_extension (_, ext) -> check_extensions ext
          | Pcf_inherit _ -> false
          | Pcf_constraint _ -> false
          | Pcf_attribute _ -> false )
    | Jkd _ | Mod _ | Top | Tli _ | Typ _ | Tyv _ | Pat _ | Mty _ | Sig _
     |Td _ | Kab _ | Rep ->
        assert false

  let assert_check_exp xexp =
    let dump {ctx; ast= exp} = dump ctx (Exp exp) in
    assert_no_raise ~f:check_exp ~dump xexp

  let rec is_simple (c : Conf.t) width ({ast= exp; _} as xexp) =
    let ctx = Exp exp in
    match exp.pexp_desc with
    | Pexp_constant _ -> Exp.is_trivial exp
    | Pexp_field _ | Pexp_unboxed_field _ | Pexp_ident _ | Pexp_send _
     |Pexp_construct (_, None)
     |Pexp_variant (_, None) ->
        true
    | Pexp_cons l ->
        List.for_all l ~f:(fun e -> is_simple c width (sub_exp ~ctx e))
        && fit_margin c (width xexp)
    | Pexp_construct (_, Some e0) | Pexp_variant (_, Some e0) ->
        Exp.is_trivial e0
    | Pexp_array (_, e1N) | Pexp_list e1N ->
        List.for_all e1N ~f:Exp.is_trivial && fit_margin c (width xexp)
    | Pexp_tuple e1N | Pexp_unboxed_tuple e1N ->
        List.for_all e1N ~f:(fun (_, e) -> Exp.is_trivial e)
        && fit_margin c (width xexp)
    | Pexp_record (e1N, e0) | Pexp_record_unboxed_product (e1N, e0) ->
        Option.for_all e0 ~f:Exp.is_trivial
        && List.for_all e1N ~f:(fun (_, c, eo) ->
               Option.is_none c && Option.for_all eo ~f:Exp.is_trivial )
        && fit_margin c (width xexp)
    | Pexp_indexop_access {pia_lhs; pia_kind; pia_rhs= None; _} ->
        Exp.is_trivial pia_lhs
        && ( match pia_kind with
           | Builtin idx -> Exp.is_trivial idx
           | Dotop (_, _, idx) -> List.for_all idx ~f:Exp.is_trivial )
        && fit_margin c (width xexp)
    | Pexp_prefix (_, e) -> Exp.is_trivial e && fit_margin c (width xexp)
    | Pexp_infix ({txt= ":="; _}, _, _) -> false
    | Pexp_infix (_, e1, e2) ->
        Exp.is_trivial e1 && Exp.is_trivial e2 && fit_margin c (width xexp)
    | Pexp_apply (e0, e1N) ->
        Exp.is_trivial e0
        && List.for_all e1N ~f:(snd >> Exp.is_trivial)
        && fit_margin c (width xexp)
    | Pexp_extension (_, PStr [{pstr_desc= Pstr_eval (e0, []); _}]) ->
        is_simple c width (sub_exp ~ctx e0)
    | Pexp_extension (_, (PStr [] | PTyp _)) -> true
    | _ -> false

  (** [prec_ctx {ctx; ast}] is the precedence of the context of [ast] within
      [ctx], where [ast] is an immediate sub-term (modulo syntactic sugar) of
      [ctx]. Also returns whether [ast] is the left, right, or neither child
      of [ctx]. Meaningful for binary operators, otherwise returns [None]. *)
  let prec_ctx ctx =
    let open Prec in
    let open Assoc in
    let is_tuple_lvl1_in_constructor ty = function
      | {pcd_args= Pcstr_tuple t1N; _} ->
          List.exists t1N ~f:(fun carg -> carg.pca_type |> phys_equal ty)
      | _ -> false
    in
    let is_tuple_lvl1_in_ext_constructor ty = function
      | {pext_kind= Pext_decl (_, Pcstr_tuple t1N, _); _} ->
          List.exists t1N ~f:(fun carg -> carg.pca_type |> phys_equal ty)
      | _ -> false
    in
    let constructor_cxt_prec_of_inner = function
      | {ptyp_desc= Ptyp_arrow _; _} -> Some (Apply, Non)
      | {ptyp_desc= Ptyp_tuple _; _} -> Some (InfixOp3, Non)
      | _ -> None
    in
    match ctx with
    | { ctx= Td {ptype_kind= Ptype_variant v; _}
      ; ast= Typ ({ptyp_desc= Ptyp_arrow _ | Ptyp_tuple _; _} as typ) }
      when List.exists v ~f:(is_tuple_lvl1_in_constructor typ) ->
        constructor_cxt_prec_of_inner typ
    | { ctx=
          ( Str {pstr_desc= Pstr_typext {ptyext_constructors= l; _}; _}
          | Sig {psig_desc= Psig_typext {ptyext_constructors= l; _}; _} )
      ; ast= Typ ({ptyp_desc= Ptyp_arrow _ | Ptyp_tuple _; _} as typ)
      ; _ }
      when List.exists l ~f:(is_tuple_lvl1_in_ext_constructor typ) ->
        constructor_cxt_prec_of_inner typ
    | { ctx=
          ( Str {pstr_desc= Pstr_exception {ptyexn_constructor= constr; _}; _}
          | Sig {psig_desc= Psig_exception {ptyexn_constructor= constr; _}; _}
          | Exp {pexp_desc= Pexp_letexception (constr, _); _} )
      ; ast= Typ ({ptyp_desc= Ptyp_tuple _ | Ptyp_arrow _; _} as typ) }
      when is_tuple_lvl1_in_ext_constructor typ constr ->
        constructor_cxt_prec_of_inner typ
    | {ctx= Str _; ast= Typ _; _} -> None
    | {ctx= Typ {ptyp_desc; _}; ast= Typ typ; _} -> (
      match ptyp_desc with
      | Ptyp_arrow (t, _, _) ->
          let assoc =
            if List.exists t ~f:(fun x -> x.pap_type == typ) then Left
            else Right
          in
          Some (MinusGreater, assoc)
      | Ptyp_tuple _ | Ptyp_unboxed_tuple _ -> Some (InfixOp3, Non)
      | Ptyp_alias _ -> Some (As, Non)
      | Ptyp_constr (_, _ :: _ :: _) -> Some (Comma, Non)
      | Ptyp_constr _ -> Some (Apply, Non)
      | Ptyp_any | Ptyp_var _ | Ptyp_object _ | Ptyp_class _
       |Ptyp_variant _ | Ptyp_poly _ | Ptyp_package _ | Ptyp_extension _ ->
          None
      | Ptyp_constr_unboxed (_, _ :: _ :: _) -> Some (Comma, Non)
      | Ptyp_constr_unboxed _ -> Some (Apply, Non) )
    | {ctx= Cty {pcty_desc; _}; ast= Typ typ; _} -> (
      match pcty_desc with
      | Pcty_constr (_, _ :: _ :: _) -> Some (Comma, Non)
      | Pcty_arrow (t, _) ->
          let assoc =
            if List.exists t ~f:(fun x -> x.pap_type == typ) then Left
            else Right
          in
          Some (MinusGreater, assoc)
      | _ -> None )
    | {ctx= Cty {pcty_desc; _}; ast= Cty typ; _} -> (
      match pcty_desc with
      | Pcty_arrow (_, t2) ->
          Some (MinusGreater, if t2 == typ then Right else Left)
      | _ -> None )
    | {ast= Cty _; _} -> None
    | {ast= Typ _; _} -> None
    | {ctx= Exp {pexp_desc; _}; ast= Exp exp} -> (
      match pexp_desc with
      | Pexp_tuple ((_, e0) :: _ as exps)
       |Pexp_unboxed_tuple ((_, e0) :: _ as exps) ->
          (* Jane Street: Here we pretend tuple elements with labels have the
             same precedence as function arguments, because they need to be
             parenthesized in the same cases. *)
          let lr = if exp == e0 then Left else Right in
          let prec =
            List.find_map exps ~f:(fun (l, e) ->
                if exp == e then
                  match l with Some _ -> Some Apply | None -> Some Comma
                else None )
          in
          let prec = match prec with Some p -> p | None -> Comma in
          Some (prec, lr)
      | Pexp_cons l ->
          Some (ColonColon, if exp == List.last_exn l then Right else Left)
      | Pexp_construct
          ({txt= Lident "[]"; _}, Some {pexp_desc= Pexp_tuple [_; _]; _}) ->
          Some (Semi, Non)
      | Pexp_array _ | Pexp_list _ | Pexp_list_comprehension _
       |Pexp_array_comprehension _ ->
          Some (Semi, Non)
      | Pexp_construct (_, Some _)
       |Pexp_assert _ | Pexp_lazy _
       |Pexp_variant (_, Some _) ->
          Some (Apply, Non)
      | Pexp_indexop_access {pia_lhs= lhs; pia_rhs= rhs; _} -> (
          if lhs == exp then Some (Dot, Left)
          else
            match rhs with
            | Some e when e == exp -> Some (LessMinus, Right)
            | _ -> Some (Low, Left) )
      | Pexp_prefix ({txt= i; loc}, _) -> (
        match i with
        | "~-" | "~-." | "~+" | "~+." ->
            if
              loc.loc_end.pos_cnum - loc.loc_start.pos_cnum
              = String.length i - 1
            then Some (UMinus, Non)
            else Some (High, Non)
        | _ -> (
          match i.[0] with
          | '!' | '?' | '~' -> Some (High, Non)
          | _ -> Some (Apply, Non) ) )
      | Pexp_infix ({txt= i; _}, e1, _) -> (
          let child = if e1 == exp then Left else Right in
          match (i.[0], i) with
          | _, ":=" -> Some (ColonEqual, child)
          | _, ("or" | "||") -> Some (BarBar, child)
          | _, ("&" | "&&") -> Some (AmperAmper, child)
          | ('=' | '<' | '>' | '|' | '&' | '$'), _ | _, "!=" ->
              Some (InfixOp0, child)
          | ('@' | '^'), _ -> Some (InfixOp1, child)
          | ('+' | '-'), _ -> Some (InfixOp2, child)
          | '*', _ when String.(i <> "*") && Char.(i.[1] = '*') ->
              Some (InfixOp4, child)
          | ('*' | '/' | '%'), _ | _, ("lor" | "lxor" | "mod" | "land") ->
              Some (InfixOp3, child)
          | _, ("lsl" | "lsr" | "asr") -> Some (InfixOp4, child)
          | '#', _ -> Some (HashOp, child)
          | _ -> Some (Apply, child) )
      | Pexp_apply _ -> Some (Apply, Non)
      | Pexp_setfield (e0, _, _) when e0 == exp -> Some (Dot, Left)
      | Pexp_setfield (_, _, e0) when e0 == exp -> Some (LessMinus, Non)
      | Pexp_setinstvar _ -> Some (LessMinus, Non)
      | Pexp_field _ | Pexp_unboxed_field _ -> Some (Dot, Left)
      (* We use [Dot] so [x#y] has the same precedence as [x.y], it is
         different to what is done in the parser, but it is intended. *)
      | Pexp_send _ -> Some (Dot, Left)
      | _ -> None )
    | {ctx= Cl {pcl_desc; _}; ast= Cl _ | Exp _} -> (
      match pcl_desc with Pcl_apply _ -> Some (Apply, Non) | _ -> None )
    | { ctx= Exp _
      ; ast=
          ( Pld _ | Top | Tli _ | Kab _ | Tyv _ | Pat _ | Cl _ | Mty _
          | Mod _ | Sig _ | Str _ | Clf _ | Ctf _ | Rep | Mb _ | Md _ | Jkd _
            ) }
     |{ctx= Fp _; ast= _}
     |{ctx= _; ast= Fp _}
     |{ctx= Vc _; ast= _}
     |{ctx= _; ast= Vc _}
     |{ctx= Lb _; ast= _}
     |{ctx= _; ast= Lb _}
     |{ctx= Td _; ast= _}
     |{ctx= _; ast= Td _}
     |{ ctx= Cl _
      ; ast=
          ( Pld _ | Top | Tli _ | Tyv _ | Kab _ | Pat _ | Mty _ | Mod _
          | Sig _ | Str _ | Clf _ | Ctf _ | Rep | Mb _ | Md _ | Jkd _ ) }
     |{ ctx=
          ( Pld _ | Top | Tli _ | Typ _ | Tyv _ | Kab _ | Cty _ | Pat _
          | Mty _ | Mod _ | Sig _ | Str _ | Clf _ | Ctf _ | Rep | Mb _ | Md _
          | Jkd _ )
      ; ast=
          ( Pld _ | Top | Tli _ | Tyv _ | Kab _ | Pat _ | Exp _ | Cl _
          | Mty _ | Mod _ | Sig _ | Str _ | Clf _ | Ctf _ | Rep | Mb _ | Md _
          | Jkd _ ) } ->
        None

  (** [prec_ast ast] is the precedence of [ast]. Meaningful for binary
      operators, otherwise returns [None]. *)
  let rec prec_ast =
    let open Prec in
    function
    | Pld _ -> None
    | Typ {ptyp_desc; _} -> (
      match ptyp_desc with
      | Ptyp_package _ -> Some Low
      | Ptyp_arrow _ -> Some MinusGreater
      | Ptyp_tuple _ | Ptyp_unboxed_tuple _ -> Some InfixOp3
      | Ptyp_alias _ -> Some As
      | Ptyp_any | Ptyp_var _ | Ptyp_constr _ | Ptyp_object _
       |Ptyp_class _ | Ptyp_variant _ | Ptyp_poly _ | Ptyp_extension _ ->
          None
      | Ptyp_constr_unboxed _ -> None )
    | Td _ -> None
    | Tyv _ -> None
    | Kab _ -> None
    | Cty {pcty_desc; _} -> (
      match pcty_desc with Pcty_arrow _ -> Some MinusGreater | _ -> None )
    | Exp {pexp_desc; _} -> (
      match pexp_desc with
      | Pexp_tuple _ | Pexp_unboxed_tuple _ -> Some Comma
      | Pexp_cons _ -> Some ColonColon
      | Pexp_construct (_, Some _) -> Some Apply
      | Pexp_constant
          {pconst_desc= Pconst_integer (i, _) | Pconst_float (i, _); _} -> (
        match i.[0] with '-' | '+' -> Some UMinus | _ -> Some Atomic )
      | Pexp_constant
          { pconst_desc=
              ( Pconst_unboxed_integer (sign, _, _)
              | Pconst_unboxed_float (sign, _, _) )
          ; _ } -> (
        match sign with Negative -> Some UMinus | Positive -> Some Atomic )
      | Pexp_indexop_access {pia_rhs= rhs; _} -> (
        match rhs with Some _ -> Some LessMinus | _ -> Some Dot )
      | Pexp_prefix ({txt= i; loc; _}, _) -> (
        match i with
        | "~-" | "~-." | "~+." | "~+" ->
            if
              loc.loc_end.pos_cnum - loc.loc_start.pos_cnum
              = String.length i - 1
            then Some UMinus
            else Some High
        | "!=" -> Some Apply
        | _ -> (
          match i.[0] with '!' | '?' | '~' -> Some High | _ -> Some Apply ) )
      | Pexp_infix ({txt= i; _}, _, _) -> (
        match (i.[0], i) with
        | _, ":=" -> Some ColonEqual
        | _, ("or" | "||") -> Some BarBar
        | _, ("&" | "&&") -> Some AmperAmper
        | ('=' | '<' | '>' | '|' | '&' | '$'), _ | _, "!=" -> Some InfixOp0
        | ('@' | '^'), _ -> Some InfixOp1
        | ('+' | '-'), _ -> Some InfixOp2
        | '*', _ when String.(i <> "*") && Char.(i.[1] = '*') ->
            Some InfixOp4
        | ('*' | '/' | '%'), _ | _, ("lor" | "lxor" | "mod" | "land") ->
            Some InfixOp3
        | _, ("lsl" | "lsr" | "asr") -> Some InfixOp4
        | '#', _ -> Some HashOp
        | _ -> Some Apply )
      | Pexp_apply _ -> Some Apply
      | Pexp_assert _ | Pexp_lazy _ | Pexp_for _
       |Pexp_variant (_, Some _)
       |Pexp_while _ | Pexp_new _ | Pexp_object _ ->
          Some Apply
      | Pexp_extension (ext, PStr [{pstr_desc= Pstr_eval (e, _); _}])
        when Source.extension_using_sugar ~name:ext ~payload:e.pexp_loc ->
          prec_ast (Exp e)
      | Pexp_setfield _ -> Some LessMinus
      | Pexp_setinstvar _ -> Some LessMinus
      | Pexp_field _ | Pexp_unboxed_field _ -> Some Dot
      | Pexp_send _ -> Some Dot
      | _ -> None )
    | Fp _ -> None
    | Vc _ -> None
    | Lb _ -> None
    | Cl c -> (
      match c.pcl_desc with
      | Pcl_apply _ -> Some Apply
      | Pcl_structure _ -> Some Apply
      | _ -> None )
    | Top | Pat _ | Mty _ | Mod _ | Sig _ | Str _ | Tli _ | Clf _ | Ctf _
     |Rep | Mb _ | Md _ | Jkd _ ->
        None

  (** [ambig_prec {ctx; ast}] holds when [ast] is ambiguous in its context
      [ctx], indicating that [ast] should be parenthesized. Meaningful for
      binary operators, otherwise returns [None] if [ctx] has no precedence
      or [Some None] if [ctx] does but [ast] does not. *)
  let ambig_prec ({ast; _} as xast) =
    match prec_ctx xast with
    | Some (prec_ctx, which_child) -> (
      match prec_ast ast with
      | Some prec_ast ->
          let ambiguous =
            match Prec.compare prec_ctx prec_ast with
            | 0 ->
                (* which child and associativity match: no parens *)
                (* which child and assoc conflict: add parens *)
                Assoc.equal which_child Non
                || not (Assoc.equal (Assoc.of_prec prec_ast) which_child)
            (* add parens only when the context has a higher prec than ast *)
            | cmp -> cmp >= 0
          in
          if ambiguous then `Ambiguous else `Non_ambiguous
      | None -> `No_prec_ast )
    | None -> `No_prec_ctx

  (** [parenze_typ {ctx; ast}] holds when type [ast] should be parenthesized
      in context [ctx]. *)
  let parenze_typ ({ctx; ast= typ} as xtyp) =
    assert_check_typ xtyp ;
    match xtyp with
    | {ast= {ptyp_desc= Ptyp_package _; _}; _} -> true
    | {ast= {ptyp_desc= Ptyp_alias _; _}; ctx= Typ _} -> true
    | { ast= {ptyp_desc= Ptyp_arrow _ | Ptyp_tuple _; _}
      ; ctx= Typ {ptyp_desc= Ptyp_class _; _} } ->
        true
    | { ast= {ptyp_desc= Ptyp_alias _; _}
      ; ctx=
          ( Str {pstr_desc= Pstr_typext _; _}
          | Sig {psig_desc= Psig_typext _; _} ) } ->
        true
    | { ast= {ptyp_desc= Ptyp_alias _; _}
      ; ctx= Td {ptype_kind= Ptype_variant l; _} }
      when List.exists l ~f:(fun c ->
               match c.pcd_args with
               | Pcstr_tuple l ->
                   List.exists l ~f:(fun carg ->
                       carg.pca_type |> phys_equal typ )
               | _ -> false ) ->
        true
    | { ast= {ptyp_desc= Ptyp_alias _ | Ptyp_arrow _ | Ptyp_tuple _; _}
      ; ctx=
          ( Str {pstr_desc= Pstr_exception _; _}
          | Sig {psig_desc= Psig_exception _; _} ) } ->
        true
    | {ast= {ptyp_desc= Ptyp_arrow _; ptyp_attributes= attrs; _}; _}
      when List.exists attrs ~f:(fun a ->
               String.equal a.attr_name.txt "extension.curry" ) ->
        true
    | { ast= {ptyp_desc= Ptyp_poly _; _}
      ; ctx= Typ {ptyp_desc= Ptyp_arrow _; _} } ->
        true
    | { ast= {ptyp_desc= Ptyp_var (_, _); _}
      ; ctx= Typ {ptyp_desc= Ptyp_constr (_, args); _} }
      when List.length args > 1 ->
        (* Type variables and _ do not need parens when they appear as an
           argument to a multi-parameter type constructor, even if they have
           layout annotations. *)
        false
    | {ast= {ptyp_desc= Ptyp_var (_, l); _}; ctx= _} when Option.is_some l ->
        true
    | { ast= {ptyp_desc= Ptyp_tuple ((Some _, _) :: _); _}
      ; ctx= Typ {ptyp_desc= Ptyp_arrow (args, _, _); _} }
      when List.exists args ~f:(fun arg -> arg.pap_type == typ) ->
        true
    | _ -> (
      match ambig_prec (sub_ast ~ctx (Typ typ)) with
      | `Ambiguous -> true
      | _ -> false )

  (** [parenze_cty {ctx; ast}] holds when class type [ast] should be
      parenthesized in context [ctx]. *)
  let parenze_cty ({ctx; ast= cty} as xcty) =
    assert_check_cty xcty ;
    match ambig_prec (sub_ast ~ctx (Cty cty)) with
    | `Ambiguous -> true
    | _ -> false

  (** [parenze_mty {ctx; ast}] holds when module type [ast] should be
      parenthesized in context [ctx]. *)
  let parenze_mty {ctx; ast= mty} =
    Mty.has_trailing_attributes mty
    ||
    match (ctx, mty.pmty_desc) with
    | Mty {pmty_desc= Pmty_with _; _}, Pmty_with _ -> true
    | Mty {pmty_desc= Pmty_strengthen _; _}, Pmty_strengthen _ -> true
    | _ -> false

  (** [parenze_mod {ctx; ast}] holds when module expr [ast] should be
      parenthesized in context [ctx]. *)
  let parenze_mod {ctx; ast= m} =
    Mod.has_trailing_attributes m
    ||
    match (ctx, m.pmod_desc) with
    (* The RHS of an application is always parenthesized already. *)
    | Mod {pmod_desc= Pmod_apply (_, x); _}, Pmod_functor _ when m == x ->
        false
    | Mod {pmod_desc= Pmod_apply _ | Pmod_apply_unit _; _}, Pmod_functor _ ->
        true
    | _ -> false

  (* Whether a pattern should be parenthesed if followed by a [:]. *)
  let exposed_right_colon pat =
    match pat.ppat_desc with
    (* Some patterns that are always parenthesed are not mentionned here:
       Ppat_constraint, Ppat_unpack *)
    | Ppat_tuple _ -> true
    | _ -> false

  (** [parenze_pat {ctx; ast}] holds when pattern [ast] should be
      parenthesized in context [ctx]. *)
  let parenze_pat ({ctx; ast= pat} as xpat) =
    assert_check_pat xpat ;
    (fun k -> Pat.has_trailing_attributes pat || k ())
    @@ fun () ->
    match (ctx, pat.ppat_desc) with
    | Pat {ppat_desc= Ppat_cons pl; _}, Ppat_cons _
      when List.last_exn pl == pat ->
        false
    | Pat {ppat_desc= Ppat_cons _; _}, inner -> (
      match inner with
      | Ppat_cons _ -> true
      | Ppat_construct _ | Ppat_record _ | Ppat_record_unboxed_product _
       |Ppat_unboxed_tuple _ | Ppat_variant _ ->
          false
      | _ -> true )
    | Fp {pparam_desc= Pparam_val (_, _, _, _); _}, Ppat_cons _ -> true
    | Pat {ppat_desc= Ppat_construct _; _}, Ppat_cons _ -> true
    | Fp _, Ppat_constraint (_, Some {ptyp_desc= Ptyp_poly _; _}, _) -> true
    | _, Ppat_constraint (_, Some {ptyp_desc= Ptyp_poly _; _}, _) -> false
    | ( Exp {pexp_desc= Pexp_letop _; _}
      , ( Ppat_construct (_, Some _)
        | Ppat_cons _
        | Ppat_variant (_, Some _)
        | Ppat_or _ | Ppat_alias _
        | Ppat_constraint ({ppat_desc= Ppat_any; _}, _, _)
        | Ppat_constraint (_, _, _ :: _) ) ) ->
        true
    | Lb _, Ppat_constraint ({ppat_desc= Ppat_any; _}, _, _) -> true
    | Lb _, Ppat_constraint ({ppat_desc= Ppat_tuple _; _}, _, _) -> false
    | ( Exp {pexp_desc= Pexp_letop _; _}
      , Ppat_constraint ({ppat_desc= Ppat_tuple _; _}, _, _) ) ->
        false
    (* Modes on elements of let-bound tuple patterns require the tuple to be
       parenthesized *)
    | (Str _ | Exp _ | Cl _), Ppat_tuple (els, _)
      when List.exists els ~f:(function
             | _, {ppat_desc= Ppat_constraint (_, None, _ :: _); _} -> true
             | _ -> false ) ->
        true
    (* Modes on let-bound tuple patterns require the tuple to be
       parenthesized *)
    | ( ( Str {pstr_desc= Pstr_value bindings; _}
        | Exp {pexp_desc= Pexp_let (bindings, _); _}
        | Cl {pcl_desc= Pcl_let (bindings, _); _} )
      , Ppat_tuple _ )
      when let binding =
             List.find_exn bindings.pvbs_bindings ~f:(fun binding ->
                 binding.pvb_pat == pat )
           in
           not (List.is_empty binding.pvb_modes) ->
        true
    | _, Ppat_constraint _
     |_, Ppat_unpack _
     |( Pat
          { ppat_desc=
              ( Ppat_alias _ | Ppat_array _ | Ppat_list _ | Ppat_constraint _
              | Ppat_construct _ | Ppat_variant _ )
          ; _ }
      , Ppat_tuple _ )
     |( ( Pat
            { ppat_desc=
                ( Ppat_construct _ | Ppat_exception _ | Ppat_or _
                | Ppat_lazy _ | Ppat_tuple _ | Ppat_unboxed_tuple _
                | Ppat_variant _ | Ppat_list _ )
            ; _ }
        | Exp {pexp_desc= Pexp_fun _; _} )
      , Ppat_alias _ )
     |( Pat {ppat_desc= Ppat_lazy _; _}
      , ( Ppat_construct _ | Ppat_cons _
        | Ppat_variant (_, Some _)
        | Ppat_or _ ) )
     |( Pat
          { ppat_desc=
              ( Ppat_construct _ | Ppat_exception _ | Ppat_tuple _
              | Ppat_unboxed_tuple _ | Ppat_variant _ | Ppat_list _ )
          ; _ }
      , Ppat_or _ )
     |Pat {ppat_desc= Ppat_lazy _; _}, Ppat_tuple _
     |Pat {ppat_desc= Ppat_tuple _ | Ppat_unboxed_tuple _; _}, Ppat_tuple _
     |Pat _, Ppat_lazy _
     |Pat _, Ppat_exception _
     |Exp {pexp_desc= Pexp_fun _; _}, Ppat_or _
     |Cl {pcl_desc= Pcl_fun _; _}, Ppat_variant (_, Some _)
     |Cl {pcl_desc= Pcl_fun _; _}, Ppat_tuple _
     |Cl {pcl_desc= Pcl_fun _; _}, Ppat_construct _
     |Cl {pcl_desc= Pcl_fun _; _}, Ppat_alias _
     |Cl {pcl_desc= Pcl_fun _; _}, Ppat_lazy _
     |Exp {pexp_desc= Pexp_letop _; _}, Ppat_exception _
     |( Exp {pexp_desc= Pexp_fun _; _}
      , ( Ppat_construct _ | Ppat_cons _ | Ppat_lazy _ | Ppat_tuple _
        | Ppat_variant _ ) ) ->
        true
    (* Jane Street: Labeled tuple pattern elements must be parenthesized more
       than normal tuple elements. E.g., {[ match foo with | Some x, Some y
       -> ... ]} is valid but {[ match foo with | Some x, ~l:Some y -> ... ]}
       is not - parens are needed. *)
    | ( Pat {ppat_desc= Ppat_tuple (els, _) | Ppat_unboxed_tuple (els, _); _}
      , _ )
      when List.exists els ~f:(function
             | Some _, pat' -> pat == pat'
             | _ -> false )
           && not (Pat.is_simple_in_parser pat) ->
        true
    | (Str _ | Exp _), Ppat_lazy _ -> true
    | ( Fp _
      , ( Ppat_tuple _ | Ppat_construct _ | Ppat_alias _ | Ppat_variant _
        | Ppat_lazy _ | Ppat_exception _ | Ppat_or _ ) )
     |( Pat {ppat_desc= Ppat_construct _ | Ppat_variant _; _}
      , (Ppat_construct (_, Some _) | Ppat_cons _ | Ppat_variant (_, Some _))
      ) ->
        true
    | _, Ppat_var _ when List.is_empty pat.ppat_attributes -> false
    | ( ( Exp {pexp_desc= Pexp_let ({pvbs_bindings; _}, _); _}
        | Str {pstr_desc= Pstr_value {pvbs_bindings; _}; _} )
      , pat_desc ) -> (
      match pat_desc with
      | Ppat_construct (_, Some _)
       |Ppat_variant (_, Some _)
       |Ppat_cons _ | Ppat_alias _ | Ppat_constraint _ | Ppat_lazy _
       |Ppat_or _ ->
          (* Add disambiguation parentheses that are not necessary. *)
          true
      | _ when exposed_right_colon pat ->
          (* Some patterns must be parenthesed when followed by a colon. *)
          let pvb =
            List.find_exn pvbs_bindings ~f:(fun pvb -> pvb.pvb_pat == pat)
          in
          Option.is_some pvb.pvb_constraint
      | _ -> false )
    | _ -> false

  let marked_parenzed_inner_nested_match =
    let memo = Hashtbl.Poly.create () in
    register_reset (fun () -> Hashtbl.clear memo) ;
    memo

  (** [exposed cls exp] holds if there is a right-most subexpression of [exp]
      which satisfies [Exp.mem_cls cls] and is not parenthesized. *)
  let rec exposed_right_exp =
    (* exponential without memoization *)
    let memo = Hashtbl.Poly.create () in
    register_reset (fun () -> Hashtbl.clear memo) ;
    fun cls exp ->
      let exposed_ () =
        let continue subexp =
          (not (parenze_exp (sub_exp ~ctx:(Exp exp) subexp)))
          && exposed_right_exp cls subexp
        in
        match exp.pexp_desc with
        | Pexp_assert e
         |Pexp_stack e
         |Pexp_construct (_, Some e)
         |Pexp_fun (_, e)
         |Pexp_ifthenelse (_, Some e)
         |Pexp_prefix (_, e)
         |Pexp_infix (_, _, e)
         |Pexp_lazy e
         |Pexp_newtype (_, e)
         |Pexp_open (_, e)
         |Pexp_letopen (_, e)
         |Pexp_sequence (_, e)
         |Pexp_setfield (_, _, e)
         |Pexp_setinstvar (_, e)
         |Pexp_variant (_, Some e) ->
            continue e
        | Pexp_cons l -> continue (List.last_exn l)
        | Pexp_ifthenelse (eN, None) -> continue (List.last_exn eN).if_body
        | Pexp_extension
            ( ext
            , PStr
                [ { pstr_desc= Pstr_eval (({pexp_attributes= []; _} as e), _)
                  ; _ } ] )
          when Source.extension_using_sugar ~name:ext ~payload:e.pexp_loc ->
            continue e
        | Pexp_let (_, e)
         |Pexp_letop {body= e; _}
         |Pexp_letexception (_, e)
         |Pexp_letmodule (_, _, _, e) -> (
          match cls with Match | Then | ThenElse -> continue e | _ -> false )
        | Pexp_match _ when match cls with Then -> true | _ -> false ->
            false
        | Pexp_function cases | Pexp_match (_, cases) | Pexp_try (_, cases)
          ->
            continue (List.last_exn cases).pc_rhs
        | Pexp_apply
            ( { pexp_desc=
                  Pexp_extension ({txt= "extension.local"; _}, PStr [])
              ; _ }
            , [(Nolabel, _)] )
          when match cls with Then -> true | _ -> false ->
            true
        | Pexp_apply (_, args) -> continue (snd (List.last_exn args))
        | Pexp_tuple es -> continue (snd (List.last_exn es))
        | Pexp_unboxed_tuple _ -> false
        | Pexp_array _ | Pexp_list _ | Pexp_coerce _ | Pexp_constant _
         |Pexp_constraint _
         |Pexp_construct (_, None)
         |Pexp_extension _ | Pexp_field _ | Pexp_unboxed_field _
         |Pexp_for _ | Pexp_ident _ | Pexp_new _ | Pexp_object _
         |Pexp_override _ | Pexp_pack _ | Pexp_poly _ | Pexp_record _
         |Pexp_record_unboxed_product _ | Pexp_send _ | Pexp_unreachable
         |Pexp_variant (_, None)
         |Pexp_hole | Pexp_while _ | Pexp_beginend _ | Pexp_parens _
         |Pexp_indexop_access _ | Pexp_list_comprehension _
         |Pexp_array_comprehension _ ->
            false
      in
      Exp.mem_cls cls exp
      || Hashtbl.find_or_add memo (cls, exp) ~default:exposed_

  and exposed_right_cl =
    let memo = Hashtbl.Poly.create () in
    register_reset (fun () -> Hashtbl.clear memo) ;
    fun cls cl ->
      let exposed_ () =
        match cl.pcl_desc with
        | Pcl_apply (_, args) ->
            let exp = snd (List.last_exn args) in
            (not (parenze_exp (sub_exp ~ctx:(Cl cl) exp)))
            && exposed_right_exp cls exp
        | Pcl_fun (_, _, _, e) ->
            (not (parenze_cl (sub_cl ~ctx:(Cl cl) e)))
            && exposed_right_cl cls e
        | _ -> false
      in
      Cl.mem_cls cls cl
      || Hashtbl.find_or_add memo (cls, cl) ~default:exposed_

  and mark_parenzed_inner_nested_match exp =
    let exposed_ () =
      let continue subexp =
        if not (parenze_exp (sub_exp ~ctx:(Exp exp) subexp)) then
          mark_parenzed_inner_nested_match subexp ;
        false
      in
      match exp.pexp_desc with
      | Pexp_assert e
       |Pexp_stack e
       |Pexp_construct (_, Some e)
       |Pexp_ifthenelse (_, Some e)
       |Pexp_prefix (_, e)
       |Pexp_infix (_, _, e)
       |Pexp_lazy e
       |Pexp_newtype (_, e)
       |Pexp_open (_, e)
       |Pexp_letopen (_, e)
       |Pexp_fun (_, e)
       |Pexp_sequence (_, e)
       |Pexp_setfield (_, _, e)
       |Pexp_setinstvar (_, e)
       |Pexp_variant (_, Some e) ->
          continue e
      | Pexp_cons l -> continue (List.last_exn l)
      | Pexp_let (_, e)
       |Pexp_letop {body= e; _}
       |Pexp_letexception (_, e)
       |Pexp_letmodule (_, _, _, e) ->
          continue e
      | Pexp_ifthenelse (eN, None) -> continue (List.last_exn eN).if_body
      | Pexp_extension (ext, PStr [{pstr_desc= Pstr_eval (e, _); _}])
        when Source.extension_using_sugar ~name:ext ~payload:e.pexp_loc -> (
        match e.pexp_desc with
        | Pexp_function cases | Pexp_match (_, cases) | Pexp_try (_, cases)
          ->
            List.iter cases ~f:(fun case ->
                mark_parenzed_inner_nested_match case.pc_rhs ) ;
            true
        | _ -> continue e )
      | Pexp_function cases | Pexp_match (_, cases) | Pexp_try (_, cases) ->
          List.iter cases ~f:(fun case ->
              mark_parenzed_inner_nested_match case.pc_rhs ) ;
          true
      | Pexp_indexop_access {pia_rhs= rhs; _} -> (
        match rhs with Some e -> continue e | None -> false )
      | Pexp_apply (_, args) -> continue (snd (List.last_exn args))
      | Pexp_tuple es -> continue (snd (List.last_exn es))
      | Pexp_unboxed_tuple _ -> false
      | Pexp_array _ | Pexp_list _ | Pexp_coerce _ | Pexp_constant _
       |Pexp_constraint _
       |Pexp_construct (_, None)
       |Pexp_extension _ | Pexp_field _ | Pexp_unboxed_field _ | Pexp_for _
       |Pexp_ident _ | Pexp_new _ | Pexp_object _ | Pexp_override _
       |Pexp_pack _ | Pexp_poly _ | Pexp_record _
       |Pexp_record_unboxed_product _ | Pexp_send _ | Pexp_unreachable
       |Pexp_variant (_, None)
       |Pexp_hole | Pexp_while _ | Pexp_beginend _ | Pexp_parens _
       |Pexp_list_comprehension _ | Pexp_array_comprehension _ ->
          false
    in
    Hashtbl.find_or_add marked_parenzed_inner_nested_match exp
      ~default:exposed_
    |> (ignore : bool -> _)

  (** [parenze_exp {ctx; ast}] holds when expression [ast] should be
      parenthesized in context [ctx]. *)
  and parenze_exp ({ctx; ast= exp} as xexp) =
    let parenze () =
      let is_right_infix_arg ctx_desc exp =
        match ctx_desc with
        | Pexp_infix (_, _, e2)
          when e2 == exp
               && Option.value_map ~default:false (prec_ast ctx) ~f:(fun p ->
                      Prec.compare p Apply < 0 ) ->
            true
        | Pexp_tuple e1N | Pexp_unboxed_tuple e1N ->
            snd (List.last_exn e1N) == xexp.ast
        | _ -> false
      in
      match ambig_prec (sub_ast ~ctx (Exp exp)) with
      | `No_prec_ctx -> false (* ctx not apply *)
      | `Ambiguous -> true (* exp is apply and ambig *)
      | _ -> (
        match ctx with
        | Exp {pexp_desc; _} ->
            if is_right_infix_arg pexp_desc exp then Exp.is_sequence exp
            else exposed_right_exp Non_apply exp
        | _ -> exposed_right_exp Non_apply exp )
    in
    let rec ifthenelse pexp_desc =
      match pexp_desc with
      | Pexp_extension (ext, PStr [{pstr_desc= Pstr_eval (e, _); _}])
        when Source.extension_using_sugar ~name:ext ~payload:e.pexp_loc ->
          ifthenelse e.pexp_desc
      | Pexp_let _ | Pexp_match _ | Pexp_try _ -> true
      | _ -> false
    in
    let exp_in_sequence lhs rhs exp =
      match (lhs.pexp_desc, exp.pexp_attributes) with
      | (Pexp_match _ | Pexp_try _), _ :: _ when lhs == exp -> true
      | _, _ :: _ -> false
      | ( Pexp_extension
            ( _
            , PStr
                [ { pstr_desc= Pstr_eval ({pexp_desc= Pexp_sequence _; _}, [])
                  ; _ } ] )
        , _ )
        when lhs == exp ->
          true
      | _ when lhs == exp -> exposed_right_exp Let_match exp
      | _ when rhs == exp -> false
      | _ -> failwith "exp must be lhs or rhs from the parent expression"
    in
    let trailing_attrs_require_parens ctx exp =
      match ctx with
      | Exp {pexp_desc; _} -> (
        match pexp_desc with
        | Pexp_let (_, e)
         |Pexp_letmodule (_, _, _, e)
         |Pexp_letexception (_, e)
         |Pexp_letopen (_, e)
         |Pexp_open (_, e)
         |Pexp_fun (_, e)
         |Pexp_newtype (_, e)
         |Pexp_constraint (e, _, _)
         |Pexp_coerce (e, _, _)
          when e == exp ->
            false
        | Pexp_let (pvbs, _)
          when List.exists pvbs.pvbs_bindings ~f:(fun pvb ->
                   pvb.pvb_expr == exp ) ->
            false
        | _ -> true )
      | _ -> true
    in
    assert_check_exp xexp ;
    Hashtbl.find marked_parenzed_inner_nested_match exp
    |> Option.value ~default:false
    ||
    match (ctx, exp) with
    | Str {pstr_desc= Pstr_eval _; _}, _ -> false
    | ( Exp
          { pexp_desc=
              Pexp_apply
                ( { pexp_desc=
                      Pexp_extension ({txt= extension_local; _}, PStr [])
                  ; _ }
                , [(Nolabel, _)] )
          ; _ }
      , _ )
      when Conf.is_jane_street_local_annotation "local" ~test:extension_local
      ->
        false
    | ( Exp
          { pexp_desc=
              Pexp_apply
                ( { pexp_desc=
                      Pexp_extension ({txt= extension_exclave; _}, PStr [])
                  ; _ }
                , [(Nolabel, _)] )
          ; _ }
      , _ )
      when Conf.is_jane_street_local_annotation "exclave"
             ~test:extension_exclave ->
        false
    | _, {pexp_desc= Pexp_infix _; pexp_attributes= _ :: _; _} -> true
    | ( Exp {pexp_desc= Pexp_infix (_, e1, _); _}
      , { pexp_desc=
            Pexp_apply
              ( { pexp_desc=
                    Pexp_extension ({txt= extension_local; _}, PStr [])
                ; _ }
              , [(Nolabel, _)] )
        ; _ } )
      when e1 == exp
           && ( Conf.is_jane_street_local_annotation "local"
                  ~test:extension_local
              || Conf.is_jane_street_local_annotation "exclave"
                   ~test:extension_local ) ->
        true
    | ( Exp {pexp_desc= Pexp_stack _; _}
      , { pexp_desc=
            ( Pexp_apply _ | Pexp_fun _ | Pexp_function _ | Pexp_lazy _
            | Pexp_new _ | Pexp_tuple _
            | Pexp_construct (_, Some _)
            | Pexp_cons _ | Pexp_infix _ | Pexp_prefix _ | Pexp_stack _
            | Pexp_let _ | Pexp_letop _ | Pexp_letopen _ | Pexp_letmodule _
            | Pexp_send _ | Pexp_setfield _ | Pexp_ifthenelse _
            | Pexp_variant (_, Some _) )
        ; _ } ) ->
        true
    | ( Exp {pexp_desc= Pexp_apply _ | Pexp_construct _; _}
      , {pexp_desc= Pexp_stack _; _} ) ->
        true
    | ( Str
          { pstr_desc=
              Pstr_value
                { pvbs_rec= Nonrecursive
                ; pvbs_bindings= [{pvb_pat= {ppat_desc= Ppat_any; _}; _}]
                ; _ }
          ; _ }
      , _ ) ->
        false
    (* Object fields do not require parens, even with trailing attributes *)
    | Exp {pexp_desc= Pexp_object _; _}, _ -> false
    | _, {pexp_desc= Pexp_object _; pexp_attributes= []; _}
      when Ocaml_version.(compare !ocaml_version Releases.v4_14_0 >= 0) ->
        false
    | ( Exp {pexp_desc= Pexp_construct ({txt= id; _}, _); _}
      , {pexp_attributes= _ :: _; _} )
      when Std_longident.is_infix id ->
        true
    | Exp _, e when Exp.is_symbol e || Exp.is_monadic_binding e -> true
    | Exp {pexp_desc= Pexp_cons _; _}, {pexp_attributes= _ :: _; _} -> true
    | Exp {pexp_desc= Pexp_extension _; _}, {pexp_desc= Pexp_tuple _; _} ->
        false
    | Pld _, {pexp_desc= Pexp_tuple _; _} -> false
    (* Jane Street: Labeled tuple elements must be parenthesized more than
       normal tuple elements. *)
    | Exp {pexp_desc= Pexp_tuple els | Pexp_unboxed_tuple els; _}, _
      when List.exists els ~f:(function
             | Some _, exp' -> exp == exp'
             | _ -> false )
           && not (Exp.is_simple_in_parser exp) ->
        true
    | Cl {pcl_desc= Pcl_apply _; _}, _ -> parenze ()
    | Clf _, _ -> parenze ()
    | Exp {pexp_desc= Pexp_ifthenelse (eN, _); _}, {pexp_desc; _}
      when !parens_ite
           && List.exists eN ~f:(fun x -> x.if_body == exp)
           && ifthenelse pexp_desc ->
        true
    | Exp {pexp_desc= Pexp_ifthenelse (_, Some e); _}, {pexp_desc; _}
      when !parens_ite && e == exp && ifthenelse pexp_desc ->
        true
    | ( Exp {pexp_desc= Pexp_infix (_, _, e1); _}
      , { pexp_desc=
            Pexp_apply ({pexp_desc= Pexp_ident {txt= Lident "not"; _}; _}, _)
        ; _ } )
      when not (e1 == exp) ->
        true
    | ( Exp {pexp_desc= Pexp_apply (e, _); _}
      , {pexp_desc= Pexp_construct _ | Pexp_cons _ | Pexp_variant _; _} )
      when e == exp ->
        true
    | ( Exp {pexp_desc= Pexp_apply (e, _ :: _); _}
      , {pexp_desc= Pexp_prefix _; pexp_attributes= _ :: _; _} )
      when e == exp ->
        true
    | ( Exp {pexp_desc= Pexp_indexop_access {pia_lhs= lhs; _}; _}
      , {pexp_desc= Pexp_construct _ | Pexp_cons _; _} )
      when lhs == exp ->
        true
    | Exp {pexp_desc= Pexp_indexop_access {pia_kind= Builtin idx; _}; _}, _
      when idx == exp ->
        false
    | ( Exp {pexp_desc= Pexp_constraint (e, _, _) | Pexp_coerce (e, _, _); _}
      , {pexp_desc= Pexp_tuple _ | Pexp_match _ | Pexp_try _; _} )
      when e == exp && !ocp_indent_compat ->
        true
    | ( Exp
          { pexp_desc=
              Pexp_indexop_access
                {pia_kind= Dotop (_, _, [idx]); pia_paren= Paren; _}
          ; _ }
      , _ )
      when idx == exp && not (Exp.is_sequence idx) ->
        false
    | ( Exp {pexp_desc= Pexp_prefix (_, e); _}
      , { pexp_desc=
            ( Pexp_indexop_access {pia_lhs= x; _}
            | Pexp_infix (_, x, _)
            | Pexp_apply (_, [(_, x); _]) )
        ; _ } )
      when e == exp && Exp.exposed_left x ->
        true
    (* Integers without suffixes must be parenthesised on the lhs of an
       indexing operator *)
    | ( Exp {pexp_desc= Pexp_indexop_access {pia_lhs= lhs; _}; _}
      , { pexp_desc= Pexp_constant {pconst_desc= Pconst_integer (_, None); _}
        ; _ } )
      when exp == lhs ->
        true
    | ( Exp {pexp_desc= Pexp_field (e, _) | Pexp_unboxed_field (e, _); _}
      , {pexp_desc= Pexp_construct _ | Pexp_cons _; _} )
      when e == exp ->
        true
    | Exp {pexp_desc; _}, _ -> (
        (* This is the old fallthrough case, but we need it for the
           extensions match and the real match, so we factor it out; we have
           to do an external match on [ctx_exp] because the below fallthrough
           case matches on subterms, which can cause exceptions if we break
           into the representation of a language extension. *)
        let fallthrough_case () =
          match exp.pexp_desc with
          | Pexp_list _ | Pexp_array _ | Pexp_list_comprehension _
           |Pexp_array_comprehension _ ->
              false
          | _ ->
              Exp.has_trailing_attributes exp
              && trailing_attrs_require_parens ctx exp
              || parenze ()
        in
        match pexp_desc with
        | Pexp_extension
            ( _
            , PStr
                [ { pstr_desc=
                      Pstr_eval
                        ( { pexp_desc=
                              ( Pexp_function cases
                              | Pexp_match (_, cases)
                              | Pexp_try (_, cases) )
                          ; _ }
                        , _ )
                  ; _ } ] )
         |Pexp_function cases
         |Pexp_match (_, cases)
         |Pexp_try (_, cases) ->
            if !leading_nested_match_parens then
              List.iter cases ~f:(fun {pc_rhs; _} ->
                  mark_parenzed_inner_nested_match pc_rhs ) ;
            List.exists cases ~f:(fun {pc_rhs; _} -> pc_rhs == exp)
            && exposed_right_exp Match exp
        | Pexp_ifthenelse (eN, _)
          when List.exists eN ~f:(fun x -> x.if_cond == exp) ->
            false
        | Pexp_ifthenelse (eN, None) when (List.last_exn eN).if_body == exp
          ->
            exposed_right_exp Then exp
        | Pexp_ifthenelse (eN, _)
          when List.exists eN ~f:(fun x -> x.if_body == exp) ->
            exposed_right_exp ThenElse exp
        | Pexp_ifthenelse (_, Some els) when els == exp ->
            Exp.is_sequence exp
        | Pexp_apply (({pexp_desc= Pexp_new _; _} as exp2), _)
          when exp2 == exp ->
            false
        | Pexp_apply
            ( ( { pexp_desc=
                    Pexp_extension
                      ( _
                      , PStr
                          [ { pstr_desc=
                                Pstr_eval ({pexp_desc= Pexp_new _; _}, [])
                            ; _ } ] )
                ; _ } as exp2 )
            , _ )
          when exp2 == exp ->
            false
        | (Pexp_record (flds, _) | Pexp_record_unboxed_product (flds, _))
          when List.exists flds ~f:(fun (_, _, e0) ->
                   Option.exists e0 ~f:(fun x -> x == exp) ) ->
            exposed_right_exp Non_apply exp
            (* Non_apply is perhaps pessimistic *)
        | Pexp_record (_, Some ({pexp_desc= Pexp_prefix _; _} as e0))
         |Pexp_record_unboxed_product
            (_, Some ({pexp_desc= Pexp_prefix _; _} as e0))
          when e0 == exp ->
            (* don't put parens around [!e] in [{ !e with a; b }] *)
            false
        | Pexp_record
            ( _
            , Some
                ( { pexp_desc=
                      ( Pexp_ident _ | Pexp_constant _ | Pexp_record _
                      | Pexp_constraint _ | Pexp_unboxed_field _
                      | Pexp_field _ )
                  ; _ } as e0 ) )
         |Pexp_record_unboxed_product
            ( _
            , Some
                ( { pexp_desc=
                      ( Pexp_ident _ | Pexp_constant _ | Pexp_record _
                      | Pexp_constraint _ | Pexp_unboxed_field _
                      | Pexp_field _ )
                  ; _ } as e0 ) )
          when e0 == exp ->
            false
        | Pexp_record (_, Some e0)
         |Pexp_record_unboxed_product (_, Some e0)
          when e0 == exp ->
            true
        | Pexp_override fields
          when List.exists fields ~f:(fun (_, e0) -> e0 == exp) ->
            exposed_right_exp Sequence exp
        | Pexp_sequence (lhs, rhs) -> exp_in_sequence lhs rhs exp
        | Pexp_apply (_, args)
          when List.exists args ~f:(fun (_, e0) ->
                   match (e0.pexp_desc, e0.pexp_attributes) with
                   | Pexp_list _, _ :: _ when e0 == exp -> true
                   | Pexp_array _, _ :: _ when e0 == exp -> true
                   | ( ( Pexp_list_comprehension _
                       | Pexp_array_comprehension _ )
                     , _ :: _ )
                     when e0 == exp ->
                       true
                   | _ -> false ) ->
            true
        | _ -> fallthrough_case () )
    | _, {pexp_desc= Pexp_list _; _} -> false
    | _, {pexp_desc= Pexp_array _; _} -> false
    | _, {pexp_desc= Pexp_list_comprehension _; _} -> false
    | _, {pexp_desc= Pexp_array_comprehension _; _} -> false
    | ctx, exp
      when Exp.has_trailing_attributes exp
           && trailing_attrs_require_parens ctx exp ->
        true
    | _ -> false

  (** [parenze_cl {ctx; ast}] holds when class expr [ast] should be
      parenthesized in context [ctx]. *)
  and parenze_cl ({ctx; ast= cl} as xcl) =
    assert_check_cl xcl ;
    match ambig_prec (sub_ast ~ctx (Cl cl)) with
    | `No_prec_ctx -> false
    | `Ambiguous -> true
    | _ -> exposed_right_cl Non_apply cl

  let parenze_nested_exp {ctx; ast= exp} =
    let infix_prec ast =
      match ast with
      | Exp {pexp_desc= Pexp_infix _; _} -> prec_ast ast
      | Exp {pexp_desc= Pexp_apply (e, _); _} when Exp.is_infix e ->
          prec_ast ast
      | Exp {pexp_desc= Pexp_cons _; _} -> prec_ast ast
      | _ -> None
    in
    (* Make the precedence explicit for infix operators *)
    match (infix_prec ctx, infix_prec (Exp exp)) with
    | Some (InfixOp0 | ColonEqual), _ | _, Some (InfixOp0 | ColonEqual) ->
        (* special case for refs update and all InfixOp0 to reduce parens
           noise *)
        false
    | None, _ | _, None -> false
    | Some p1, Some p2 -> not (Prec.equal p1 p2)
end

include In_ctx
include Requires_sub_terms
