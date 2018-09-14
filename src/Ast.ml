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

(** Abstract syntax tree term *)

open Migrate_ast
open Parsetree

let init, register_reset, leading_nested_match_parens =
  let l = ref [] in
  let leading_nested_match_parens = ref false in
  let register f = l := f :: !l in
  let init conf =
    leading_nested_match_parens := conf.Conf.leading_nested_match_parens ;
    List.iter !l ~f:(fun f -> f ())
  in
  (init, register, leading_nested_match_parens)

(** Predicates recognizing special symbol identifiers. *)

let is_prefix_id i =
  match i with
  | "!=" -> false
  | _ -> ( match i.[0] with '!' | '?' | '~' -> true | _ -> false )

let is_prefix exp =
  match exp.pexp_desc with
  | Pexp_ident {txt= Lident i} -> is_prefix_id i
  | _ -> false

let is_infix_id i =
  match (i.[0], i) with
  | ( ( '$' | '%' | '*' | '+' | '-' | '/' | '<' | '=' | '>' | '|' | '&'
      | '@' | '^' | '#' )
    , _ )
   |( _
    , ( "!=" | "land" | "lor" | "lxor" | "mod" | "::" | ":=" | "asr" | "lsl"
      | "lsr" | "or" | "||" ) ) ->
      true
  | _ -> false

let is_infix e =
  match e.pexp_desc with
  | Pexp_ident {txt= Lident i} -> is_infix_id i
  | _ -> false

let parens_kind i =
  let len = String.length i in
  if len <= 2 then None
  else
    let opn, cls = (i.[len - 2], i.[len - 1]) in
    match (opn, cls) with
    | '(', ')' | '[', ']' | '{', '}' ->
        let txt = String.drop_suffix i 2 in
        Some (txt, opn, cls)
    | _ -> None

let index_op_get i =
  match (i.[0], parens_kind i) with
  | '.', Some (s, o, c) -> Some (s, o, c)
  | _ -> None

let index_op_set i =
  match String.chop_suffix i ~suffix:"<-" with
  | None -> None
  | Some i -> (
    match (i.[0], parens_kind i) with
    | '.', Some (s, o, c) -> Some (s, o, c)
    | _ -> None )

let index_op_string = (".", '[', ']')

let index_op_array = (".", '(', ')')

let index_op_bigarray = (".", '{', '}')

let all_args_unlabeled args =
  List.fold_right args ~init:(Some []) ~f:(fun arg acc ->
      match (acc, arg) with
      | Some args, (Asttypes.Nolabel, e) -> Some (e :: args)
      | _ -> None )

let index_op_get_sugar (ident : Longident.t) args =
  match all_args_unlabeled args with
  | None -> None
  | Some args -> (
    match (ident, args) with
    | Ldot (Lident "String", "get"), [_] -> Some (index_op_string, args)
    | Ldot (Lident "Array", "get"), [_] -> Some (index_op_array, args)
    | Ldot (Ldot (Lident "Bigarray", "Array1"), "get"), [_] ->
        Some (index_op_bigarray, args)
    | Ldot (Ldot (Lident "Bigarray", "Array2"), "get"), [_; _] ->
        Some (index_op_bigarray, args)
    | Ldot (Ldot (Lident "Bigarray", "Array3"), "get"), [_; _; _] ->
        Some (index_op_bigarray, args)
    | ( Ldot (Ldot (Lident "Bigarray", "Genarray"), "get")
      , [{pexp_desc= Pexp_array l}] )
      when List.length l > 3 ->
        Some (index_op_bigarray, l)
    | _ -> None )

let index_op_set_sugar (ident : Longident.t) args =
  match all_args_unlabeled args with
  | None -> None
  | Some args -> (
    match (ident, args) with
    | Ldot (Lident "String", "set"), [a1; e] ->
        Some (index_op_string, [a1], e)
    | Ldot (Lident "Array", "set"), [a1; e] -> Some (index_op_array, [a1], e)
    | Ldot (Ldot (Lident "Bigarray", "Array1"), "set"), [a1; e] ->
        Some (index_op_bigarray, [a1], e)
    | Ldot (Ldot (Lident "Bigarray", "Array2"), "set"), [a1; a2; e] ->
        Some (index_op_bigarray, [a1; a2], e)
    | Ldot (Ldot (Lident "Bigarray", "Array3"), "set"), [a1; a2; a3; e] ->
        Some (index_op_bigarray, [a1; a2; a3], e)
    | ( Ldot (Ldot (Lident "Bigarray", "Genarray"), "set")
      , [{pexp_desc= Pexp_array l}; e] )
      when List.length l > 3 ->
        Some (index_op_bigarray, l, e)
    | _ -> None )

let is_index_op exp =
  match exp.pexp_desc with
  | Pexp_ident {txt= Lident i} ->
      Option.is_some (index_op_get i) || Option.is_some (index_op_set i)
  | _ -> false

let is_symbol_id i =
  is_prefix_id i || is_infix_id i
  || Option.is_some (index_op_get i)
  || Option.is_some (index_op_set i)

let is_symbol e = is_prefix e || is_infix e || is_index_op e

(** Predicates recognizing classes of expressions. *)

let is_sequence exp =
  match exp.pexp_desc with
  | Pexp_sequence _
   |Pexp_extension
      ( _
      , PStr [{pstr_desc= Pstr_eval ({pexp_desc= Pexp_sequence _}, []); _}]
      ) ->
      true
  | _ -> false

let rec is_sugared_list exp =
  match exp.pexp_desc with
  | Pexp_construct ({txt= Lident "[]"}, None) -> true
  | Pexp_construct
      ( {txt= Lident "::"}
      , Some
          { pexp_desc= Pexp_tuple [_; ({pexp_attributes= []} as tl)]
          ; pexp_attributes= [] } ) ->
      is_sugared_list tl
  | _ -> false

let doc_atrs atrs =
  let docs, rev_atrs =
    List.fold atrs ~init:([], []) ~f:(fun (docs, rev_atrs) atr ->
        let open Asttypes in
        match atr with
        | ( { txt= ("ocaml.doc" | "ocaml.text") as txt
            ; loc= {loc_ghost= true} }
          , PStr
              [ { pstr_desc=
                    Pstr_eval
                      ( { pexp_desc=
                            Pexp_constant (Pconst_string (doc, None))
                        ; pexp_loc= loc
                        ; pexp_attributes= [] }
                      , [] ) } ] ) -> (
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

module type Module_item = sig
  type t

  val break_between : Cmts.t -> t * Conf.t -> t * Conf.t -> bool
end

module Structure_item : Module_item with type t = structure_item = struct
  type t = structure_item

  let has_doc itm =
    match itm.pstr_desc with
    | Pstr_attribute atr -> Option.is_some (fst (doc_atrs [atr]))
    | Pstr_eval (_, atrs)
     |Pstr_value (_, {pvb_attributes= atrs} :: _)
     |Pstr_primitive {pval_attributes= atrs}
     |Pstr_type (_, {ptype_attributes= atrs} :: _)
     |Pstr_typext {ptyext_attributes= atrs}
     |Pstr_exception {pext_attributes= atrs}
     |Pstr_recmodule ({pmb_expr= {pmod_attributes= atrs}} :: _)
     |Pstr_modtype {pmtd_attributes= atrs}
     |Pstr_open {popen_attributes= atrs}
     |Pstr_extension (_, atrs)
     |Pstr_class_type ({pci_attributes= atrs} :: _)
     |Pstr_class ({pci_attributes= atrs} :: _) ->
        Option.is_some (fst (doc_atrs atrs))
    | Pstr_include
        {pincl_mod= {pmod_attributes= atrs1}; pincl_attributes= atrs2}
     |Pstr_module {pmb_attributes= atrs1; pmb_expr= {pmod_attributes= atrs2}}
      ->
        Option.is_some (fst (doc_atrs (List.append atrs1 atrs2)))
    | Pstr_value (_, [])
     |Pstr_type (_, [])
     |Pstr_recmodule []
     |Pstr_class_type []
     |Pstr_class [] ->
        false

  let is_simple (itm, c) =
    match c.Conf.module_item_spacing with
    | `Compact ->
        Location.width itm.pstr_loc <= c.Conf.margin
        && Location.is_single_line itm.pstr_loc
    | `Sparse -> (
      match itm.pstr_desc with
      | Pstr_include {pincl_mod= me} | Pstr_module {pmb_expr= me} ->
          let rec is_simple_mod me =
            match me.pmod_desc with
            | Pmod_apply (me1, me2) ->
                is_simple_mod me1 && is_simple_mod me2
            | Pmod_functor (_, _, me) -> is_simple_mod me
            | Pmod_ident _ -> true
            | _ -> false
          in
          is_simple_mod me
      | Pstr_open _ -> true
      | _ -> false )

  let allow_adjacent (itmI, cI) (itmJ, cJ) =
    match Conf.(cI.module_item_spacing, cJ.module_item_spacing) with
    | `Compact, `Compact -> (
      match (itmI.pstr_desc, itmJ.pstr_desc) with
      | Pstr_eval _, Pstr_eval _
       |Pstr_value _, Pstr_value _
       |Pstr_primitive _, Pstr_primitive _
       |(Pstr_type _ | Pstr_typext _), (Pstr_type _ | Pstr_typext _)
       |Pstr_exception _, Pstr_exception _
       |( (Pstr_module _ | Pstr_recmodule _ | Pstr_open _ | Pstr_include _)
        , (Pstr_module _ | Pstr_recmodule _ | Pstr_open _ | Pstr_include _)
        )
       |Pstr_modtype _, Pstr_modtype _
       |Pstr_class _, Pstr_class _
       |Pstr_class_type _, Pstr_class_type _
       |Pstr_attribute _, Pstr_attribute _
       |Pstr_extension _, Pstr_extension _ ->
          true
      | _ -> false )
    | _ -> true

  let break_between cmts (i1, c1) (i2, c2) =
    Cmts.has_after cmts i1.pstr_loc
    || Cmts.has_before cmts i2.pstr_loc
    || has_doc i1 || has_doc i2
    || (not (is_simple (i1, c1)))
    || (not (is_simple (i2, c2)))
    || not (allow_adjacent (i1, c1) (i2, c2))
end

module Signature_item : Module_item with type t = signature_item = struct
  type t = signature_item

  let has_doc itm =
    match itm.psig_desc with
    | Psig_attribute atr -> Option.is_some (fst (doc_atrs [atr]))
    | Psig_value {pval_attributes= atrs}
     |Psig_type (_, {ptype_attributes= atrs} :: _)
     |Psig_typext {ptyext_attributes= atrs}
     |Psig_exception {pext_attributes= atrs}
     |Psig_modtype {pmtd_attributes= atrs}
     |Psig_open {popen_attributes= atrs}
     |Psig_extension (_, atrs)
     |Psig_class_type ({pci_attributes= atrs} :: _)
     |Psig_class ({pci_attributes= atrs} :: _) ->
        Option.is_some (fst (doc_atrs atrs))
    | Psig_recmodule
        ({pmd_type= {pmty_attributes= atrs1}; pmd_attributes= atrs2} :: _)
     |Psig_include
        {pincl_mod= {pmty_attributes= atrs1}; pincl_attributes= atrs2}
     |Psig_module {pmd_attributes= atrs1; pmd_type= {pmty_attributes= atrs2}}
      ->
        Option.is_some (fst (doc_atrs (List.append atrs1 atrs2)))
    | Psig_type (_, [])
     |Psig_recmodule []
     |Psig_class_type []
     |Psig_class [] ->
        false

  let is_simple (itm, c) =
    match c.Conf.module_item_spacing with
    | `Compact ->
        Location.width itm.psig_loc <= c.Conf.margin
        && Location.is_single_line itm.psig_loc
    | `Sparse -> (
      match itm.psig_desc with
      | Psig_open _ -> true
      | Psig_module {pmd_type= {pmty_desc= Pmty_alias _}} -> true
      | _ -> false )

  let allow_adjacent (itmI, cI) (itmJ, cJ) =
    match Conf.(cI.module_item_spacing, cJ.module_item_spacing) with
    | `Compact, `Compact -> (
      match (itmI.psig_desc, itmJ.psig_desc) with
      | Psig_value _, Psig_value _
       |(Psig_type _ | Psig_typext _), (Psig_type _ | Psig_typext _)
       |Psig_exception _, Psig_exception _
       |( (Psig_module _ | Psig_recmodule _ | Psig_open _ | Psig_include _)
        , (Psig_module _ | Psig_recmodule _ | Psig_open _ | Psig_include _)
        )
       |Psig_modtype _, Psig_modtype _
       |Psig_class _, Psig_class _
       |Psig_class_type _, Psig_class_type _
       |Psig_attribute _, Psig_attribute _
       |Psig_extension _, Psig_extension _ ->
          true
      | _ -> false )
    | _ -> true

  let break_between cmts (i1, c1) (i2, c2) =
    Cmts.has_after cmts i1.psig_loc
    || Cmts.has_before cmts i2.psig_loc
    || has_doc i1 || has_doc i2
    || (not (is_simple (i1, c1)))
    || (not (is_simple (i2, c2)))
    || not (allow_adjacent (i1, c1) (i2, c2))
end

module Expression : Module_item with type t = expression = struct
  type t = expression

  let is_simple (i, c) =
    Poly.(c.Conf.module_item_spacing = `Compact)
    && Location.width i.pexp_loc <= c.Conf.margin
    && Location.is_single_line i.pexp_loc

  let break_between cmts (i1, c1) (i2, c2) =
    Cmts.has_after cmts i1.pexp_loc
    || Cmts.has_before cmts i2.pexp_loc
    || (not (is_simple (i1, c1)))
    || not (is_simple (i2, c2))
end

let may_force_break (c : Conf.t) s =
  let contains_internal_newline s =
    match String.index s '\n' with
    | None -> false
    | Some i when i = String.length s - 1 -> false
    | _ -> true
  in
  match c.break_string_literals with
  | `Newlines -> contains_internal_newline s
  | _ -> false

let rec is_trivial c exp =
  match exp.pexp_desc with
  | Pexp_constant (Pconst_string (s, None)) -> not (may_force_break c s)
  | Pexp_constant _ | Pexp_field _ | Pexp_ident _ | Pexp_send _ -> true
  | Pexp_construct (_, exp) -> Option.for_all exp ~f:(is_trivial c)
  | Pexp_apply (e0, [(_, e1)]) when is_prefix e0 -> is_trivial c e1
  | Pexp_apply ({pexp_desc= Pexp_ident {txt= Lident "not"}}, [(_, e1)]) ->
      is_trivial c e1
  | _ -> false

let has_trailing_attributes_exp {pexp_desc; pexp_attributes} =
  match pexp_desc with
  | Pexp_fun _ | Pexp_function _ | Pexp_ifthenelse _ | Pexp_match _
   |Pexp_newtype _ | Pexp_try _ ->
      false
  | _ ->
      List.exists pexp_attributes ~f:(function
        | {Location.txt= "ocaml.doc" | "ocaml.text"}, _ -> false
        | _ -> true )

let has_trailing_attributes_pat {ppat_desc; ppat_attributes} =
  match ppat_desc with
  | Ppat_construct (_, None)
   |Ppat_constant _ | Ppat_any | Ppat_var _
   |Ppat_variant (_, None)
   |Ppat_record _ | Ppat_array _ | Ppat_type _ | Ppat_unpack _
   |Ppat_extension _ | Ppat_open _ | Ppat_interval _ ->
      false
  | _ ->
      List.exists ppat_attributes ~f:(function
        | {Location.txt= "ocaml.doc" | "ocaml.text"}, _ -> false
        | _ -> true )

let has_trailing_attributes_mty {pmty_desc; pmty_attributes} =
  match pmty_desc with _ ->
    List.exists pmty_attributes ~f:(function
      | {Location.txt= "ocaml.doc" | "ocaml.text"}, _ -> false
      | _ -> true )

let has_trailing_attributes_mod {pmod_desc; pmod_attributes} =
  match pmod_desc with _ ->
    List.exists pmod_attributes ~f:(function
      | {Location.txt= "ocaml.doc" | "ocaml.text"}, _ -> false
      | _ -> true )

(** Ast terms of various forms. *)
module T = struct
  type t =
    | Pld of payload
    | Typ of core_type
    | Cty of class_type
    | Pat of pattern
    | Exp of expression
    | Cl of class_expr
    | Mty of module_type
    | Mod of module_expr
    | Sig of signature_item
    | Str of structure_item
    | Top

  let dump fs = function
    | Pld l -> Format.fprintf fs "Pld:@\n%a" (Printast.payload 0) l
    | Typ t -> Format.fprintf fs "Typ:@\n%a" Pprintast.core_type t
    | Pat p -> Format.fprintf fs "Pat:@\n%a" Pprintast.pattern p
    | Exp e ->
        Format.fprintf fs "Exp:@\n%a@\n@\n%a" Pprintast.expression e
          (Printast.expression 0) e
    | Cl cl ->
        let str =
          let open Ast_helper in
          Str.class_
            [ { pci_virt= Concrete
              ; pci_params= []
              ; pci_name= {txt= ""; loc= Location.none}
              ; pci_expr= cl
              ; pci_loc= Location.none
              ; pci_attributes= [] } ]
        in
        Format.fprintf fs "Cl:@\n%a@\n%a" Pprintast.structure [str]
          Printast.implementation [str]
    | Mty mt ->
        let si =
          let open Ast_helper in
          Sig.modtype (Mtd.mk {txt= ""; loc= Location.none} ~typ:mt)
        in
        Format.fprintf fs "Mty:@\n%a@\n%a" Pprintast.signature [si]
          Printast.interface [si]
    | Cty cty ->
        let si =
          let open Ast_helper in
          Sig.class_type
            [ { pci_virt= Concrete
              ; pci_params= []
              ; pci_name= {txt= ""; loc= Location.none}
              ; pci_expr= cty
              ; pci_loc= Location.none
              ; pci_attributes= [] } ]
        in
        Format.fprintf fs "Cty:@\n%a@\n%a" Pprintast.signature [si]
          Printast.interface [si]
    | Mod m ->
        let m =
          let open Ast_helper in
          Str.module_
            { pmb_name= {txt= ""; loc= Location.none}
            ; pmb_expr= m
            ; pmb_attributes= []
            ; pmb_loc= Location.none }
        in
        Format.fprintf fs "Mod:@\n%a@\n%a" Pprintast.structure [m]
          Printast.implementation [m]
    | Sig s ->
        Format.fprintf fs "Sig:@\n%a@\n%a" Pprintast.signature [s]
          Printast.interface [s]
    | Str s ->
        Format.fprintf fs "Str:@\n%a@\n%a" Pprintast.structure [s]
          Printast.implementation [s]
    | Top -> Format.pp_print_string fs "Top"
end

include T

let break_between cmts (i1, c1) (i2, c2) =
  match (i1, i2) with
  | Str i1, Str i2 -> Structure_item.break_between cmts (i1, c1) (i2, c2)
  | Sig i1, Sig i2 -> Signature_item.break_between cmts (i1, c1) (i2, c2)
  | Exp i1, Exp i2 -> Expression.break_between cmts (i1, c1) (i2, c2)
  | _ -> assert false

(** Precedence levels of Ast terms. *)
type prec =
  | Low
  | Semi
  | LessMinus
  | ColonEqual
  | As
  | Comma
  | MinusGreater
  | BarBar
  | AmperAmper
  | InfixOp0
  | InfixOp1
  | ColonColon
  | InfixOp2
  | InfixOp3
  | InfixOp4
  | UMinus
  | Apply
  | Dot
  | HashOp
  | High
  | Atomic

let string_of_prec = function
  | Low -> "Low"
  | Semi -> "Semi"
  | LessMinus -> "LessMinus"
  | ColonEqual -> "ColonEqual"
  | As -> "As"
  | Comma -> "Comma"
  | MinusGreater -> "MinusGreater"
  | BarBar -> "BarBar"
  | AmperAmper -> "AmperAmper"
  | InfixOp0 -> "InfixOp0"
  | InfixOp1 -> "InfixOp1"
  | ColonColon -> "ColonColon"
  | InfixOp2 -> "InfixOp2"
  | InfixOp3 -> "InfixOp3"
  | InfixOp4 -> "InfixOp4"
  | UMinus -> "UMinus"
  | Apply -> "Apply"
  | Dot -> "Dot"
  | HashOp -> "HashOp"
  | High -> "High"
  | Atomic -> "Atomic"

let _ = string_of_prec

(** Associativities of Ast terms. *)
type assoc = Left | Non | Right

let string_of_assoc = function
  | Left -> "Left"
  | Non -> "Non"
  | Right -> "Right"

let _ = string_of_assoc

(** Compute associativity from precedence, since associativity is uniform
    across precedence levels. *)
let assoc_of_prec = function
  | Low | Semi | LessMinus -> Non
  | ColonEqual -> Right
  | As -> Non
  | Comma -> Non
  | MinusGreater | BarBar | AmperAmper -> Right
  | InfixOp0 -> Left
  | InfixOp1 -> Right
  | ColonColon -> Right
  | InfixOp2 | InfixOp3 -> Left
  | InfixOp4 -> Right
  | UMinus | Apply -> Non
  | HashOp -> Left
  | Dot -> Left
  | High -> Non
  | Atomic -> Non

(** Term-in-context, [{ctx; ast}] records that [ast] is (considered to be)
    an immediate sub-term of [ctx] as assumed by the operations in
    [Requires_sub_terms]. *)
module rec In_ctx : sig
  type 'a xt = private {ctx: T.t; ast: 'a}

  val sub_ast : ctx:T.t -> T.t -> T.t xt

  val sub_typ : ctx:T.t -> core_type -> core_type xt

  val sub_cty : ctx:T.t -> class_type -> class_type xt

  val sub_pat : ctx:T.t -> pattern -> pattern xt

  val sub_exp : ctx:T.t -> expression -> expression xt

  val sub_cl : ctx:T.t -> class_expr -> class_expr xt

  val sub_mty : ctx:T.t -> module_type -> module_type xt

  val sub_mod : ctx:T.t -> module_expr -> module_expr xt

  val sub_sig : ctx:T.t -> signature_item -> signature_item xt

  val sub_str : ctx:T.t -> structure_item -> structure_item xt
end = struct
  open Requires_sub_terms

  type 'a xt = {ctx: T.t; ast: 'a}

  let sub_ast ~ctx ast = {ctx; ast}

  let sub_typ ~ctx typ = check parenze_typ {ctx; ast= typ}

  let sub_cty ~ctx cty = {ctx; ast= cty}

  let sub_pat ~ctx pat = check parenze_pat {ctx; ast= pat}

  let sub_exp ~ctx exp = check parenze_exp {ctx; ast= exp}

  let sub_cl ~ctx cl = {ctx; ast= cl}

  let sub_mty ~ctx mty = {ctx; ast= mty}

  let sub_mod ~ctx mod_ = {ctx; ast= mod_}

  let sub_sig ~ctx sig_ = {ctx; ast= sig_}

  let sub_str ~ctx str = {ctx; ast= str}
end

(** Operations determining precedence and necessary parenthesization of
    terms based on their super-terms. *)
and Requires_sub_terms : sig
  val is_simple :
    Conf.t -> (expression In_ctx.xt -> int) -> expression In_ctx.xt -> bool

  type cls = Let_match | Match | Non_apply | Sequence | Then | ThenElse

  val exposed_right_exp : cls -> expression -> bool

  val exposed_left_exp : expression -> bool

  val exposed_left_typ : core_type -> bool

  val exposed_right_typ : core_type -> bool

  val prec_ast : T.t -> prec option

  val parenze_typ : core_type In_ctx.xt -> bool

  val parenze_mty : module_type In_ctx.xt -> bool

  val parenze_mod : module_expr In_ctx.xt -> bool

  val parenze_cty : class_type In_ctx.xt -> bool

  val parenze_cl : class_expr In_ctx.xt -> bool

  val parenze_pat : pattern In_ctx.xt -> bool

  val parenze_exp : expression In_ctx.xt -> bool
end = struct
  open In_ctx

  (* This module uses physical equality extensively to detect sub-terms. *)

  let ( == ) = Caml.Pervasives.( == )

  let dump fs ctx ast =
    Format.fprintf fs "ast: %a@\nctx: %a@\n" T.dump ast T.dump ctx

  let fail ctx ast exc =
    if Conf.debug then (
      let bt = Caml.Printexc.get_backtrace () in
      dump Format.err_formatter ctx ast ;
      Format.eprintf "%s%!" bt ) ;
    raise exc

  (** Predicates to check the claimed sub-term relation. *)

  let check_typ {ctx; ast= typ} =
    let f tI = typ == tI in
    let fst_f (tI, _) = typ == tI in
    let snd_f (_, tI) = typ == tI in
    let check_cstr = function
      | Pcstr_tuple t1N -> List.exists t1N ~f
      | Pcstr_record ld1N ->
          List.exists ld1N ~f:(fun {pld_type} -> typ == pld_type)
    in
    let check_ext {pext_kind} =
      match pext_kind with
      | Pext_decl (cstr, t0) -> check_cstr cstr || Option.exists t0 ~f
      | _ -> false
    in
    let check_typext {ptyext_params; ptyext_constructors} =
      List.exists ptyext_params ~f:fst_f
      || List.exists ptyext_constructors ~f:check_ext
    in
    let check_type {ptype_params; ptype_cstrs; ptype_kind; ptype_manifest} =
      List.exists ptype_params ~f:fst_f
      || List.exists ptype_cstrs ~f:(fun (t1, t2, _) ->
             typ == t1 || typ == t2 )
      || ( match ptype_kind with
         | Ptype_variant cd1N ->
             List.exists cd1N ~f:(fun {pcd_args; pcd_res} ->
                 check_cstr pcd_args || Option.exists pcd_res ~f )
         | Ptype_record ld1N ->
             List.exists ld1N ~f:(fun {pld_type} -> typ == pld_type)
         | _ -> false )
      || Option.exists ptype_manifest ~f
    in
    let check_pcstr_fields pcstr_fields =
      List.exists pcstr_fields ~f:(fun f ->
          match f.pcf_desc with
          | Pcf_inherit (_, _, _) -> false
          | Pcf_val (_, _, Cfk_virtual t) -> typ == t
          | Pcf_val
              (_, _, Cfk_concrete (_, {pexp_desc= Pexp_constraint (_, t)}))
            ->
              typ == t
          | Pcf_val (_, _, Cfk_concrete _) -> false
          | Pcf_method (_, _, Cfk_virtual t) -> typ == t
          | Pcf_method
              (_, _, Cfk_concrete (_, {pexp_desc= Pexp_constraint (_, t)}))
            ->
              typ == t
          | Pcf_method
              (_, _, Cfk_concrete (_, {pexp_desc= Pexp_poly (e, topt)})) ->
              let rec loop = function
                | {pexp_desc= Pexp_newtype (_, e)} -> loop e
                | {pexp_desc= Pexp_constraint (_, t)} -> t == typ
                | {pexp_desc= Pexp_fun (_, _, _, e)} -> loop e
                | _ -> false
              in
              (match topt with None -> false | Some t -> typ == t)
              || loop e
          | Pcf_method (_, _, Cfk_concrete _) -> false
          | Pcf_constraint (t1, t2) -> t1 == typ || t2 == typ
          | Pcf_initializer _ | Pcf_attribute _ | Pcf_extension _ -> false
      )
    in
    let check_class_type l =
      List.exists l ~f:(fun {pci_expr= {pcty_desc}; pci_params} ->
          List.exists pci_params ~f:(fun (t, _) -> t == typ)
          ||
          match pcty_desc with
          | Pcty_constr (_, l) -> List.exists l ~f:(fun x -> x == typ)
          | Pcty_arrow (_, t, _) -> t == typ
          | _ -> false )
    in
    match ctx with
    | Pld (PTyp t1) -> assert (typ == t1)
    | Pld _ -> assert false
    | Typ ctx -> (
      match ctx.ptyp_desc with
      | Ptyp_extension _ -> ()
      | Ptyp_any | Ptyp_var _ -> assert false
      | Ptyp_alias (t1, _) | Ptyp_poly (_, t1) -> assert (typ == t1)
      | Ptyp_arrow (_, t1, t2) -> assert (typ == t1 || typ == t2)
      | Ptyp_tuple t1N | Ptyp_constr (_, t1N) -> assert (List.exists t1N ~f)
      | Ptyp_variant (r1N, _, _) ->
          assert (
            List.exists r1N ~f:(function
              | Rtag (_, _, _, t1N) -> List.exists t1N ~f
              | Rinherit t1 -> typ == t1 ) )
      | Ptyp_package (_, it1N) -> assert (List.exists it1N ~f:snd_f)
      | Ptyp_object (fields, _) ->
          assert (
            List.exists fields ~f:(function
              | Otag (_, _, t1) -> typ == t1
              | Oinherit t1 -> typ == t1 ) )
      | Ptyp_class (_, l) -> assert (List.exists l ~f) )
    | Cty {pcty_desc} ->
        assert (
          match pcty_desc with
          | Pcty_constr (_, l) -> List.exists l ~f
          | Pcty_arrow (_, t, _) -> t == typ
          | Pcty_open _ -> false
          | Pcty_extension _ -> false
          | Pcty_signature {pcsig_self; pcsig_fields} ->
              pcsig_self == typ
              || List.exists pcsig_fields ~f:(fun {pctf_desc} ->
                     match pctf_desc with
                     | Pctf_constraint (t1, t2) -> t1 == typ || t2 == typ
                     | Pctf_val (_, _, _, t) -> t == typ
                     | Pctf_method (_, _, _, t) -> t == typ
                     | Pctf_inherit _ -> false
                     | Pctf_attribute _ -> false
                     | Pctf_extension _ -> false ) )
    | Pat ctx -> (
      match ctx.ppat_desc with
      | Ppat_constraint (_, t1) -> assert (typ == t1)
      | Ppat_extension (_, PTyp t) -> assert (typ == t)
      | _ -> assert false )
    | Exp ctx -> (
      match ctx.pexp_desc with
      | Pexp_constraint (_, ({ptyp_desc= Ptyp_package (_, it1N)} as ty)) ->
          assert (typ == ty || List.exists it1N ~f:snd_f)
      | Pexp_constraint (_, t1)
       |Pexp_coerce (_, None, t1)
       |Pexp_poly (_, Some t1)
       |Pexp_extension (_, PTyp t1) ->
          assert (typ == t1)
      | Pexp_coerce (_, Some t1, t2) -> assert (typ == t1 || typ == t2)
      | Pexp_letexception (ext, _) -> assert (check_ext ext)
      | Pexp_object {pcstr_fields} ->
          assert (check_pcstr_fields pcstr_fields)
      | _ -> assert false )
    | Cl {pcl_desc} ->
        assert (
          match pcl_desc with
          | Pcl_constr (_, l) -> List.exists l ~f
          | Pcl_constraint _ -> false
          | Pcl_let _ -> false
          | Pcl_apply _ -> false
          | Pcl_fun _ -> false
          | Pcl_open _ -> false
          | Pcl_extension _ -> false
          | Pcl_structure {pcstr_fields} -> check_pcstr_fields pcstr_fields
        )
    | Mty ctx ->
        let rec loop m =
          match m with
          | Pmty_with (m, c1N) ->
              List.exists c1N ~f:(function
                | Pwith_type (_, d1) | Pwith_typesubst (_, d1) ->
                    check_type d1
                | _ -> false )
              || loop m.pmty_desc
          | _ -> false
        in
        assert (loop ctx.pmty_desc)
    | Mod ctx -> (
      match ctx.pmod_desc with
      | Pmod_unpack e1 -> (
        match e1.pexp_desc with
        | Pexp_constraint (_, ({ptyp_desc= Ptyp_package (_, it1N)} as ty))
          ->
            assert (typ == ty || List.exists it1N ~f:snd_f)
        | Pexp_constraint (_, t1)
         |Pexp_coerce (_, None, t1)
         |Pexp_poly (_, Some t1)
         |Pexp_extension (_, PTyp t1) ->
            assert (typ == t1)
        | Pexp_coerce (_, Some t1, t2) -> assert (typ == t1 || typ == t2)
        | Pexp_letexception (ext, _) -> assert (check_ext ext)
        | Pexp_object {pcstr_fields} ->
            assert (check_pcstr_fields pcstr_fields)
        | _ -> assert false )
      | _ -> assert false )
    | Sig ctx -> (
      match ctx.psig_desc with
      | Psig_value {pval_type= t1} -> assert (typ == t1)
      | Psig_type (_, d1N) -> assert (List.exists d1N ~f:check_type)
      | Psig_typext typext -> assert (check_typext typext)
      | Psig_exception ext -> assert (check_ext ext)
      | Psig_class_type l -> assert (check_class_type l)
      | Psig_class l -> assert (check_class_type l)
      | _ -> assert false )
    | Str ctx -> (
      match ctx.pstr_desc with
      | Pstr_primitive {pval_type= t1} -> assert (typ == t1)
      | Pstr_type (_, d1N) -> assert (List.exists d1N ~f:check_type)
      | Pstr_typext typext -> assert (check_typext typext)
      | Pstr_exception ext -> assert (check_ext ext)
      | Pstr_class l ->
          assert (
            List.exists l ~f:(fun {pci_expr= {pcl_desc}; pci_params} ->
                List.exists pci_params ~f:(fun (t, _) -> t == typ)
                ||
                match pcl_desc with
                | Pcl_constr (_, l) -> List.exists l ~f:(fun x -> x == typ)
                | _ -> false ) )
      | Pstr_class_type l -> assert (check_class_type l)
      | Pstr_extension ((_, PTyp t), _) -> assert (t == typ)
      | Pstr_extension (_, _) -> assert false
      | _ -> assert false )
    | Top -> assert false

  let check_typ ({ctx; ast= typ} as xtyp) =
    try check_typ xtyp with exc -> fail ctx (Typ typ) exc

  let check_cty {ctx; ast= cty} =
    let check_class_type l =
      List.exists l ~f:(fun {pci_expr} ->
          let rec loop x =
            x == cty
            ||
            match x.pcty_desc with
            | Pcty_arrow (_, _, x) -> loop x
            | _ -> false
          in
          loop pci_expr )
    in
    match (ctx : t) with
    | Exp _ -> assert false
    | Pld _ -> assert false
    | Str ctx -> (
      match ctx.pstr_desc with
      | Pstr_class_type l -> assert (check_class_type l)
      | Pstr_class l ->
          assert (
            List.exists l ~f:(fun {pci_expr} ->
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
    | Cty {pcty_desc} -> (
      match pcty_desc with
      | Pcty_arrow (_, _, t) -> assert (t == cty)
      | Pcty_signature {pcsig_fields} ->
          assert (
            List.exists pcsig_fields ~f:(fun {pctf_desc} ->
                match pctf_desc with
                | Pctf_inherit t -> t == cty
                | Pctf_val _ -> false
                | Pctf_method _ -> false
                | Pctf_constraint _ -> false
                | Pctf_attribute _ -> false
                | Pctf_extension _ -> false ) )
      | Pcty_open (_, _, t) -> assert (t == cty)
      | Pcty_constr _ -> assert false
      | Pcty_extension _ -> assert false )
    | Top -> assert false
    | Typ _ -> assert false
    | Pat _ -> assert false
    | Cl ctx ->
        assert (
          match ctx.pcl_desc with
          | Pcl_fun (_, _, _, _) -> false
          | Pcl_constr _ -> false
          | Pcl_structure _ -> false
          | Pcl_apply _ -> false
          | Pcl_let (_, _, _) -> false
          | Pcl_constraint (_, x) -> x == cty
          | Pcl_extension _ -> false
          | Pcl_open _ -> false )
    | Mty _ -> assert false
    | Mod _ -> assert false

  let check_cty ({ctx; ast= cty} as xcty) =
    try check_cty xcty with exc -> fail ctx (Cty cty) exc

  let check_cl {ctx; ast= cl} =
    let check_pcstr_fields pcstr_fields =
      List.exists pcstr_fields ~f:(fun f ->
          match f.pcf_desc with
          | Pcf_inherit (_, x, _) -> x == cl
          | _ -> false )
    in
    match (ctx : t) with
    | Exp e -> (
      match e.pexp_desc with
      | Pexp_object {pcstr_fields} ->
          assert (check_pcstr_fields pcstr_fields)
      | _ -> assert false )
    | Pld _ -> assert false
    | Str ctx -> (
      match ctx.pstr_desc with
      | Pstr_class l ->
          assert (
            List.exists l ~f:(fun {pci_expr} ->
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
    | Typ _ -> assert false
    | Pat _ -> assert false
    | Cl {pcl_desc} ->
        assert (
          match pcl_desc with
          | Pcl_structure {pcstr_fields} -> check_pcstr_fields pcstr_fields
          | Pcl_fun (_, _, _, x) -> x == cl
          | Pcl_apply (x, _) -> x == cl
          | Pcl_let (_, _, x) -> x == cl
          | Pcl_constraint (x, _) -> x == cl
          | Pcl_open (_, _, x) -> x == cl
          | Pcl_constr _ -> false
          | Pcl_extension _ -> false )
    | Mty _ -> assert false
    | Mod _ -> assert false

  let check_cl ({ctx; ast= cl} as xcl) =
    try check_cl xcl with exc -> fail ctx (Cl cl) exc

  let check_pat {ctx; ast= pat} =
    let check_pcstr_fields pcstr_fields =
      List.exists pcstr_fields ~f:(fun {pcf_desc} ->
          match pcf_desc with
          | Pcf_initializer _ -> false
          | Pcf_val (_, _, _) -> false
          | Pcf_method (_, _, _) -> false
          | Pcf_extension (_, PPat (p, _)) -> p == pat
          | Pcf_extension (_, _) -> false
          | Pcf_inherit _ -> false
          | Pcf_constraint _ -> false
          | Pcf_attribute _ -> false )
    in
    let check_extensions = function
      | PPat (p, _) -> p == pat
      | _ -> false
    in
    let check_bindings l =
      List.exists l ~f:(fun {pvb_pat} ->
          pvb_pat == pat
          ||
          match pvb_pat.ppat_desc with
          | Ppat_constraint (p, _) -> p == pat
          | _ -> false )
    in
    match ctx with
    | Pld (PPat (p1, _)) -> assert (p1 == pat)
    | Pld _ -> assert false
    | Typ ctx -> (
      match ctx.ptyp_desc with
      | Ptyp_extension (_, ext) -> assert (check_extensions ext)
      | _ -> assert false )
    | Pat ctx -> (
        let f pI = pI == pat in
        let snd_f (_, pI) = pI == pat in
        match ctx.ppat_desc with
        | Ppat_array p1N | Ppat_tuple p1N -> assert (List.exists p1N ~f)
        | Ppat_record (p1N, _) -> assert (List.exists p1N ~f:snd_f)
        | Ppat_construct
            ({txt= Lident "::"}, Some {ppat_desc= Ppat_tuple [p1; p2]})
         |Ppat_or (p1, p2) ->
            assert (p1 == pat || p2 == pat)
        | Ppat_alias (p1, _)
         |Ppat_constraint (p1, _)
         |Ppat_construct (_, Some p1)
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
      | Pexp_apply _ | Pexp_array _ | Pexp_assert _ | Pexp_coerce _
       |Pexp_constant _ | Pexp_constraint _ | Pexp_construct _
       |Pexp_field _ | Pexp_ident _ | Pexp_ifthenelse _ | Pexp_lazy _
       |Pexp_letexception _ | Pexp_letmodule _ | Pexp_new _
       |Pexp_newtype _ | Pexp_open _ | Pexp_override _ | Pexp_pack _
       |Pexp_poly _ | Pexp_record _ | Pexp_send _ | Pexp_sequence _
       |Pexp_setfield _ | Pexp_setinstvar _ | Pexp_tuple _
       |Pexp_unreachable | Pexp_variant _ | Pexp_while _ ->
          assert false
      | Pexp_extension (_, ext) -> assert (check_extensions ext)
      | Pexp_object {pcstr_self; pcstr_fields} ->
          assert (pcstr_self == pat || check_pcstr_fields pcstr_fields)
      | Pexp_let (_, bindings, _) -> assert (check_bindings bindings)
      | Pexp_function cases | Pexp_match (_, cases) | Pexp_try (_, cases) ->
          assert (
            List.exists cases ~f:(function
              | {pc_lhs} when pc_lhs == pat -> true
              | _ -> false ) )
      | Pexp_for (p, _, _, _, _) | Pexp_fun (_, _, p, _) -> assert (p == pat)
      )
    | Cl ctx ->
        assert (
          match ctx.pcl_desc with
          | Pcl_fun (_, _, p, _) -> p == pat
          | Pcl_constr _ -> false
          | Pcl_structure {pcstr_self; pcstr_fields} ->
              pcstr_self == pat || check_pcstr_fields pcstr_fields
          | Pcl_apply _ -> false
          | Pcl_let (_, l, _) -> check_bindings l
          | Pcl_constraint _ -> false
          | Pcl_extension (_, ext) -> check_extensions ext
          | Pcl_open _ -> false )
    | Cty _ -> assert false
    | Mty _ | Mod _ | Sig _ -> assert false
    | Str str -> (
      match str.pstr_desc with
      | Pstr_value (_, bindings) -> assert (check_bindings bindings)
      | Pstr_extension ((_, ext), _) -> assert (check_extensions ext)
      | _ -> assert false )
    | Top -> assert false

  let check_pat ({ctx; ast= pat} as xpat) =
    try check_pat xpat with exc -> fail ctx (Pat pat) exc

  let check_exp {ctx; ast= exp} =
    let check_extensions = function
      | PPat (_, Some e) -> e == exp
      | PStr [{pstr_desc= Pstr_eval (e, _)}] -> e == exp
      | _ -> false
    in
    let check_pcstr_fields pcstr_fields =
      List.exists pcstr_fields ~f:(fun {pcf_desc} ->
          match pcf_desc with
          | Pcf_initializer e -> e == exp
          | Pcf_val (_, _, Cfk_concrete (_, e)) ->
              let rec loop x =
                x == exp
                ||
                match x with
                | {pexp_desc= Pexp_constraint (e, _)} -> loop e
                | _ -> false
              in
              loop e
          | Pcf_val (_, _, Cfk_virtual _) -> false
          | Pcf_method
              (_, _, Cfk_concrete (_, {pexp_desc= Pexp_poly (e, _)}))
           |Pcf_method (_, _, Cfk_concrete (_, e)) ->
              let rec loop x =
                x == exp
                ||
                match x with
                | {pexp_desc= Pexp_newtype (_, e)} -> loop e
                | {pexp_desc= Pexp_constraint (e, _)} -> loop e
                | {pexp_desc= Pexp_fun (_, _, _, e)} -> loop e
                | _ -> false
              in
              loop e
          | Pcf_method (_, _, Cfk_virtual _) -> false
          | Pcf_extension (_, ext) -> check_extensions ext
          | Pcf_inherit _ -> false
          | Pcf_constraint _ -> false
          | Pcf_attribute _ -> false )
    in
    match ctx with
    | Pld (PPat (_, Some e1)) -> assert (e1 == exp)
    | Pld _ -> assert false
    | Exp ctx -> (
        let f eI = eI == exp in
        let snd_f (_, eI) = eI == exp in
        match ctx.pexp_desc with
        | Pexp_construct
            ({txt= Lident "::"}, Some {pexp_desc= Pexp_tuple [e1; e2]}) ->
            assert (e1 == exp || e2 == exp)
        | Pexp_extension (_, ext) -> assert (check_extensions ext)
        | Pexp_constant _ | Pexp_ident _ | Pexp_new _ | Pexp_pack _
         |Pexp_unreachable ->
            assert false
        | Pexp_object {pcstr_fields} ->
            assert (check_pcstr_fields pcstr_fields)
        | Pexp_let (_, bindings, e) ->
            assert (
              List.exists bindings ~f:(fun {pvb_expr} -> pvb_expr == exp)
              || e == exp )
        | (Pexp_match (e, _) | Pexp_try (e, _)) when e == exp -> ()
        | Pexp_function cases | Pexp_match (_, cases) | Pexp_try (_, cases)
          ->
            assert (
              List.exists cases ~f:(function
                | {pc_guard= Some g} when g == exp -> true
                | {pc_rhs} when pc_rhs == exp -> true
                | _ -> false ) )
        | Pexp_fun (_, default, _, body) ->
            assert (
              Option.value_map default ~default:false ~f || body == exp )
        | Pexp_apply
            ( ({pexp_desc= Pexp_ident {txt}; pexp_attributes= []} as e0)
            , e1 :: indices )
          when Option.is_some (index_op_get_sugar txt indices) ->
            let _, indices =
              Option.value_exn (index_op_get_sugar txt indices)
            in
            assert (e0 == exp || snd_f e1 || List.exists indices ~f)
        | Pexp_apply
            ( ({pexp_desc= Pexp_ident {txt}; pexp_attributes= []} as e0)
            , e1 :: indices )
          when Option.is_some (index_op_set_sugar txt indices) ->
            let _, indices, e =
              Option.value_exn (index_op_set_sugar txt indices)
            in
            assert (
              e0 == exp || snd_f e1 || List.exists indices ~f || e == exp )
        | Pexp_apply (e0, e1N) ->
            assert (e0 == exp || List.exists e1N ~f:snd_f)
        | Pexp_tuple e1N | Pexp_array e1N -> assert (List.exists e1N ~f)
        | Pexp_construct (_, e) | Pexp_variant (_, e) ->
            assert (Option.exists e ~f)
        | Pexp_record (e1N, e0) ->
            assert (Option.exists e0 ~f || List.exists e1N ~f:snd_f)
        | Pexp_assert e
         |Pexp_constraint (e, _)
         |Pexp_coerce (e, _, _)
         |Pexp_field (e, _)
         |Pexp_lazy e
         |Pexp_letexception (_, e)
         |Pexp_letmodule (_, _, e)
         |Pexp_newtype (_, e)
         |Pexp_open (_, _, e)
         |Pexp_poly (e, _)
         |Pexp_send (e, _)
         |Pexp_setinstvar (_, e) ->
            assert (e == exp)
        | Pexp_sequence (e1, e2) -> assert (e1 == exp || e2 == exp)
        | Pexp_setfield (e1, _, e2) | Pexp_while (e1, e2) ->
            assert (e1 == exp || e2 == exp)
        | Pexp_ifthenelse (e1, e2, e3) ->
            assert (e1 == exp || e2 == exp || Option.exists e3 ~f)
        | Pexp_for (_, e1, e2, _, e3) ->
            assert (e1 == exp || e2 == exp || e3 == exp)
        | Pexp_override e1N -> assert (List.exists e1N ~f:snd_f) )
    | Str str -> (
      match str.pstr_desc with
      | Pstr_eval (e0, _) -> assert (e0 == exp)
      | Pstr_value (_, bindings) ->
          assert (List.exists bindings ~f:(fun {pvb_expr} -> pvb_expr == exp)
          )
      | Pstr_extension ((_, ext), _) -> assert (check_extensions ext)
      | Pstr_primitive _ | Pstr_type _ | Pstr_typext _ | Pstr_exception _
       |Pstr_module _ | Pstr_recmodule _ | Pstr_modtype _ | Pstr_open _
       |Pstr_class _ | Pstr_class_type _ | Pstr_include _ | Pstr_attribute _
        ->
          assert false )
    | Mod {pmod_desc= Pmod_unpack e1} -> (
      match e1 with
      | { pexp_desc=
            Pexp_constraint
              (e, {ptyp_desc= Ptyp_package _; ptyp_attributes= []})
        ; pexp_attributes= [] } ->
          assert (e == exp)
      | e -> assert (e == exp) )
    | Cl ctx ->
        let rec loop ctx =
          match ctx.pcl_desc with
          | Pcl_fun (_, eopt, _, e) ->
              Option.exists eopt ~f:(fun e -> e == exp) || loop e
          | Pcl_constr _ -> false
          | Pcl_structure {pcstr_fields} -> check_pcstr_fields pcstr_fields
          | Pcl_apply (_, l) -> List.exists l ~f:(fun (_, e) -> e == exp)
          | Pcl_let (_, l, _) ->
              List.exists l ~f:(fun {pvb_expr} -> pvb_expr == exp)
          | Pcl_constraint _ -> false
          | Pcl_extension _ -> false
          | Pcl_open _ -> false
        in
        assert (loop ctx)
    | Cty _ -> assert false
    | Mod _ | Top | Typ _ | Pat _ | Mty _ | Sig _ -> assert false

  let check_exp ({ctx; ast= exp} as xexp) =
    try check_exp xexp with exc -> fail ctx (Exp exp) exc

  let rec is_simple (c : Conf.t) width ({ast= exp} as xexp) =
    let ctx = Exp exp in
    match exp.pexp_desc with
    | Pexp_constant _ -> is_trivial c exp
    | Pexp_field _ | Pexp_ident _ | Pexp_send _
     |Pexp_construct (_, None)
     |Pexp_variant (_, None) ->
        true
    | Pexp_construct
        ({txt= Lident "::"}, Some {pexp_desc= Pexp_tuple [e1; e2]}) ->
        is_simple c width (sub_exp ~ctx e1)
        && is_simple c width (sub_exp ~ctx e2)
        && width xexp * 3 < c.margin
    | Pexp_construct (_, Some e0) | Pexp_variant (_, Some e0) ->
        is_trivial c e0
    | Pexp_array e1N | Pexp_tuple e1N ->
        List.for_all e1N ~f:(is_trivial c) && width xexp * 3 < c.margin
    | Pexp_record (e1N, e0) ->
        Option.for_all e0 ~f:(is_trivial c)
        && List.for_all e1N ~f:(snd >> is_trivial c)
        && width xexp * 3 < c.margin
    | Pexp_apply ({pexp_desc= Pexp_ident {txt= Lident ":="}}, _) -> false
    | Pexp_apply (e0, e1N) ->
        is_trivial c e0
        && List.for_all e1N ~f:(snd >> is_trivial c)
        && width xexp * 3 < c.margin
    | Pexp_extension (_, PStr [{pstr_desc= Pstr_eval (e0, []); _}]) ->
        is_simple c width (sub_exp ~ctx e0)
    | Pexp_extension (_, (PStr [] | PTyp _)) -> true
    | _ -> false

  (** [prec_ctx {ctx; ast}] is the precedence of the context of [ast] within
      [ctx], where [ast] is an immediate sub-term (modulo syntactic sugar)
      of [ctx]. Also returns whether [ast] is the left, right, or neither
      child of [ctx]. Meaningful for binary operators, otherwise returns
      [None]. *)
  let prec_ctx ctx =
    let is_tuple_lvl1_in_constructor ty = function
      | {ptype_kind= Ptype_variant cd1N} ->
          List.exists cd1N ~f:(function
            | {pcd_args= Pcstr_tuple t1N} ->
                List.exists t1N ~f:(phys_equal ty)
            | _ -> false )
      | _ -> false
    in
    let is_tuple_lvl1_in_ext_constructor ty = function
      | {pext_kind= Pext_decl (Pcstr_tuple t1N, _)} ->
          List.exists t1N ~f:(phys_equal ty)
      | _ -> false
    in
    let constructor_cxt_prec_of_inner = function
      | {ptyp_desc= Ptyp_arrow _} -> Some (Apply, Non)
      | {ptyp_desc= Ptyp_tuple _} -> Some (InfixOp3, Non)
      | _ -> None
    in
    match ctx with
    | { ctx=
          ( Str {pstr_desc= Pstr_type (_, t1N)}
          | Sig {psig_desc= Psig_type (_, t1N)} )
      ; ast= Typ ({ptyp_desc= Ptyp_arrow _ | Ptyp_tuple _} as typ) }
      when List.exists t1N ~f:(is_tuple_lvl1_in_constructor typ) ->
        constructor_cxt_prec_of_inner typ
    | { ctx=
          ( Str {pstr_desc= Pstr_typext {ptyext_constructors= l}}
          | Sig {psig_desc= Psig_typext {ptyext_constructors= l}} )
      ; ast= Typ ({ptyp_desc= Ptyp_arrow _ | Ptyp_tuple _} as typ) }
      when List.exists l ~f:(is_tuple_lvl1_in_ext_constructor typ) ->
        constructor_cxt_prec_of_inner typ
    | { ctx=
          ( Str {pstr_desc= Pstr_exception constr}
          | Sig {psig_desc= Psig_exception constr}
          | Exp {pexp_desc= Pexp_letexception (constr, _)} )
      ; ast= Typ ({ptyp_desc= Ptyp_tuple _ | Ptyp_arrow _} as typ) }
      when is_tuple_lvl1_in_ext_constructor typ constr ->
        constructor_cxt_prec_of_inner typ
    | {ctx= Str _; ast= Typ _} -> None
    | {ctx= Typ {ptyp_desc}; ast= Typ typ} -> (
      match ptyp_desc with
      | Ptyp_arrow (_, t1, _) ->
          Some (MinusGreater, if t1 == typ then Left else Right)
      | Ptyp_tuple _ -> Some (InfixOp3, Non)
      | Ptyp_alias _ -> Some (As, Non)
      | Ptyp_constr (_, _ :: _ :: _) -> Some (Comma, Non)
      | Ptyp_constr _ -> Some (Apply, Non)
      | Ptyp_any | Ptyp_var _ | Ptyp_object _ | Ptyp_class _
       |Ptyp_variant _ | Ptyp_poly _ | Ptyp_package _ | Ptyp_extension _ ->
          None )
    | {ctx= Cty {pcty_desc}; ast= Typ typ} -> (
      match pcty_desc with
      | Pcty_constr (_, _ :: _ :: _) -> Some (Comma, Non)
      | Pcty_arrow (_, t1, _) ->
          Some (MinusGreater, if t1 == typ then Left else Right)
      | _ -> None )
    | {ctx= Cty {pcty_desc}; ast= Cty typ} -> (
      match pcty_desc with
      | Pcty_arrow (_, _, t2) ->
          Some (MinusGreater, if t2 == typ then Right else Left)
      | _ -> None )
    | {ast= Cty _} -> None
    | {ast= Typ _} -> None
    | {ctx= Exp {pexp_desc}; ast= Exp exp} -> (
      match pexp_desc with
      | Pexp_tuple (e0 :: _) ->
          Some (Comma, if exp == e0 then Left else Right)
      | Pexp_construct
          ({txt= Lident "::"}, Some {pexp_desc= Pexp_tuple [_; e2]}) ->
          if is_sugared_list e2 then Some (Semi, Non)
          else Some (ColonColon, if exp == e2 then Right else Left)
      | Pexp_array _ -> Some (Semi, Non)
      | Pexp_construct (_, Some _)
       |Pexp_assert _ | Pexp_lazy _
       |Pexp_variant (_, Some _) ->
          Some (Apply, Non)
      | Pexp_apply ({pexp_desc= Pexp_ident {txt= Lident i; loc}}, [_]) -> (
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
      | Pexp_apply ({pexp_desc= Pexp_ident {txt}}, (Nolabel, a1) :: args)
        when Option.is_some (index_op_get_sugar txt args) ->
          if a1 == exp then Some (Dot, Left) else Some (Comma, Left)
      | Pexp_apply ({pexp_desc= Pexp_ident {txt}}, (Nolabel, a1) :: args)
        when Option.is_some (index_op_set_sugar txt args) ->
          let _, _, e = Option.value_exn (index_op_set_sugar txt args) in
          if a1 == exp then Some (Dot, Left)
          else if e == exp then Some (Comma, Right)
          else Some (Comma, Left)
      | Pexp_apply ({pexp_desc= Pexp_ident {txt= Lident i}}, [(_, e1); _])
        -> (
          let child = if e1 == exp then Left else Right in
          match (i.[0], i) with
          | _, ":=" -> Some (ColonEqual, child)
          | _, ("or" | "||") -> Some (BarBar, child)
          | _, ("&" | "&&") -> Some (AmperAmper, child)
          | ('=' | '<' | '>' | '|' | '&' | '$'), _ | _, "!=" ->
              Some (InfixOp0, child)
          | ('@' | '^'), _ -> Some (InfixOp1, child)
          | ('+' | '-'), _ -> Some (InfixOp2, child)
          | '*', _ when Poly.(i <> "*" && i.[1] = '*') ->
              Some (InfixOp4, child)
          | ('*' | '/' | '%'), _ | _, ("lor" | "lxor" | "mod" | "land") ->
              Some (InfixOp3, child)
          | _, ("lsl" | "lsr" | "asr") -> Some (InfixOp4, child)
          | '#', _ -> Some (HashOp, child)
          | _ -> Some (Apply, if is_infix_id i then child else Non) )
      | Pexp_apply _ -> Some (Apply, Non)
      | Pexp_setfield (e0, _, _) when e0 == exp -> Some (Dot, Non)
      | Pexp_setfield (_, _, e0) when e0 == exp -> Some (LessMinus, Non)
      | Pexp_setinstvar _ -> Some (LessMinus, Non)
      | Pexp_field _ -> Some (Dot, Left)
      | Pexp_send _ -> Some (HashOp, Non)
      | _ -> None )
    | {ctx= Cl {pcl_desc}; ast= Cl _ | Exp _} -> (
      match pcl_desc with Pcl_apply _ -> Some (Apply, Non) | _ -> None )
    | { ctx= Exp _
      ; ast= Pld _ | Top | Pat _ | Cl _ | Mty _ | Mod _ | Sig _ | Str _ }
     |{ctx= Cl _; ast= Pld _ | Top | Pat _ | Mty _ | Mod _ | Sig _ | Str _}
     |{ ctx=
          ( Pld _ | Top | Typ _ | Cty _ | Pat _ | Mty _ | Mod _ | Sig _
          | Str _ )
      ; ast=
          Pld _ | Top | Pat _ | Exp _ | Cl _ | Mty _ | Mod _ | Sig _ | Str _
      } ->
        None

  (** [prec_ast ast] is the precedence of [ast]. Meaningful for binary
      operators, otherwise returns [None]. *)
  let prec_ast = function
    | Pld _ -> None
    | Typ {ptyp_desc} -> (
      match ptyp_desc with
      | Ptyp_package _ -> Some Low
      | Ptyp_arrow _ -> Some MinusGreater
      | Ptyp_tuple _ -> Some InfixOp3
      | Ptyp_alias _ -> Some As
      | Ptyp_any | Ptyp_var _ | Ptyp_constr _ | Ptyp_object _
       |Ptyp_class _ | Ptyp_variant _ | Ptyp_poly _ | Ptyp_extension _ ->
          None )
    | Cty {pcty_desc} -> (
      match pcty_desc with Pcty_arrow _ -> Some MinusGreater | _ -> None )
    | Exp {pexp_desc} -> (
      match pexp_desc with
      | Pexp_tuple _ -> Some Comma
      | Pexp_construct ({txt= Lident "::"}, Some {pexp_desc= Pexp_tuple _})
        ->
          Some ColonColon
      | Pexp_construct (_, Some _) -> Some Apply
      | Pexp_constant (Pconst_integer (i, _) | Pconst_float (i, _)) -> (
        match i.[0] with '-' | '+' -> Some UMinus | _ -> Some Atomic )
      | Pexp_apply ({pexp_desc= Pexp_ident {txt= Lident i; loc}}, [_]) -> (
        match i with
        | "~-" | "~-." | "~+." | "~+" ->
            if
              loc.loc_end.pos_cnum - loc.loc_start.pos_cnum
              = String.length i - 1
            then Some UMinus
            else Some High
        | "!=" -> Some Apply
        | _ -> (
          match i.[0] with '!' | '?' | '~' -> Some High | _ -> Some Apply )
        )
      | Pexp_apply ({pexp_desc= Pexp_ident {txt}}, (Nolabel, _) :: args)
        when Option.is_some (index_op_get_sugar txt args) ->
          Some Dot
      | Pexp_apply ({pexp_desc= Pexp_ident {txt}}, (Nolabel, _) :: args)
        when Option.is_some (index_op_set_sugar txt args) ->
          Some Comma
      | Pexp_apply ({pexp_desc= Pexp_ident {txt= Lident i}}, [_; _]) -> (
        match (i.[0], i) with
        | _, ":=" -> Some ColonEqual
        | _, ("or" | "||") -> Some BarBar
        | _, ("&" | "&&") -> Some AmperAmper
        | ('=' | '<' | '>' | '|' | '&' | '$'), _ | _, "!=" -> Some InfixOp0
        | ('@' | '^'), _ -> Some InfixOp1
        | ('+' | '-'), _ -> Some InfixOp2
        | '*', _ when Poly.(i <> "*" && i.[1] = '*') -> Some InfixOp4
        | ('*' | '/' | '%'), _ | _, ("lor" | "lxor" | "mod" | "land") ->
            Some InfixOp3
        | _, ("lsl" | "lsr" | "asr") -> Some InfixOp4
        | '#', _ -> Some HashOp
        | _ -> Some Apply )
      | Pexp_apply _ -> Some Apply
      | Pexp_assert _ | Pexp_lazy _ | Pexp_for _
       |Pexp_variant (_, Some _)
       |Pexp_while _ | Pexp_new _ | Pexp_object _
       |Pexp_extension
          ( _
          , PStr
              [ { pstr_desc=
                    Pstr_eval
                      ( { pexp_desc=
                            ( Pexp_new _ | Pexp_object _ | Pexp_while _
                            | Pexp_for _ ) }
                      , _ ) } ] ) ->
          Some Apply
      | Pexp_setfield _ -> Some LessMinus
      | Pexp_setinstvar _ -> Some LessMinus
      | Pexp_field _ -> Some Dot
      | Pexp_send _ -> Some HashOp
      | _ -> None )
    | Cl c -> (
      match c.pcl_desc with
      | Pcl_apply _ -> Some Apply
      | Pcl_structure _ -> Some Apply
      | _ -> None )
    | Top | Pat _ | Mty _ | Mod _ | Sig _ | Str _ -> None

  (** [ambig_prec {ctx; ast}] holds when [ast] is ambiguous in its context
      [ctx], indicating that [ast] should be parenthesized. Meaningful for
      binary operators, otherwise returns [None] if [ctx] has no precedence
      or [Some None] if [ctx] does but [ast] does not. *)
  let ambig_prec ({ast} as xast) =
    prec_ctx xast
    >>| fun (prec_ctx, which_child) ->
    prec_ast ast
    >>| fun prec_ast ->
    let cmp = Poly.compare prec_ctx prec_ast in
    if cmp < 0 then (* ast higher precedence than context: no parens *)
      false
    else if cmp > 0 then (* context higher prec than ast: add parens *)
      true
    else if
      Poly.(assoc_of_prec prec_ast = which_child && which_child <> Non)
    then (* which child and associativity match: no parens *)
      false
    else (* which child and assoc conflict: add parens *)
      true

  (** [parenze_typ {ctx; ast}] holds when type [ast] should be parenthesized
      in context [ctx]. *)
  let parenze_typ ({ctx; ast= typ} as xtyp) =
    assert (check_typ xtyp ; true) ;
    match xtyp with
    | {ast= {ptyp_desc= Ptyp_package _}} -> true
    | {ast= {ptyp_desc= Ptyp_alias _}; ctx= Typ _} -> true
    | { ast= {ptyp_desc= Ptyp_alias _}
      ; ctx= Str {pstr_desc= Pstr_typext _} | Sig {psig_desc= Psig_typext _}
      } ->
        true
    | { ast= {ptyp_desc= Ptyp_alias _}
      ; ctx=
          ( Str {pstr_desc= Pstr_type (_, t)}
          | Sig {psig_desc= Psig_type (_, t)} ) }
      when List.exists t ~f:(fun t ->
               match t.ptype_kind with
               | Ptype_variant l ->
                   List.exists l ~f:(fun c ->
                       match c.pcd_args with
                       | Pcstr_tuple l -> List.exists l ~f:(phys_equal typ)
                       | _ -> false )
               | _ -> false ) ->
        true
    | { ast= {ptyp_desc= Ptyp_alias _}
      ; ctx=
          ( Str
              { pstr_desc=
                  Pstr_exception {pext_kind= Pext_decl (Pcstr_tuple t, _)}
              }
          | Sig
              { psig_desc=
                  Psig_exception {pext_kind= Pext_decl (Pcstr_tuple t, _)}
              } ) }
      when List.exists t ~f:(phys_equal typ) ->
        true
    | _ -> (
      match ambig_prec (sub_ast ~ctx (Typ typ)) with
      | Some (Some true) -> true
      | _ -> false )

  (** [parenze_cty {ctx; ast}] holds when class type [ast] should be
      parenthesized in context [ctx]. *)
  let parenze_cty ({ctx; ast= cty} as xcty) =
    assert (check_cty xcty ; true) ;
    match xcty with _ -> (
      match ambig_prec (sub_ast ~ctx (Cty cty)) with
      | Some (Some true) -> true
      | _ -> false )

  (** [parenze_mty {ctx; ast}] holds when module type [ast] should be
      parenthesized in context [ctx]. *)
  let parenze_mty {ctx; ast= mty} =
    has_trailing_attributes_mty mty
    ||
    match (ctx, mty.pmty_desc) with
    | Str {pstr_desc= Pstr_recmodule _}, Pmty_with _ -> true
    | Sig {psig_desc= Psig_recmodule _}, Pmty_with _ -> true
    | _ -> false

  (** [parenze_mod {ctx; ast}] holds when module expr [ast] should be
      parenthesized in context [ctx]. *)
  let parenze_mod {ctx; ast= m} =
    has_trailing_attributes_mod m
    ||
    match (ctx, m.pmod_desc) with
    | Mod {pmod_desc= Pmod_apply _}, Pmod_functor _ -> true
    | _ -> false

  (** [parenze_pat {ctx; ast}] holds when pattern [ast] should be
      parenthesized in context [ctx]. *)
  let parenze_pat ({ctx; ast= pat} as xpat) =
    assert (check_pat xpat ; true) ;
    has_trailing_attributes_pat pat
    ||
    match (ctx, pat.ppat_desc) with
    | ( Pat
          { ppat_desc=
              Ppat_construct
                ({txt= Lident "::"}, Some {ppat_desc= Ppat_tuple [_; tl]})
          }
      , Ppat_construct ({txt= Lident "::"}, _) )
      when tl == pat ->
        false
    | ( Pat
          { ppat_desc=
              Ppat_construct
                ({txt= Lident "::"}, Some {ppat_desc= Ppat_tuple [_; _]}) }
      , inner ) -> (
      match inner with
      | Ppat_construct ({txt= Lident "::"}, _) -> true
      | Ppat_construct _ | Ppat_record _ | Ppat_variant _ -> false
      | _ -> true )
    | ( Pat {ppat_desc= Ppat_construct _}
      , Ppat_construct ({txt= Lident "::"}, _) ) ->
        true
    | Exp {pexp_desc= Pexp_fun (Optional _, _, _, _)}, Ppat_record _ -> true
    | ( (Exp {pexp_desc= Pexp_let _; _} | Str {pstr_desc= Pstr_value _; _})
      , ( Ppat_construct (_, Some _)
        | Ppat_variant (_, Some _)
        | Ppat_or _ | Ppat_alias _ ) ) ->
        true
    | ( (Exp {pexp_desc= Pexp_let _} | Str {pstr_desc= Pstr_value _})
      , Ppat_constraint (_, {ptyp_desc= Ptyp_poly _}) ) ->
        false
    | ( (Exp {pexp_desc= Pexp_let _} | Str {pstr_desc= Pstr_value _})
      , Ppat_constraint ({ppat_desc= Ppat_any | Ppat_tuple _}, _) ) ->
        false
    | ( (Exp {pexp_desc= Pexp_let _} | Str {pstr_desc= Pstr_value _})
      , Ppat_constraint _ ) ->
        true
    | Pat _, Ppat_constraint _
     |_, Ppat_unpack _
     |_, Ppat_constraint ({ppat_desc= Ppat_unpack _}, _)
     |( Pat
          { ppat_desc=
              ( Ppat_alias _ | Ppat_array _ | Ppat_constraint _
              | Ppat_construct _ | Ppat_variant _ ) }
      , Ppat_tuple _ )
     |( ( Pat
            { ppat_desc=
                ( Ppat_construct _ | Ppat_exception _ | Ppat_or _
                | Ppat_lazy _ | Ppat_tuple _ | Ppat_variant _ ) }
        | Exp {pexp_desc= Pexp_fun _} )
      , Ppat_alias _ )
     |( Pat {ppat_desc= Ppat_lazy _}
      , (Ppat_construct _ | Ppat_variant (_, Some _) | Ppat_or _) )
     |( Pat
          { ppat_desc=
              ( Ppat_construct _ | Ppat_exception _ | Ppat_tuple _
              | Ppat_variant _ ) }
      , Ppat_or _ )
     |Pat {ppat_desc= Ppat_lazy _}, Ppat_tuple _
     |Pat {ppat_desc= Ppat_tuple _}, Ppat_tuple _
     |Pat _, Ppat_lazy _
     |Pat _, Ppat_exception _
     |Exp {pexp_desc= Pexp_fun _}, Ppat_or _
     |Cl {pcl_desc= Pcl_fun _}, Ppat_constraint _
     |Cl {pcl_desc= Pcl_fun _}, Ppat_tuple _
     |Cl {pcl_desc= Pcl_fun _}, Ppat_construct _
     |Cl {pcl_desc= Pcl_fun _}, Ppat_alias _
     |( Exp
          { pexp_desc=
              Pexp_fun _ | Pexp_function _ | Pexp_match _ | Pexp_try _ }
      , Ppat_constraint _ )
     |Exp {pexp_desc= Pexp_let _}, Ppat_exception _
     |( Exp {pexp_desc= Pexp_fun _}
      , (Ppat_construct _ | Ppat_lazy _ | Ppat_tuple _ | Ppat_variant _) )
      ->
        true
    | (Str _ | Exp _), Ppat_lazy _ -> true
    | ( Pat {ppat_desc= Ppat_construct _ | Ppat_variant _; _}
      , (Ppat_construct (_, Some _) | Ppat_variant (_, Some _)) ) ->
        true
    | ( ( Exp {pexp_desc= Pexp_let (_, bindings, _)}
        | Str {pstr_desc= Pstr_value (_, bindings)} )
      , _ ) ->
        List.exists bindings ~f:(function
          | {pvb_pat; pvb_expr= {pexp_desc= Pexp_constraint _}} ->
              pvb_pat == pat
          | _ -> false )
    | Pld _, Ppat_constraint _ -> true
    | _ -> false

  (** Check if an exp is a prefix op that is not fully applied *)
  let is_displaced_prefix_op {ctx; ast= exp} =
    match (ctx, exp.pexp_desc) with
    | ( Exp {pexp_desc= Pexp_apply (e0, [(Nolabel, _)])}
      , Pexp_ident {txt= Lident i} )
      when e0 == exp && is_prefix_id i ->
        false
    | _, Pexp_ident {txt= Lident i} when is_prefix_id i -> true
    | _ -> false

  (** Check if an exp is an infix op that is not fully applied *)
  let is_displaced_infix_op {ctx; ast= exp} =
    match (ctx, exp.pexp_desc) with
    | ( Exp {pexp_desc= Pexp_apply (e0, [(Nolabel, _); (Nolabel, _)])}
      , Pexp_ident {txt= Lident i} )
      when e0 == exp && is_infix_id i && List.is_empty exp.pexp_attributes
      ->
        false
    | _, Pexp_ident {txt= Lident i} when is_infix_id i -> true
    | _ -> false

  (** 'Classes' of expressions which are parenthesized differently. *)
  type cls = Let_match | Match | Non_apply | Sequence | Then | ThenElse

  (** [mem_cls_exp cls exp] holds if [exp] is in the named class of
      expressions [cls]. *)
  let mem_cls_exp cls ast =
    match (ast, cls) with
    | {pexp_desc= Pexp_ifthenelse (_, _, None)}, (Non_apply | ThenElse)
     |{pexp_desc= Pexp_ifthenelse _}, Non_apply
     |{pexp_desc= Pexp_sequence _}, (Non_apply | Sequence | Then | ThenElse)
     |( {pexp_desc= Pexp_function _ | Pexp_match _ | Pexp_try _}
      , (Match | Let_match | Non_apply) )
     |( { pexp_desc=
            ( Pexp_fun _ | Pexp_let _ | Pexp_letexception _
            | Pexp_letmodule _ | Pexp_newtype _ | Pexp_open _ ) }
      , (Let_match | Non_apply) ) ->
        true
    | _ -> false

  (** [mem_cls_cl cls cl] holds if [cl] is in the named class of expressions
      [cls]. *)
  let mem_cls_cl cls ast =
    match (ast, cls) with
    | {pcl_desc= Pcl_fun _}, Non_apply -> true
    | _ -> false

  let marked_parenzed_inner_nested_match =
    let memo = Hashtbl.Poly.create () in
    register_reset (fun () -> Hashtbl.clear memo) ;
    memo

  let rec exposed_left_exp e =
    match e.pexp_desc with
    | Pexp_apply (op, _) -> is_prefix op || exposed_left_exp op
    | Pexp_field (e, _) -> exposed_left_exp e
    | _ -> false

  (** [exposed cls exp] holds if there is a right-most subexpression of
      [exp] which satisfies [mem_cls_exp cls] and is not parenthesized. *)
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
         |Pexp_construct
            ({txt= Lident "::"}, Some {pexp_desc= Pexp_tuple [_; e]})
         |Pexp_construct (_, Some e)
         |Pexp_extension (_, PStr [{pstr_desc= Pstr_eval (e, _)}])
         |Pexp_fun (_, _, _, e)
         |Pexp_ifthenelse (_, e, None)
         |Pexp_ifthenelse (_, _, Some e)
         |Pexp_lazy e
         |Pexp_newtype (_, e)
         |Pexp_open (_, _, e)
         |Pexp_sequence (_, e)
         |Pexp_setfield (_, _, e)
         |Pexp_setinstvar (_, e)
         |Pexp_variant (_, Some e) ->
            continue e
        | Pexp_let (_, _, e)
         |Pexp_letexception (_, e)
         |Pexp_letmodule (_, _, e) -> (
          match cls with
          | Match | Then | ThenElse -> continue e
          | _ -> false )
        | Pexp_match _ when match cls with Then -> true | _ -> false ->
            false
        | Pexp_function cases | Pexp_match (_, cases) | Pexp_try (_, cases)
          ->
            continue (List.last_exn cases).pc_rhs
        | Pexp_apply ({pexp_desc= Pexp_ident {txt}}, (Nolabel, _) :: args)
          when Option.is_some (index_op_set_sugar txt args) ->
            let _, _, e = Option.value_exn (index_op_set_sugar txt args) in
            continue e
        | Pexp_apply ({pexp_desc= Pexp_ident {txt}}, (Nolabel, _) :: args)
          when Option.is_some (index_op_get_sugar txt args) ->
            let _, indices =
              Option.value_exn (index_op_get_sugar txt args)
            in
            continue (List.last_exn indices)
        | Pexp_apply (_, args) -> continue (snd (List.last_exn args))
        | Pexp_tuple es -> continue (List.last_exn es)
        | Pexp_array _ | Pexp_coerce _ | Pexp_constant _ | Pexp_constraint _
         |Pexp_construct (_, None)
         |Pexp_extension _ | Pexp_field _ | Pexp_for _ | Pexp_ident _
         |Pexp_new _ | Pexp_object _ | Pexp_override _ | Pexp_pack _
         |Pexp_poly _ | Pexp_record _ | Pexp_send _ | Pexp_unreachable
         |Pexp_variant (_, None)
         |Pexp_while _ ->
            false
      in
      mem_cls_exp cls exp
      || Hashtbl.Poly.find_or_add memo (cls, exp) ~default:exposed_

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
      mem_cls_cl cls cl
      || Hashtbl.Poly.find_or_add memo (cls, cl) ~default:exposed_

  and mark_parenzed_inner_nested_match exp =
    let exposed_ () =
      let continue subexp =
        if not (parenze_exp (sub_exp ~ctx:(Exp exp) subexp)) then
          mark_parenzed_inner_nested_match subexp ;
        false
      in
      match exp.pexp_desc with
      | Pexp_assert e
       |Pexp_construct
          ({txt= Lident "::"}, Some {pexp_desc= Pexp_tuple [_; e]})
       |Pexp_construct (_, Some e)
       |Pexp_ifthenelse (_, e, None)
       |Pexp_ifthenelse (_, _, Some e)
       |Pexp_lazy e
       |Pexp_newtype (_, e)
       |Pexp_open (_, _, e)
       |Pexp_fun (_, _, _, e)
       |Pexp_sequence (_, e)
       |Pexp_setfield (_, _, e)
       |Pexp_setinstvar (_, e)
       |Pexp_variant (_, Some e) ->
          continue e
      | Pexp_let (_, _, e)
       |Pexp_letexception (_, e)
       |Pexp_letmodule (_, _, e) ->
          continue e
      | Pexp_extension (_, PStr [{pstr_desc= Pstr_eval (e, _)}]) -> (
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
      | Pexp_apply ({pexp_desc= Pexp_ident {txt}}, (Nolabel, _) :: args)
        when Option.is_some (index_op_set_sugar txt args) ->
          let _, _, e = Option.value_exn (index_op_set_sugar txt args) in
          continue e
      | Pexp_apply ({pexp_desc= Pexp_ident {txt}}, (Nolabel, _) :: args)
        when Option.is_some (index_op_get_sugar txt args) ->
          let _, indices = Option.value_exn (index_op_get_sugar txt args) in
          continue (List.last_exn indices)
      | Pexp_apply (_, args) -> continue (snd (List.last_exn args))
      | Pexp_tuple es -> continue (List.last_exn es)
      | Pexp_array _ | Pexp_coerce _ | Pexp_constant _ | Pexp_constraint _
       |Pexp_construct (_, None)
       |Pexp_extension _ | Pexp_field _ | Pexp_for _ | Pexp_ident _
       |Pexp_new _ | Pexp_object _ | Pexp_override _ | Pexp_pack _
       |Pexp_poly _ | Pexp_record _ | Pexp_send _ | Pexp_unreachable
       |Pexp_variant (_, None)
       |Pexp_while _ ->
          false
    in
    Hashtbl.Poly.find_or_add marked_parenzed_inner_nested_match exp
      ~default:exposed_
    |> (ignore : bool -> _)

  (** [parenze_exp {ctx; ast}] holds when expression [ast] should be
      parenthesized in context [ctx]. *)
  and parenze_exp ({ctx; ast= exp} as xexp) =
    let parenze () =
      let is_right_infix_arg ctx_desc exp =
        match ctx_desc with
        | Pexp_apply
            ({pexp_desc= Pexp_ident {txt= Lident i}}, _ :: (_, e2) :: _)
          when e2 == exp && is_infix_id i
               && Option.value_map ~default:false (prec_ast ctx)
                    ~f:(fun p -> Poly.(p < Apply) ) ->
            true
        | Pexp_tuple e1N -> List.last_exn e1N == xexp.ast
        | _ -> false
      in
      match ambig_prec (sub_ast ~ctx (Exp exp)) with
      | None -> false (* ctx not apply *)
      | Some (Some true) -> true (* exp is apply and ambig *)
      | _ -> (
        match ctx with
        | Exp {pexp_desc} ->
            if is_right_infix_arg pexp_desc exp then is_sequence exp
            else exposed_right_exp Non_apply exp
        | _ -> exposed_right_exp Non_apply exp )
    in
    assert (check_exp xexp ; true) ;
    is_displaced_prefix_op xexp
    || is_displaced_infix_op xexp
    || has_trailing_attributes_exp exp
    || Hashtbl.find marked_parenzed_inner_nested_match exp
       |> Option.value ~default:false
    ||
    match (ctx, exp) with
    | ( Exp {pexp_desc= Pexp_construct ({txt= Lident id}, _)}
      , {pexp_attributes= _ :: _} )
      when is_infix_id id ->
        true
    | Exp {pexp_desc= Pexp_extension _}, {pexp_desc= Pexp_tuple _} -> false
    | Pld _, {pexp_desc= Pexp_tuple _} -> false
    | Str {pstr_desc= Pstr_eval _}, {pexp_desc= Pexp_tuple _} -> false
    | Cl {pcl_desc= Pcl_apply _}, _ -> parenze ()
    | ( Exp {pexp_desc= Pexp_apply (op, (Nolabel, _) :: (Nolabel, e1) :: _)}
      , { pexp_desc=
            Pexp_apply ({pexp_desc= Pexp_ident {txt= Lident "not"}}, _) } )
      when is_infix op && not (e1 == exp) ->
        true
    | ( Exp {pexp_desc= Pexp_apply (e, _)}
      , {pexp_desc= Pexp_construct _ | Pexp_variant _} )
      when e == exp ->
        true
    | Exp {pexp_desc}, _ -> (
      match pexp_desc with
      | Pexp_extension
          ( _
          , PStr
              [ { pstr_desc=
                    Pstr_eval
                      ( { pexp_desc=
                            ( Pexp_function cases
                            | Pexp_match (_, cases)
                            | Pexp_try (_, cases) ) }
                      , _ ) } ] )
       |Pexp_function cases
       |Pexp_match (_, cases)
       |Pexp_try (_, cases) ->
          if !leading_nested_match_parens then
            List.iter cases ~f:(fun {pc_rhs; _} ->
                mark_parenzed_inner_nested_match pc_rhs ) ;
          List.exists cases ~f:(fun {pc_rhs} -> pc_rhs == exp)
          && exposed_right_exp Match exp
      | Pexp_ifthenelse (cnd, _, _) when cnd == exp -> false
      | Pexp_ifthenelse (_, thn, None) when thn == exp ->
          exposed_right_exp Then exp
      | Pexp_ifthenelse (_, thn, Some _) when thn == exp ->
          exposed_right_exp ThenElse exp
      | Pexp_ifthenelse (_, _, Some els) when els == exp -> is_sequence exp
      | Pexp_apply (({pexp_desc= Pexp_new _; _} as exp2), _)
        when exp2 == exp ->
          false
      | Pexp_apply
          ( ( { pexp_desc=
                  Pexp_extension
                    ( _
                    , PStr
                        [ { pstr_desc=
                              Pstr_eval ({pexp_desc= Pexp_new _}, []) } ] )
              } as exp2 )
          , _ )
        when exp2 == exp ->
          false
      | Pexp_record (flds, _)
        when List.exists flds ~f:(fun (_, e0) -> e0 == exp) ->
          exposed_right_exp Non_apply exp
          (* Non_apply is perhaps pessimistic *)
      | Pexp_record (_, Some ({pexp_desc= Pexp_apply (ident, [_])} as e0))
        when e0 == exp && is_prefix ident ->
          (* don't put parens around [!e] in [{ !e with a; b }] *)
          false
      | Pexp_record
          ( _
          , Some
              ( { pexp_desc=
                    ( Pexp_ident _ | Pexp_constant _ | Pexp_record _
                    | Pexp_field _ ) } as e0 ) )
        when e0 == exp ->
          false
      | Pexp_record (_, Some e0) when e0 == exp -> true
      | Pexp_sequence
          ( ( { pexp_desc=
                  Pexp_extension
                    ( _
                    , PStr
                        [ { pstr_desc=
                              Pstr_eval ({pexp_desc= Pexp_sequence _}, [])
                          } ] ) } as lhs )
          , _ )
        when lhs == exp ->
          true
      | Pexp_sequence (lhs, _) when lhs == exp ->
          exposed_right_exp Let_match exp
      | Pexp_sequence (_, rhs) when rhs == exp -> false
      | _ -> parenze () )
    | _ -> false

  (** [parenze_cl {ctx; ast}] holds when class expr [ast] should be
      parenthesized in context [ctx]. *)
  and parenze_cl ({ctx; ast= cl} as xcl) =
    assert (check_cl xcl ; true) ;
    match xcl with _ -> (
      match ambig_prec (sub_ast ~ctx (Cl cl)) with
      | None -> false
      | Some (Some true) -> true
      | _ -> exposed_right_cl Non_apply cl )

  let rec exposed_left_typ typ =
    match typ.ptyp_desc with
    | Ptyp_arrow (_, t, _) -> exposed_left_typ t
    | Ptyp_tuple l -> exposed_left_typ (List.hd_exn l)
    | Ptyp_object _ -> true
    | Ptyp_alias (typ, _) -> exposed_left_typ typ
    | _ -> false

  let rec exposed_right_typ typ =
    match typ.ptyp_desc with
    | Ptyp_arrow (_, _, t) -> exposed_right_typ t
    | Ptyp_tuple l -> exposed_right_typ (List.last_exn l)
    | Ptyp_object _ -> true
    | _ -> false
end

include In_ctx
include Requires_sub_terms
