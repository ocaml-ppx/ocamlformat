(**********************************************************************)
(*                                                                    *)
(*                            OCamlFormat                             *)
(*                                                                    *)
(*  Copyright (c) 2017-present, Facebook, Inc.  All rights reserved.  *)
(*                                                                    *)
(*  This source code is licensed under the MIT license found in the   *)
(*  LICENSE file in the root directory of this source tree.           *)
(*                                                                    *)
(**********************************************************************)

(** Abstract syntax tree term *)

open Migrate_ast
open Parsetree

(** Predicates recognizing special symbol identifiers. *)

let is_prefix_id i = match i.[0] with '!' | '?' | '~' -> true | _ -> false

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


let is_symbol_id i = is_prefix_id i || is_infix_id i

let is_symbol e = is_prefix e || is_infix e

(** Predicates recognizing classes of expressions. *)

let is_sequence exp =
  match exp.pexp_desc with Pexp_sequence _ -> true | _ -> false


let rec is_sugared_list exp =
  match exp.pexp_desc with
  | Pexp_construct ({txt= Lident "[]"}, None) -> true
  | Pexp_construct ({txt= Lident "::"}, Some {pexp_desc= Pexp_tuple [_; tl]}) ->
      is_sugared_list tl
  | _ -> false


let rec is_trivial exp =
  match exp.pexp_desc with
  | Pexp_constant _ | Pexp_field _ | Pexp_ident _ -> true
  | Pexp_construct (_, exp) -> Option.for_all exp ~f:is_trivial
  | _ -> false


let has_attributes {pexp_attributes} =
  List.exists pexp_attributes ~f:(function
    | {Location.txt= "ocaml.doc" | "ocaml.text"}, _ -> false
    | _ -> true )


(** Ast terms of various forms. *)
module T = struct
  type t =
    | Pld of payload
    | Typ of core_type
    | Pat of pattern
    | Exp of expression
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
    | Mty mt ->
        let si =
          let open Ast_helper in
          Sig.modtype (Mtd.mk {txt= ""; loc= Location.none} ~typ:mt)
        in
        Format.fprintf fs "Mty:@\n%a@\n%a" Pprintast.signature [si]
          Printast.interface [si]
    | Mod _ -> Format.pp_print_string fs "Mod"
    | Sig s ->
        Format.fprintf fs "Sig:@\n%a@\n%a" Pprintast.signature [s]
          Printast.interface [s]
    | Str s ->
        Format.fprintf fs "Str:@\n%a@\n%a" Pprintast.structure [s]
          Printast.implementation [s]
    | Top -> Format.pp_print_string fs "Top"

end

include T

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
  | HashOp
  | Dot
  | High
  | Atomic

(** Associativities of Ast terms. *)
type assoc = Left | Non | Right

(** Compute associativity from precedence, since associativity is uniform
    across precedence levels. *)
let assoc_of_prec = function
  | Low | Semi | LessMinus -> Non
  | ColonEqual -> Right
  | As -> Non
  | Comma -> Left
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

  val sub_pat : ctx:T.t -> pattern -> pattern xt

  val sub_exp : ctx:T.t -> expression -> expression xt

  val sub_mty : ctx:T.t -> module_type -> module_type xt

  val sub_mod : ctx:T.t -> module_expr -> module_expr xt

  val sub_sig : ctx:T.t -> signature_item -> signature_item xt

  val sub_str : ctx:T.t -> structure_item -> structure_item xt
end = struct
  open Requires_sub_terms

  type 'a xt = {ctx: T.t; ast: 'a}

  let sub_ast ~ctx ast = {ctx; ast}

  let sub_typ ~ctx typ = check parenze_typ {ctx; ast= typ}

  let sub_pat ~ctx pat = check parenze_pat {ctx; ast= pat}

  let sub_exp ~ctx exp = check parenze_exp {ctx; ast= exp}

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

  type cls = Let_match | Match | Non_apply | Sequence | Then

  val exposed : cls -> expression -> bool

  val prec_ast : T.t -> prec option

  val parenze_typ : core_type In_ctx.xt -> bool

  val parenze_pat : pattern In_ctx.xt -> bool

  val parenze_exp : expression In_ctx.xt -> bool
end = struct
  open In_ctx

  (* This module uses physical equality extensively to detect sub-terms. *)

  let ( == ) = Caml.Pervasives.( == )

  let ( != ) = Caml.Pervasives.( != )

  let dump fs ctx ast =
    Format.fprintf fs "ast: %a@\nctx: %a@\n" T.dump ast T.dump ctx


  let fail ctx ast exc =
    let bt = Caml.Printexc.get_backtrace () in
    dump Format.err_formatter ctx ast ;
    Format.eprintf "%s" bt ;
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
    match ctx with
    | Pld PTyp t1 -> assert (typ == t1)
    | Pld _ -> assert false
    | Typ ctx -> (
      match ctx.ptyp_desc with
      | Ptyp_any | Ptyp_var _ | Ptyp_extension _ -> assert false
      | Ptyp_alias (t1, _) | Ptyp_poly (_, t1) -> assert (typ == t1)
      | Ptyp_arrow (_, t1, t2) -> assert (typ == t1 || typ == t2)
      | Ptyp_tuple t1N | Ptyp_constr (_, t1N) -> assert (List.exists t1N ~f)
      | Ptyp_variant (r1N, _, _) ->
          assert (
            List.exists r1N ~f:(function
              | Rtag (_, _, _, t1N) -> List.exists t1N ~f
              | Rinherit t1 -> typ == t1 ) )
      | Ptyp_package (_, it1N) -> assert (List.exists it1N ~f:snd_f)
      | Ptyp_object _ | Ptyp_class _ ->
          internal_error "objects not implemented" [] )
    | Pat ctx -> (
      match ctx.ppat_desc with
      | Ppat_constraint (_, t1) -> assert (typ == t1)
      | _ -> assert false )
    | Exp ctx -> (
      match ctx.pexp_desc with
      | Pexp_constraint (_, t1)
       |Pexp_coerce (_, None, t1)
       |Pexp_poly (_, Some t1)
       |Pexp_extension (_, PTyp t1) ->
          assert (typ == t1)
      | Pexp_coerce (_, Some t1, t2) -> assert (typ == t1 || typ == t2)
      | Pexp_letexception (ext, _) -> assert (check_ext ext)
      | _ -> assert false )
    | Mty ctx -> (
      match ctx.pmty_desc with
      | Pmty_with (_, c1N) ->
          assert (
            List.exists c1N ~f:(function
              | Pwith_type (_, d1) | Pwith_typesubst d1 -> check_type d1
              | _ -> false ) )
      | _ -> assert false )
    | Mod _ -> assert false
    | Sig ctx -> (
      match ctx.psig_desc with
      | Psig_value {pval_type= t1} -> assert (typ == t1)
      | Psig_type (_, d1N) -> assert (List.exists d1N ~f:check_type)
      | Psig_typext typext -> assert (check_typext typext)
      | Psig_exception ext -> assert (check_ext ext)
      | _ -> assert false )
    | Str ctx -> (
      match ctx.pstr_desc with
      | Pstr_primitive {pval_type= t1} -> assert (typ == t1)
      | Pstr_type (_, d1N) -> assert (List.exists d1N ~f:check_type)
      | Pstr_typext typext -> assert (check_typext typext)
      | Pstr_exception ext -> assert (check_ext ext)
      | _ -> assert false )
    | Top -> assert false


  let check_typ ({ctx; ast= typ} as xtyp) =
    try check_typ xtyp
    with exc -> fail ctx (Typ typ) exc


  let check_pat {ctx; ast= pat} =
    match ctx with
    | Pld PPat (p1, _) -> assert (p1 == pat)
    | Pld _ -> assert false
    | Typ _ -> assert false
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
        | Ppat_any
         |Ppat_constant _
         |Ppat_construct (_, None)
         |Ppat_extension _
         |Ppat_interval _
         |Ppat_type _
         |Ppat_unpack _
         |Ppat_var _
         |Ppat_variant (_, None) ->
            assert false )
    | Exp ctx -> (
      match ctx.pexp_desc with
      | Pexp_apply _
       |Pexp_array _
       |Pexp_assert _
       |Pexp_coerce _
       |Pexp_constant _
       |Pexp_constraint _
       |Pexp_construct _
       |Pexp_extension _
       |Pexp_field _
       |Pexp_ident _
       |Pexp_ifthenelse _
       |Pexp_lazy _
       |Pexp_letexception _
       |Pexp_letmodule _
       |Pexp_new _
       |Pexp_newtype _
       |Pexp_object _
       |Pexp_open _
       |Pexp_override _
       |Pexp_pack _
       |Pexp_poly _
       |Pexp_record _
       |Pexp_send _
       |Pexp_sequence _
       |Pexp_setfield _
       |Pexp_setinstvar _
       |Pexp_tuple _
       |Pexp_unreachable
       |Pexp_variant _
       |Pexp_while _ ->
          assert false
      | Pexp_let (_, bindings, _) ->
          assert (List.exists bindings ~f:(fun {pvb_pat} -> pvb_pat == pat))
      | Pexp_function cases | Pexp_match (_, cases) | Pexp_try (_, cases) ->
          assert (
            List.exists cases ~f:(function
              | {pc_lhs} when pc_lhs == pat -> true
              | _ -> false ) )
      | Pexp_for (p, _, _, _, _) | Pexp_fun (_, _, p, _) -> assert (p == pat)
      )
    | Mty _ | Mod _ | Sig _ -> assert false
    | Str str -> (
      match str.pstr_desc with
      | Pstr_value (_, bindings) ->
          assert (List.exists bindings ~f:(fun {pvb_pat} -> pvb_pat == pat))
      | _ -> assert false )
    | Top -> assert false


  let check_pat ({ctx; ast= pat} as xpat) =
    try check_pat xpat
    with exc -> fail ctx (Pat pat) exc


  let check_exp {ctx; ast= exp} =
    match ctx with
    | Pld PPat (_, Some e1) -> assert (e1 == exp)
    | Pld _ -> assert false
    | Exp ctx -> (
        let f eI = eI == exp in
        let snd_f (_, eI) = eI == exp in
        match ctx.pexp_desc with
        | Pexp_construct
            ({txt= Lident "::"}, Some {pexp_desc= Pexp_tuple [e1; e2]}) ->
            assert (e1 == exp || e2 == exp)
        | Pexp_constant _
         |Pexp_extension _
         |Pexp_ident _
         |Pexp_new _
         |Pexp_object _
         |Pexp_pack _
         |Pexp_unreachable ->
            assert false
        | Pexp_let (_, bindings, e) ->
            assert (
              List.exists bindings ~f:(fun {pvb_expr} -> pvb_expr == exp)
              || e == exp )
        | (Pexp_match (e, _) | Pexp_try (e, _)) when e == exp -> ()
        | Pexp_function cases | Pexp_match (_, cases) | Pexp_try (_, cases) ->
            assert (
              List.exists cases ~f:(function
                | {pc_guard= Some g} when g == exp -> true
                | {pc_rhs} when pc_rhs == exp -> true
                | _ -> false ) )
        | Pexp_fun (_, default, _, body) ->
            assert (
              Option.value_map default ~default:false ~f || body == exp )
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
      | Pstr_eval (e0, []) -> assert (e0 == exp)
      | Pstr_value (_, bindings) ->
          assert (List.exists bindings ~f:(fun {pvb_expr} -> pvb_expr == exp)
          )
      | Pstr_eval (_, _ :: _)
       |Pstr_primitive _
       |Pstr_type _
       |Pstr_typext _
       |Pstr_exception _
       |Pstr_module _
       |Pstr_recmodule _
       |Pstr_modtype _
       |Pstr_open _
       |Pstr_class _
       |Pstr_class_type _
       |Pstr_include _
       |Pstr_attribute _
       |Pstr_extension _ ->
          assert false )
    | Mod {pmod_desc= Pmod_unpack e1} -> assert (e1 == exp)
    | Mod _ | Top | Typ _ | Pat _ | Mty _ | Sig _ -> assert false


  let check_exp ({ctx; ast= exp} as xexp) =
    try check_exp xexp
    with exc -> fail ctx (Exp exp) exc


  let rec is_simple (c: Conf.t) width ({ast= exp} as xexp) =
    let ctx = Exp exp in
    match exp.pexp_desc with
    | Pexp_array _
     |Pexp_constant _
     |Pexp_field _
     |Pexp_ident _
     |Pexp_record _
     |Pexp_tuple _
     |Pexp_variant _
     |Pexp_construct (_, None) ->
        true
    | Pexp_construct
        ({txt= Lident "::"}, Some {pexp_desc= Pexp_tuple [e1; e2]}) ->
        is_simple c width (sub_exp ~ctx e1)
        && is_simple c width (sub_exp ~ctx e2)
    | Pexp_construct (_, Some e0) -> is_simple c width (sub_exp ~ctx e0)
    | Pexp_apply ({pexp_desc= Pexp_ident {txt= Lident ":="}}, _) -> false
    | Pexp_apply (e0, e1N) ->
        is_trivial e0 && List.for_all e1N ~f:(snd >> is_trivial)
        && width xexp * 3 < c.margin
    | _ -> false


  (** [prec_ctx {ctx; ast}] is the precedence of the context of [ast] within
      [ctx], where [ast] is an immediate sub-term (modulo syntactic sugar)
      of [ctx].  Also returns whether [ast] is the left, right, or neither
      child of [ctx]. Meaningful for binary operators, otherwise returns
      [None]. *)
  let prec_ctx = function
    | { ctx= Sig {psig_desc= Psig_type (_, t1N)}
      ; ast= Typ ({ptyp_desc= Ptyp_arrow _} as typ) }
      when List.exists t1N ~f:(function
             | {ptype_kind= Ptype_variant cd1N} ->
                 List.exists cd1N ~f:(function
                   | {pcd_args= Pcstr_tuple t1N} ->
                       List.exists t1N ~f:(phys_equal typ)
                   | _ -> false )
             | _ -> false ) ->
        Some (Apply, Non)
    | { ctx= Sig {psig_desc= Psig_type (_, t1N)}
      ; ast= Typ ({ptyp_desc= Ptyp_tuple _} as typ) }
      when List.exists t1N ~f:(function
             | {ptype_kind= Ptype_variant cd1N} ->
                 List.exists cd1N ~f:(function
                   | {pcd_args= Pcstr_tuple t1N} ->
                       List.exists t1N ~f:(phys_equal typ)
                   | _ -> false )
             | _ -> false ) ->
        Some (InfixOp3, Non)
    | { ctx=
          Sig
            { psig_desc=
                Psig_exception {pext_kind= Pext_decl (Pcstr_tuple t1N, _)} }
      ; ast= Typ ({ptyp_desc= Ptyp_tuple _} as typ) }
      when List.mem ~equal:phys_equal t1N typ ->
        Some (InfixOp3, Non)
    | { ctx=
          Str
            { pstr_desc=
                Pstr_exception {pext_kind= Pext_decl (Pcstr_tuple t1N, _)} }
      ; ast= Typ ({ptyp_desc= Ptyp_tuple _} as typ) }
      when List.mem ~equal:phys_equal t1N typ ->
        Some (InfixOp3, Non)
    | {ctx= Str {pstr_desc}; ast= Typ typ} -> (
      match pstr_desc with
      | Pstr_type (_, td1N) ->
          List.find_map td1N ~f:(fun {ptype_kind} ->
              match ptype_kind with
              | Ptype_variant cd1N ->
                  List.find_map cd1N ~f:(fun {pcd_args} ->
                      match pcd_args with
                      | Pcstr_tuple t1N -> (
                        match t1N with
                        | [] -> None
                        | [_] -> Some (Apply, Non)
                        | _ ->
                            let tN = List.last_exn t1N in
                            if typ == tN then Some (InfixOp3, Right)
                            else
                              List.find_map t1N ~f:(fun tI ->
                                  if typ == tI then Some (InfixOp3, Left)
                                  else None ) )
                      | Pcstr_record _ -> None )
              | _ -> None )
      | Pstr_value _
       |Pstr_recmodule _
       |Pstr_class _
       |Pstr_class_type _
       |Pstr_eval _
       |Pstr_primitive _
       |Pstr_typext _
       |Pstr_exception _
       |Pstr_module _
       |Pstr_modtype _
       |Pstr_open _
       |Pstr_include _
       |Pstr_attribute _
       |Pstr_extension _ ->
          None )
    | {ctx= Typ {ptyp_desc}; ast= Typ typ} -> (
      match ptyp_desc with
      | Ptyp_arrow (_, t1, _) ->
          Some (MinusGreater, if t1 == typ then Left else Right)
      | Ptyp_tuple _ -> Some (InfixOp3, Non)
      | Ptyp_alias _ -> Some (As, Non)
      | Ptyp_constr (_, _ :: _ :: _) -> Some (Comma, Non)
      | Ptyp_constr _ -> Some (Apply, Non)
      | Ptyp_any
       |Ptyp_var _
       |Ptyp_object _
       |Ptyp_class _
       |Ptyp_variant _
       |Ptyp_poly _
       |Ptyp_package _
       |Ptyp_extension _ ->
          None )
    | {ast= Typ _} -> None
    | {ctx= Exp {pexp_desc}; ast= Exp exp} -> (
      match pexp_desc with
      | Pexp_tuple (e0 :: _) ->
          Some (Comma, if exp == e0 then Left else Right)
      | Pexp_construct
          ({txt= Lident "::"}, Some {pexp_desc= Pexp_tuple [_; e2]}) ->
          if is_sugared_list e2 then Some (Semi, Non)
          else Some (ColonColon, if exp == e2 then Right else Left)
      | Pexp_construct (_, Some _)
       |Pexp_assert _
       |Pexp_lazy _
       |Pexp_variant (_, Some _) ->
          Some (Apply, Non)
      | Pexp_apply (op, _) when op == exp -> Some (Low, Non)
      | Pexp_apply ({pexp_desc= Pexp_ident {txt= Lident i}}, [_]) -> (
        match i with
        | "~-" | "~+" -> Some (UMinus, Non)
        | _ ->
          match i.[0] with
          | '-' | '+' -> Some (UMinus, Non)
          | '!' | '?' | '~' -> Some (High, Non)
          | _ -> Some (Apply, Non) )
      | Pexp_apply
          ( { pexp_desc=
                Pexp_ident
                  { txt=
                      Ldot
                        ( Lident ("Array" | "String")
                        , ("get" | "set" as name) ) } }
          , (_, a1) :: (_, a2) :: _ ) ->
          if a1 == exp then
            match name with
            | "get" -> Some (Dot, Non)
            | "set" -> Some (LessMinus, Non)
            | _ -> impossible "matched pattern"
          else if a2 == exp then Some (Comma, Left)
          else Some (Comma, Right)
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
      | _ -> None )
    | {ctx= Exp _; ast= Pld _ | Top | Pat _ | Mty _ | Mod _ | Sig _ | Str _}
     |{ ctx= Pld _ | Top | Typ _ | Pat _ | Mty _ | Mod _ | Sig _ | Str _
      ; ast= Pld _ | Top | Pat _ | Exp _ | Mty _ | Mod _ | Sig _ | Str _ } ->
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
      | Ptyp_any
       |Ptyp_var _
       |Ptyp_constr _
       |Ptyp_object _
       |Ptyp_class _
       |Ptyp_variant _
       |Ptyp_poly _
       |Ptyp_extension _ ->
          None )
    | Exp {pexp_desc} -> (
      match pexp_desc with
      | Pexp_tuple _ -> Some Comma
      | Pexp_construct ({txt= Lident "::"}, Some {pexp_desc= Pexp_tuple _}) ->
          Some ColonColon
      | Pexp_construct (_, Some _) -> Some Apply
      | Pexp_constant (Pconst_integer (i, _) | Pconst_float (i, _)) -> (
        match i.[0] with '-' | '+' -> Some UMinus | _ -> Some Atomic )
      | Pexp_apply ({pexp_desc= Pexp_ident {txt= Lident i}}, [_]) -> (
        match i with
        | "~-" | "~+" -> Some UMinus
        | _ ->
          match i.[0] with
          | '-' | '+' -> Some UMinus
          | '!' | '?' | '~' -> Some High
          | _ -> Some Apply )
      | Pexp_apply
          ( { pexp_desc=
                Pexp_ident
                  {txt= Ldot (Lident ("Array" | "String"), ("get" | "set"))}
            }
          , _ :: _ :: _ ) ->
          Some Dot
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
      | Pexp_assert _ | Pexp_lazy _ | Pexp_variant (_, Some _) -> Some Apply
      | Pexp_setfield _ -> Some LessMinus
      | Pexp_setinstvar _ -> Some LessMinus
      | Pexp_field _ -> Some Dot
      | _ -> None )
    | Top | Pat _ | Mty _ | Mod _ | Sig _ | Str _ -> None


  (** [ambig_prec {ctx; ast}] holds when [ast] is ambiguous in its context
      [ctx], indicating that [ast] should be parenthesized.  Meaningful for
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
    else if Poly.(assoc_of_prec prec_ast = which_child && which_child <> Non)
    then (* which child and associativity match: no parens *)
      false
    else (* which child and assoc conflict: add parens *)
      true


  (** [parenze_typ {ctx; ast}] holds when type [ast] should be parenthesized
      in context [ctx]. *)
  let parenze_typ ({ctx; ast= typ} as xtyp) =
    assert (check_typ xtyp ; true) ;
    match xtyp with
    | { ctx= Exp {pexp_desc= Pexp_constraint _}
      ; ast= {ptyp_desc= Ptyp_package _} } ->
        true
    | _ ->
      match ambig_prec (sub_ast ~ctx (Typ typ)) with
      | Some Some true -> true
      | _ -> false


  (** [parenze_pat {ctx; ast}] holds when pattern [ast] should be
      parenthesized in context [ctx]. *)
  let parenze_pat ({ctx; ast= pat} as xpat) =
    assert (check_pat xpat ; true) ;
    match (ctx, pat.ppat_desc) with
    | ( Pat
          { ppat_desc=
              Ppat_construct
                ({txt= Lident "::"}, Some {ppat_desc= Ppat_tuple [_; tl]}) }
      , Ppat_construct ({txt= Lident "::"}, _) )
      when tl == pat ->
        false
    | ( Pat
          { ppat_desc=
              Ppat_construct
                ({txt= Lident "::"}, Some {ppat_desc= Ppat_tuple [_; _]}) }
      , _ ) ->
        true
    | ( Pat {ppat_desc= Ppat_construct _}
      , Ppat_construct ({txt= Lident "::"}, _) ) ->
        true
    | Exp {pexp_desc= Pexp_let (_, bindings, _)}, Ppat_tuple _ ->
        List.exists bindings ~f:(function
          | {pvb_pat; pvb_expr= {pexp_desc= Pexp_constraint _}} ->
              pvb_pat == pat
          | _ -> false )
    | Pat {ppat_desc= Ppat_construct _}, Ppat_constraint _
     |( Pat
          { ppat_desc=
              ( Ppat_alias _ | Ppat_constraint _ | Ppat_construct _
              | Ppat_variant _ ) }
      , Ppat_tuple _ )
     |( ( Pat
            { ppat_desc=
                ( Ppat_construct _ | Ppat_exception _ | Ppat_or _
                | Ppat_tuple _ ) } | Exp {pexp_desc= Pexp_fun _}
        | Str {pstr_desc= Pstr_value _} )
      , Ppat_alias _ )
     |( Pat {ppat_desc= Ppat_lazy _}
      , (Ppat_construct _ | Ppat_variant (_, Some _) | Ppat_or _) )
     |( Pat {ppat_desc= Ppat_construct _ | Ppat_exception _ | Ppat_tuple _}
      , Ppat_or _ )
     |Pat {ppat_desc= Ppat_tuple _}, (Ppat_constraint _ | Ppat_tuple _)
     |Pat {ppat_desc= Ppat_lazy _}, Ppat_lazy _
     |( Exp {pexp_desc= Pexp_fun _ | Pexp_function _ | Pexp_let _}
      , Ppat_constraint _ )
     |Exp {pexp_desc= Pexp_let _}, Ppat_exception _
     |( Exp {pexp_desc= Pexp_fun _}
      , (Ppat_construct _ | Ppat_lazy _ | Ppat_tuple _) ) ->
        true
    | _ -> false


  (** Check if an exp is a prefix op that is not fully applied *)
  let is_displaced_prefix_op {ctx; ast= exp} =
    match (ctx, exp.pexp_desc) with
    | Exp {pexp_desc= Pexp_apply (e0, _ :: _)}, Pexp_ident {txt= Lident i}
      when e0 == exp && is_prefix_id i ->
        false
    | _, Pexp_ident {txt= Lident i} when is_prefix_id i -> true
    | _ -> false


  (** Check if an exp is an infix op that is not fully applied *)
  let is_displaced_infix_op {ctx; ast= exp} =
    match (ctx, exp.pexp_desc) with
    | ( Exp {pexp_desc= Pexp_apply (e0, (Nolabel, _) :: (Nolabel, _) :: _)}
      , Pexp_ident {txt= Lident i} )
      when e0 == exp && is_infix_id i ->
        false
    | _, Pexp_ident {txt= Lident i} when is_infix_id i -> true
    | _ -> false


  (** 'Classes' of expressions which are parenthesized differently. *)
  type cls = Let_match | Match | Non_apply | Sequence | Then

  (** [mem_cls cls exp] holds if [exp] is in the named class of expressions
      [cls]. *)
  let mem_cls cls exp =
    match (exp.pexp_desc, cls) with
    | Pexp_ifthenelse (_, _, None), (Non_apply | Then)
     |Pexp_ifthenelse _, Non_apply
     |Pexp_sequence _, (Non_apply | Sequence)
     |( (Pexp_function _ | Pexp_match _ | Pexp_try _)
      , (Match | Let_match | Non_apply) )
     |( ( Pexp_fun _ | Pexp_let _ | Pexp_letexception _ | Pexp_letmodule _
        | Pexp_newtype _ | Pexp_open _ )
      , (Let_match | Non_apply) ) ->
        true
    | _ -> false


  (** [exposed cls exp] holds if there is a right-most subexpression of
      [exp] which satisfies [mem_cls cls] and is not parenthesized. *)
  let rec exposed =
    (* exponential without memoization *)
    let memo = Hashtbl.Poly.create () in
    fun cls exp ->
      let exposed_ () =
        let continue subexp =
          not (parenze_exp (sub_exp ~ctx:(Exp exp) subexp))
          && exposed cls subexp
        in
        match exp.pexp_desc with
        | Pexp_assert e
         |Pexp_construct
            ({txt= Lident "::"}, Some {pexp_desc= Pexp_tuple [_; e]})
         |Pexp_construct (_, Some e)
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
          match cls with Match | Then -> continue e | _ -> false )
        | Pexp_function cases | Pexp_match (_, cases) | Pexp_try (_, cases) ->
            continue (List.last_exn cases).pc_rhs
        | Pexp_apply (_, args) -> continue (snd (List.last_exn args))
        | Pexp_tuple es -> continue (List.last_exn es)
        | Pexp_array _
         |Pexp_coerce _
         |Pexp_constant _
         |Pexp_constraint _
         |Pexp_construct (_, None)
         |Pexp_extension _
         |Pexp_field _
         |Pexp_for _
         |Pexp_ident _
         |Pexp_new _
         |Pexp_object _
         |Pexp_override _
         |Pexp_pack _
         |Pexp_poly _
         |Pexp_record _
         |Pexp_send _
         |Pexp_unreachable
         |Pexp_variant (_, None)
         |Pexp_while _ ->
            false
      in
      mem_cls cls exp || Hashtbl.Poly.find_or_add memo exp ~default:exposed_


  (** [parenze_exp {ctx; ast}] holds when expression [ast] should be
      parenthesized in context [ctx]. *)
  and parenze_exp ({ctx; ast= exp} as xexp) =
    assert (check_exp xexp ; true) ;
    is_displaced_prefix_op xexp || is_displaced_infix_op xexp
    || has_attributes exp
    ||
    match ctx with
    | Exp {pexp_desc} -> (
      match pexp_desc with
      | Pexp_function cases | Pexp_match (_, cases) | Pexp_try (_, cases) ->
          List.exists cases ~f:(fun {pc_rhs} -> pc_rhs == exp)
          && (List.last_exn cases).pc_rhs != exp && exposed Match exp
      | Pexp_ifthenelse (cnd, _, _) when cnd == exp -> false
      | Pexp_ifthenelse (_, thn, els) when thn == exp ->
          is_sequence exp || Option.is_some els && exposed Then exp
      | Pexp_ifthenelse (_, _, Some els) when els == exp -> is_sequence exp
      | Pexp_record (flds, _)
        when List.exists flds ~f:(fun (_, e0) -> e0 == exp) ->
          exposed Non_apply exp (* Non_apply is perhaps pessimistic *)
      | Pexp_record (_, Some ({pexp_desc= Pexp_apply _} as e0))
        when e0 == exp ->
          true
      | Pexp_sequence (lhs, _) when lhs == exp -> exposed Let_match exp
      | Pexp_sequence (_, rhs) when rhs == exp -> false
      | _ ->
          let is_right_infix_arg ctx_desc exp =
            match ctx_desc with
            | Pexp_apply
                ({pexp_desc= Pexp_ident {txt= Lident i}}, _ :: (_, e2) :: _)
              when e2 == exp && is_infix_id i ->
                true
            | Pexp_tuple e1N -> List.last_exn e1N == exp
            | _ -> false
          in
          match ambig_prec (sub_ast ~ctx (Exp exp)) with
          | None -> false (* ctx not apply *)
          | Some Some true -> true (* exp is apply and ambig *)
          | _ ->
              if is_right_infix_arg pexp_desc exp then is_sequence exp
              else exposed Non_apply exp )
    | _ -> false

end

include In_ctx
include Requires_sub_terms
