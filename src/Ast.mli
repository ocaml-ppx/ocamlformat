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

(** Abstract syntax tree terms *)

open Migrate_ast
open Parsetree

val init : Conf.t -> unit
(** Initialize internal state *)

val is_prefix : expression -> bool
(** Holds of prefix symbol expressions. *)

val is_infix_id : string -> bool
(** Holds of infix symbols. *)

val is_infix : expression -> bool
(** Holds of infix symbol expressions. *)

val index_op_get_lid : Longident.t -> (string * Char.t * Char.t) option

val index_op_set_lid : Longident.t -> (string * Char.t * Char.t) option

val index_op_get_sugar :
     Longident.t Location.loc
  -> (Asttypes.arg_label * expression) list
  -> ((string * Char.t * Char.t) Location.loc * expression list) option

val index_op_set_sugar :
     Longident.t Location.loc
  -> (Asttypes.arg_label * expression) list
  -> ((string * Char.t * Char.t) Location.loc * expression list * expression)
     option

val is_monadic_binding_id : string -> bool

val is_monadic_binding : expression -> bool

val is_symbol_id : string -> bool
(** Holds of prefix or infix symbols. *)

val is_symbol : expression -> bool
(** Holds of prefix or infix symbols. *)

val is_sugared_list : expression -> bool
(** Holds of expressions that can be sugared into [\[e1; ...; eN\]] form. *)

val doc_atrs :
     ?acc:(string Location.loc * bool) list
  -> attributes
  -> (string Location.loc * bool) list option * attributes

val longident_is_simple : Conf.t -> Longident.t -> bool

val module_expr_is_simple : module_expr -> bool

val module_type_is_simple : module_type -> bool

val class_decl_is_simple : class_expr -> bool

val class_type_is_simple : class_type -> bool

val type_decl_is_simple : type_declaration -> bool

type toplevel_item =
  [`Item of structure_item | `Directive of toplevel_directive]

(** Ast terms of various forms. *)
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
  | Tli of toplevel_item
  | Top

val break_between :
     Source.t
  -> cmts:'a
  -> has_cmts_before:('a -> Location.t -> bool)
  -> has_cmts_after:('a -> Location.t -> bool)
  -> t * Conf.t
  -> t * Conf.t
  -> bool

val attributes : t -> attributes

val location : t -> Location.t

val dump : Format.formatter -> t -> unit
(** Debug: Dump the representation of an Ast term. *)

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
  | Dot  (** [x.y] and [x#y] *)
  | High
  | Atomic

(** Associativities of Ast terms *)
type assoc = Left | Non | Right

val assoc_of_prec : prec -> assoc
(** [assoc_of_prec prec] is the associativity of Ast terms with precedence
    [prec]. (Associativity is uniform across precedence levels.) *)

type 'a xt = private {ctx: t; ast: 'a}
(** Term-in-context [{ctx; ast}] records that [ast] is (considered to be) an
    immediate sub-term of [ctx]. *)

val sub_typ : ctx:t -> core_type -> core_type xt
(** Construct a core_type-in-context. *)

val sub_cty : ctx:t -> class_type -> class_type xt
(** Construct a class_type-in-context. *)

val sub_pat : ctx:t -> pattern -> pattern xt
(** Construct a pattern-in-context. *)

val sub_exp : ctx:t -> expression -> expression xt
(** Construct a expression-in-context. *)

val sub_cl : ctx:t -> class_expr -> class_expr xt
(** Construct a class_expr-in-context. *)

val sub_mty : ctx:t -> module_type -> module_type xt
(** Construct a module_type-in-context. *)

val sub_mod : ctx:t -> module_expr -> module_expr xt
(** Construct a module_expr-in-context. *)

val sub_sig : ctx:t -> signature_item -> signature_item xt
(** Construct a signature_item-in-context. *)

val sub_str : ctx:t -> structure_item -> structure_item xt
(** Construct a structure_item-in-context. *)

val is_simple : Conf.t -> (expression xt -> int) -> expression xt -> bool
(** Holds of "simple" expressions: constants and constructor and function
    applications of other simple expressions. *)

val exposed_left_typ : core_type -> bool

val exposed_right_typ : core_type -> bool

(** 'Classes' of expressions which are parenthesized differently. *)
type cls = Let_match | Match | Non_apply | Sequence | Then | ThenElse

val exposed_right_exp : cls -> expression -> bool
(** [exposed_right_exp cls exp] holds if there is a right-most subexpression
    of [exp] which is of class [cls] and is not parenthesized. *)

val exposed_left_exp : expression -> bool
(** [exposed_left_exp exp] holds if the left-most subexpression of [exp] is
    a prefix operators. *)

val prec_ast : t -> prec option
(** [prec_ast ast] is the precedence of [ast]. Meaningful for binary
    operators, otherwise returns [None]. *)

val parenze_typ : core_type xt -> bool
(** [parenze_typ xtyp] holds when core_type-in-context [xtyp] should be
    parenthesized. *)

val parenze_cty : class_type xt -> bool
(** [parenze_cty xcty] holds when class_type-in-context [xcty] should be
    parenthesized. *)

val parenze_cl : class_expr xt -> bool
(** [parenze_cl xcl] holds when class-in-context [xcl] should be
    parenthesized. *)

val parenze_pat : pattern xt -> bool
(** [parenze_pat xpat] holds when pattern-in-context [xpat] should be
    parenthesized. *)

val parenze_exp : expression xt -> bool
(** [parenze_exp xexp] holds when expression-in-context [xexp] should be
    parenthesized. *)

val parenze_nested_exp : expression xt -> bool
(** [parenze_nested_exp xexp] holds when nested expression-in-context [xexp]
    should be parenthesized. *)

val parenze_mty : module_type xt -> bool
(** [parenze_mty xmty] holds when module_type-in-context [xmty] should be
    parenthesized. *)

val parenze_mod : module_expr xt -> bool
(** [parenze_mod xmod] holds when module_expr-in-context [xmod] should be
    parenthesized. *)
