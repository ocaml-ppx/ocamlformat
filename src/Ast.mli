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

val is_prefix : expression -> bool
(** Holds of prefix symbol expressions. *)

val is_infix_id : string -> bool
(** Holds of infix symbols. *)

val is_infix : expression -> bool
(** Holds of infix symbol expressions. *)

val is_symbol_id : string -> bool
(** Holds of prefix or infix symbols. *)

val is_symbol : expression -> bool
(** Holds of prefix or infix symbols. *)

val is_sugared_list : expression -> bool
(** Holds of expressions that can be sugared into [[e1; ...; eN]] form. *)

(** Ast terms of various forms. *)
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
  | Dot
  | HashOp
  | High
  | Atomic

(** Associativities of Ast terms *)
type assoc = Left | Non | Right

val assoc_of_prec : prec -> assoc
(** [assoc_of_prec prec] is the associativity of Ast terms with precedence
    [prec]. (Associativity is uniform across precedence levels.) *)

(** Term-in-context [{ctx; ast}] records that [ast] is (considered to be) an
    immediate sub-term of [ctx]. *)
type 'a xt = private {ctx: t; ast: 'a}

val sub_typ : ctx:t -> core_type -> core_type xt
(** Construct a core_type-in-context. *)

val sub_pat : ctx:t -> pattern -> pattern xt
(** Construct a pattern-in-context. *)

val sub_exp : ctx:t -> expression -> expression xt
(** Construct a expression-in-context. *)

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

type ltgt = LT | GT

val typ_exposed_left : ltgt -> core_type -> bool

val typ_exposed_right : ltgt -> core_type -> bool

(** 'Classes' of expressions which are parenthesized differently. *)
type cls = Let_match | Match | Non_apply | Sequence | Then | ThenElse

val exposed : cls -> expression -> bool
(** [exposed cls exp] holds if there is a right-most subexpression of [exp]
    which is of class [cls] and is not parenthesized. *)

val prec_ast : t -> prec option
(** [prec_ast ast] is the precedence of [ast]. Meaningful for binary
    operators, otherwise returns [None]. *)

val parenze_typ : core_type xt -> bool
(** [parenze_typ xtyp] holds when core_type-in-context [xtyp] should be
    parenthesized. *)

val parenze_pat : pattern xt -> bool
(** [parenze_pat xpat] holds when pattern-in-context [xpat] should be
    parenthesized. *)

val parenze_exp : expression xt -> bool
(** [parenze_exp xexp] holds when expression-in-context [xexp] should be
    parenthesized. *)
