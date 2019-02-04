(**********************************************************************
 *                                                                    *
 *                            OCamlFormat                             *
 *                                                                    *
 *  Copyright (c) 2019-present, Facebook, Inc.  All rights reserved.  *
 *                                                                    *
 *  This source code is licensed under the MIT license found in the   *
 *  LICENSE file in the root directory of this source tree.           *
 *                                                                    *
 **********************************************************************)

open Migrate_ast
open Asttypes
open Parsetree

val sugar_arrow_typ :
  Cmts.t -> core_type Ast.xt -> (arg_label * core_type Ast.xt) list
(** [sugar_arrow_typ cmts ty] returns the list of labeled sub-arrow types of
    the type [ty]. [cmts] are relocated on the sub-arrow types to have more
    precise positions. *)

val sugar_class_arrow_typ :
     Cmts.t
  -> class_type Ast.xt
  -> ( arg_label
     * [`class_type of class_type Ast.xt | `core_type of core_type Ast.xt]
     )
     list
(** [sugar_class_arrow_typ cmts ty] returns the list of labeled sub_arrow
    types of the class type [ty]. [cmts] are relocated to the sub-arrow
    types to have more precise positions. *)

val sugar_or_pat :
  ?allow_attribute:bool -> Cmts.t -> pattern Ast.xt -> pattern Ast.xt list
(** [sugar_or_pat allow_attribute cmts pat] returns the list of patterns of
    a pattern disjunction. [cmts] are relocated to the sub-patterns to have
    more precise positions. [allow_attribute] is set by default, otherwise
    patterns with not empty attributes are not processed (i.e. they are
    returned without modification). *)

type arg_kind =
  | Val of arg_label * pattern Ast.xt * expression Ast.xt option
  | Newtypes of string loc list

val sugar_fun :
     Cmts.t
  -> ?will_keep_first_ast_node:bool
  -> expression Ast.xt
  -> arg_kind sexp_list * expression Ast.xt
(** [sugar_fun cmts will_keep_first_ast_node exp] returns the list of
    arguments and the body of the function [exp]. [cmts] are relocated to
    the arguments or the function body to have more precise positions.
    [will_keep_first_ast_node] is set by default, otherwise the [exp] is
    returned without modification. *)

val sugar_cl_fun :
     ?will_keep_first_ast_node:bool
  -> Cmts.t
  -> class_expr Ast.xt
  -> arg_kind list * class_expr Ast.xt
(** [sugar_cl_fun will_keep_first_ast_node cmts exp] returns the list of
    arguments and the body of the function [exp]. [cmts] are relocated to
    the arguments or the function body to have more precise positions.
    [will_keep_first_ast_node] is set by default, otherwise the [exp] is
    returned without modification. *)

val sugar_infix :
     Cmts.t
  -> Ast.prec option
  -> expression Ast.xt
  -> (expression Ast.xt option * (arg_label * expression Ast.xt) list) list
(** [sugar_infix cmts prec exp] returns the infix operator and the list of
    operands applied to this operator from expression [exp]. [prec] is the
    precedence of the infix operator. [cmts] are relocated to the operand to
    have more precise positions. *)

val sugar_list_pat :
     Cmts.t
  -> pattern
  -> ((Warnings.loc list * pattern Ast.xt) list * Warnings.loc) option
(** [sugar_list_pat cmts pat] returns a list of patterns if [pat] is a
    pattern corresponding to a list (empty list or (::) application). [cmts]
    are relocated to the list elements to have more precise positions. *)

val sugar_list_exp :
     Cmts.t
  -> expression
  -> ((Warnings.loc list * expression Ast.xt) list * Warnings.loc) option
(** [sugar_list_exp cmts exp] returns a list of expressions if [exp] is an
    expression corresponding to a list (empty list or (::) application).
    [cmts] are relocated to the list elements to have more positions. *)

val sugar_infix_cons :
  expression Ast.xt -> (Warnings.loc list * expression Ast.xt) list
(** [sugar_infix_cons exp] returns a list of expressions if [exp] is an
    expression corresponding to a list ((::) application). *)

val sugar_ite :
     Cmts.t
  -> expression Ast.xt
  -> (expression Ast.xt option * expression Ast.xt * attributes) list
(** [sugar_ite cmts exp] returns a list of conditional expressions from
    cascading if-then-else expressions, e.g.:

    {v
         if c1 then e1
         else if c2 then e2
         else e3
    v}

    will be sugarized as [\[(Some c1, e1); (Some c2, e2); (None, e3)\]]
    (attributes are voluntarily omitted. [cmts] are relocated to the
    sub-expressions to have more precise positions. *)

val sugar_sequence :
     Conf.t
  -> Cmts.t
  -> (expression Ast.xt -> int)
  -> expression Ast.xt
  -> expression Ast.xt list list
(** [sugar_sequence conf cmts width exp] returns the list of expressions
    from a sequence of expressions [exp]. [width] is the function computing
    the width (number of columns) of an expression. Sub-sequences will be
    put in different sub-lists if one of they do not fit within the margin.
    [cmts] are relocated to the sub-expressions to have more precise
    positions. *)

val sugar_functor_type :
     Cmts.t
  -> for_functor_kw:bool
  -> module_type Ast.xt
  -> (label loc * module_type Ast.xt option) list * module_type Ast.xt
(** [sugar_functor_type cmts for_functor_kw m] returns the list of module
    types applied to the functor of module type [m]. [for_functor_kw]
    indicates if the keyword [functor] is used. The sugar is different when
    used with the [functor] keyword. The syntax M(A : A)(B : B) cannot
    handle [_] as module name. [cmts] are relocated to the functor arguments
    or body to have more precise positions. *)

val sugar_functor :
     Cmts.t
  -> for_functor_kw:bool
  -> module_expr Ast.xt
  -> (label loc * module_type Ast.xt option) list * module_expr Ast.xt
(** [sugar_functor cmts for_functor_kw m] returns the list of module types
    applied to the functor of module [m]. [for_functor_kw] indicates if the
    keyword [functor] is used. The sugar is different when used with the
    [functor] keyword. The syntax M(A : A)(B : B) cannot handle [_] as
    module name. [cmts] are relocated to the functor arguments or body to
    have more precise positions. *)

val sugar_mod_with :
     module_type Ast.xt
  -> (with_constraint list * Warnings.loc) list * module_type Ast.xt
(** [sugar_mod_with m] returns the list of [with type] constraints of module
    type [m]. *)
