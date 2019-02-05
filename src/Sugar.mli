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

val arrow_typ :
  Cmts.t -> core_type Ast.xt -> (arg_label * core_type Ast.xt) list
(** [arrow_typ cmts ty] returns the list of labeled sub-arrow types of the
    type [ty]. [cmts] are relocated on the sub-arrow types to have more
    precise positions. *)

val class_arrow_typ :
     Cmts.t
  -> class_type Ast.xt
  -> ( arg_label
     * [`class_type of class_type Ast.xt | `core_type of core_type Ast.xt]
     )
     list
(** [class_arrow_typ cmts ty] returns the list of labeled sub_arrow types of
    the class type [ty]. [cmts] are relocated to the sub-arrow types to have
    more precise positions. *)

val or_pat :
  ?allow_attribute:bool -> Cmts.t -> pattern Ast.xt -> pattern Ast.xt list
(** [or_pat allow_attribute cmts pat] returns the list of patterns of a
    pattern disjunction. [cmts] are relocated to the sub-patterns to have
    more precise positions. [allow_attribute] is set by default, otherwise
    patterns with not empty attributes are not processed (i.e. they are
    returned without modification). *)

type arg_kind =
  | Val of arg_label * pattern Ast.xt * expression Ast.xt option
  | Newtypes of string loc list

val fun_ :
     Cmts.t
  -> ?will_keep_first_ast_node:bool
  -> expression Ast.xt
  -> arg_kind sexp_list * expression Ast.xt
(** [fun_ cmts will_keep_first_ast_node exp] returns the list of arguments
    and the body of the function [exp]. [cmts] are relocated to the
    arguments or the function body to have more precise positions.
    [will_keep_first_ast_node] is set by default, otherwise the [exp] is
    returned without modification. *)

val cl_fun :
     ?will_keep_first_ast_node:bool
  -> Cmts.t
  -> class_expr Ast.xt
  -> arg_kind list * class_expr Ast.xt
(** [cl_fun will_keep_first_ast_node cmts exp] returns the list of arguments
    and the body of the function [exp]. [cmts] are relocated to the
    arguments or the function body to have more precise positions.
    [will_keep_first_ast_node] is set by default, otherwise the [exp] is
    returned without modification. *)

val infix :
     Cmts.t
  -> Ast.prec option
  -> expression Ast.xt
  -> (expression Ast.xt option * (arg_label * expression Ast.xt) list) list
(** [infix cmts prec exp] returns the infix operator and the list of
    operands applied to this operator from expression [exp]. [prec] is the
    precedence of the infix operator. [cmts] are relocated to the operand to
    have more precise positions. *)

val list_pat :
     Cmts.t
  -> pattern
  -> ((Warnings.loc list * pattern Ast.xt) list * Warnings.loc) option
(** [list_pat cmts pat] returns a list of patterns if [pat] is a pattern
    corresponding to a list (empty list or (::) application). [cmts] are
    relocated to the list elements to have more precise positions. *)

val list_exp :
     Cmts.t
  -> expression
  -> ((Warnings.loc list * expression Ast.xt) list * Warnings.loc) option
(** [list_exp cmts exp] returns a list of expressions if [exp] is an
    expression corresponding to a list (empty list or (::) application).
    [cmts] are relocated to the list elements to have more positions. *)

val infix_cons :
  expression Ast.xt -> (Warnings.loc list * expression Ast.xt) list
(** [infix_cons exp] returns a list of expressions if [exp] is an expression
    corresponding to a list ((::) application). *)

val ite :
     Cmts.t
  -> expression Ast.xt
  -> (expression Ast.xt option * expression Ast.xt * attributes) list
(** [ite cmts exp] returns a list of conditional expressions from cascading
    if-then-else expressions, e.g.:

    {[
        if c1 then e1
        else if c2 then e2
        else e3
    ]}

    will return the following list:
    [(Some c1, e1); (Some c2, e2); (None, e3)] (attributes are voluntarily
    omitted. [cmts] are relocated to the sub-expressions to have more
    precise positions. *)

val sequence : Cmts.t -> expression Ast.xt -> expression Ast.xt list
(** [sequence cmts exp] returns the list of expressions from a sequence of
    expressions [exp]. [cmts] are relocated to the sub-expressions to have
    more precise positions. *)

val functor_type :
     Cmts.t
  -> for_functor_kw:bool
  -> module_type Ast.xt
  -> (label loc * module_type Ast.xt option) list * module_type Ast.xt
(** [functor_type cmts for_functor_kw m] returns the list of module types
    applied to the functor of module type [m]. [for_functor_kw] indicates if
    the keyword [functor] is used. The sugar is different when used with the
    [functor] keyword. The syntax M(A : A)(B : B) cannot handle [_] as
    module name. [cmts] are relocated to the functor arguments or body to
    have more precise positions. *)

val functor_ :
     Cmts.t
  -> for_functor_kw:bool
  -> module_expr Ast.xt
  -> (label loc * module_type Ast.xt option) list * module_expr Ast.xt
(** [functor_ cmts for_functor_kw m] returns the list of module types
    applied to the functor of module [m]. [for_functor_kw] indicates if the
    keyword [functor] is used. The sugar is different when used with the
    [functor] keyword. The syntax M(A : A)(B : B) cannot handle [_] as
    module name. [cmts] are relocated to the functor arguments or body to
    have more precise positions. *)

val mod_with :
     module_type Ast.xt
  -> (with_constraint list * Warnings.loc) list * module_type Ast.xt
(** [mod_with m] returns the list of [with type] constraints of module type
    [m]. *)

val polynewtype :
     Cmts.t
  -> pattern
  -> expression
  -> (pattern Ast.xt * label loc list * core_type Ast.xt * expression Ast.xt)
     option
(** [polynewtype cmts pat exp] returns expression of a type-constrained
    pattern [pat] with body [exp]. [cmts] are relocated to the
    sub-expressions to have more precise positions. e.g.:

    {[
        let f: 'r 's. 'r 's t = fun (type r) -> fun (type s) -> (e : r s t)
    ]}

    Can be rewritten as:

    {[
        let f: type r s. r s t = e
    ]} *)
