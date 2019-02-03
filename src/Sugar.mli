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

val sugar_class_arrow_typ :
     Cmts.t
  -> class_type Ast.xt
  -> ( arg_label
     * [> `class_type of class_type Ast.xt | `core_type of core_type Ast.xt]
     )
     list

val sugar_or_pat :
  ?allow_attribute:bool -> Cmts.t -> pattern Ast.xt -> pattern Ast.xt list

type arg_kind =
  | Val of arg_label * pattern Ast.xt * expression Ast.xt option
  | Newtypes of string loc list

val sugar_fun :
     Cmts.t
  -> ?will_keep_first_ast_node:bool
  -> expression Ast.xt
  -> arg_kind sexp_list * expression Ast.xt

val sugar_cl_fun :
     ?will_keep_first_ast_node:bool
  -> Cmts.t
  -> class_expr Ast.xt
  -> arg_kind list * class_expr Ast.xt

val sugar_infix :
     Cmts.t
  -> Ast.prec option
  -> expression Ast.xt
  -> (expression Ast.xt option * (arg_label * expression Ast.xt) list) list

val sugar_list_pat :
     Cmts.t
  -> pattern
  -> ((Warnings.loc list * pattern Ast.xt) list * Warnings.loc) option

val sugar_list_exp :
     Cmts.t
  -> expression
  -> ((Warnings.loc list * expression Ast.xt) list * Warnings.loc) option

val sugar_infix_cons :
  expression Ast.xt -> (Warnings.loc list * expression Ast.xt) list

val sugar_ite :
     Cmts.t
  -> expression Ast.xt
  -> (expression Ast.xt option * expression Ast.xt * attributes) list

val sugar_sequence :
     Conf.t
  -> Cmts.t
  -> (expression Ast.xt -> int)
  -> expression Ast.xt
  -> expression Ast.xt list list

val sugar_functor_type :
     Cmts.t
  -> for_functor_kw:bool
  -> module_type Ast.xt
  -> (label loc * module_type Ast.xt option) list * module_type Ast.xt
(** The sugar is different when used with the [functor] keyword. The syntax
    M(A : A)(B : B) cannot handle [_] as module name. *)

val sugar_functor :
     Cmts.t
  -> for_functor_kw:bool
  -> module_expr Ast.xt
  -> (label loc * module_type Ast.xt option) list * module_expr Ast.xt
(** The sugar is different when used with the [functor] keyword. The syntax
    M(A : A)(B : B) cannot handle [_] as module name. *)

val sugar_mod_with :
     module_type Ast.xt
  -> (with_constraint list * Warnings.loc) list * module_type Ast.xt
