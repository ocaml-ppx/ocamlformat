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

open Migrate_ast
open Asttypes
open Extended_ast

val arrow_typ :
     Cmts.t
  -> core_type Ast.xt
  -> (Location.t * arg_label * core_type Ast.xt) list
(** [arrow_typ cmts ty] returns the list of labeled sub-arrow types of the
    type [ty]. *)

val class_arrow_typ :
     Cmts.t
  -> class_type Ast.xt
  -> ( arg_label
     * [`class_type of class_type Ast.xt | `core_type of core_type Ast.xt] )
     list
(** [class_arrow_typ cmts ty] returns the list of labeled sub_arrow types of
    the class type [ty]. *)

val or_pat :
  ?allow_attribute:bool -> Cmts.t -> pattern Ast.xt -> pattern Ast.xt list
(** [or_pat allow_attribute cmts pat] returns the list of patterns of a
    pattern disjunction. [allow_attribute] is set by default, otherwise
    patterns with not empty attributes are not processed (i.e. they are
    returned without modification). *)

type arg_kind =
  | Val of arg_label * pattern Ast.xt * expression Ast.xt option
  | Newtypes of string loc list

val fun_ :
     Cmts.t
  -> ?will_keep_first_ast_node:bool
  -> expression Ast.xt
  -> arg_kind list * expression Ast.xt
(** [fun_ cmts will_keep_first_ast_node exp] returns the list of arguments
    and the body of the function [exp]. [will_keep_first_ast_node] is set by
    default, otherwise the [exp] is returned without modification. *)

val cl_fun :
     ?will_keep_first_ast_node:bool
  -> Cmts.t
  -> class_expr Ast.xt
  -> arg_kind list * class_expr Ast.xt
(** [cl_fun will_keep_first_ast_node cmts exp] returns the list of arguments
    and the body of the function [exp]. [will_keep_first_ast_node] is set by
    default, otherwise the [exp] is returned without modification. *)

val infix :
     Cmts.t
  -> Prec.t option
  -> expression Ast.xt
  -> (expression Ast.xt option * (arg_label * expression Ast.xt) list) list
(** [infix cmts prec exp] returns the infix operator and the list of operands
    applied to this operator from expression [exp]. [prec] is the precedence
    of the infix operator. *)

val infix_cons :
     Cmts.t
  -> expression Ast.xt
  -> (Longident.t loc option * expression Ast.xt) list
(** [infix_cons exp] returns a list of expressions if [exp] is an expression
    corresponding to a list ((::) application). *)

val ite :
     Cmts.t
  -> expression Ast.xt
  -> (expression Ast.xt option * expression Ast.xt * attributes) list
(** [ite cmts exp] returns a list of conditional expressions from cascading
    if-then-else expressions, e.g.:

    {[ if c1 then e1 else if c2 then e2 else e3 ]}

    will return the following list:
    [(Some c1, e1); (Some c2, e2); (None, e3)]. *)

val sequence :
  Cmts.t -> expression Ast.xt -> (label loc option * expression Ast.xt) list
(** [sequence cmts exp] returns the list of expressions (with the optional
    extension) from a sequence of expressions [exp]. *)

type functor_arg =
  | Unit
  | Named of label option loc * module_type Ast.xt
      (** Equivalent of the [functor_parameter] type with a contextualized
          module type. *)

val functor_type :
     Cmts.t
  -> for_functor_kw:bool
  -> source_is_long:(module_type -> bool)
  -> module_type Ast.xt
  -> functor_arg loc list * module_type Ast.xt
(** [functor_type cmts for_functor_kw m] returns the list of module types
    applied to the functor of module type [m]. [for_functor_kw] indicates if
    the keyword [functor] is used. *)

val functor_ :
     Cmts.t
  -> for_functor_kw:bool
  -> source_is_long:(module_expr -> bool)
  -> module_expr Ast.xt
  -> functor_arg loc list * module_expr Ast.xt
(** [functor_ cmts for_functor_kw m] returns the list of module types applied
    to the functor of module [m]. [for_functor_kw] indicates if the keyword
    [functor] is used. *)

val mod_with :
     module_type Ast.xt
  -> (with_constraint list * Warnings.loc * attributes) list
     * module_type Ast.xt
(** [mod_with m] returns the list of [with type] constraints of module type
    [m]. *)

module Let_binding : sig
  type t =
    { lb_op: string loc
    ; lb_pat: pattern Ast.xt
    ; lb_typ:
        [ `Polynewtype of label loc list * core_type Ast.xt
        | `Coerce of core_type Ast.xt option * core_type Ast.xt
        | `Other of arg_kind list * core_type Ast.xt
        | `None of arg_kind list ]
    ; lb_exp: expression Ast.xt
    ; lb_pun: bool
    ; lb_attrs: attribute list
    ; lb_loc: Location.t }

  val of_value_binding :
    Cmts.t -> Source.t -> ctx:Ast.t -> first:bool -> value_binding -> t

  val of_value_bindings :
    Cmts.t -> Source.t -> ctx:Ast.t -> value_binding list -> t list

  val of_binding_ops :
    Cmts.t -> Source.t -> ctx:Ast.t -> binding_op list -> t list
end
