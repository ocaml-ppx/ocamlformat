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

module Exp : sig
  val infix :
       Cmts.t
    -> Prec.t option
    -> expression Ast.xt
    -> (string loc option * expression Ast.xt) list
  (** [infix cmts prec exp] returns the infix operator and the list of
      operands applied to this operator from expression [exp]. [prec] is the
      precedence of the infix operator. *)
end

val sequence :
  Cmts.t -> expression Ast.xt -> (label loc option * expression Ast.xt) list
(** [sequence cmts exp] returns the list of expressions (with the optional
    extension) from a sequence of expressions [exp]. *)

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
    ; lb_args: arg_kind list
    ; lb_typ:
        [ `Polynewtype of label loc list * core_type Ast.xt
        | `Coerce of core_type Ast.xt option * core_type Ast.xt
        | `Other of core_type Ast.xt
        | `None ]
    ; lb_exp: expression Ast.xt
    ; lb_pun: bool
    ; lb_attrs: attribute list
    ; lb_loc: Location.t }

  val of_let_binding :
    Cmts.t -> ctx:Ast.t -> first:bool -> value_binding -> t

  val of_let_bindings : Cmts.t -> ctx:Ast.t -> value_binding list -> t list

  val of_binding_ops : Cmts.t -> ctx:Ast.t -> binding_op list -> t list
end
