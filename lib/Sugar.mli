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
    ; lb_args: expr_function_param list
    ; lb_typ: value_constraint option
    ; lb_body: function_body Ast.xt
    ; lb_pun: bool
    ; lb_attrs: ext_attrs
    ; lb_loc: Location.t }

  val of_let_binding : ctx:Ast.t -> first:bool -> value_binding -> t

  val of_let_bindings : ctx:Ast.t -> value_binding list -> t list

  val of_binding_ops : binding_op list -> t list
end
