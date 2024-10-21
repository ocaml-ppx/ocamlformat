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

(** Abstract syntax tree terms *)

open Migrate_ast
open Extended_ast

val init : Conf.t -> unit
(** Initialize internal state *)

module Attr : sig
  module Key : sig
    type t =
      | Regular  (** [@attr] *)
      | Item  (** [@@attr] *)
      | Floating  (** [@@@attr] *)

    val to_string : t -> string
  end

  val is_doc : attribute -> bool
  (** Holds for docstrings, that are attributes of the form [(** ... *)]. *)
end

module Ext : sig
  module Key : sig
    type t = Regular  (** [%ext] *) | Item  (** [%%ext] *)

    val to_string : t -> string
  end
end

module Token : sig
  val is_infix : Parser.token -> bool
  (** Holds for infix symbols. *)
end

module Exp : sig
  val location : expression -> Location.t

  val is_prefix : expression -> bool
  (** Holds for prefix symbol expressions. *)

  val is_symbol : expression -> bool
  (** Holds for prefix or infix expressions. *)

  val is_monadic_binding : expression -> bool
  (** [is_monadic_binding id] returns whether [id] is a monadic binding
      operator of the form [let**] or [and**] where [**] can be 1 or more
      operator chars. *)

  val exposed_left : expression -> bool
  (** [exposed_left exp] holds if the left-most subexpression of [exp] is a
      prefix operators. *)
end

val doc_atrs :
     ?acc:(string Location.loc * bool) list
  -> attributes
  -> (string Location.loc * bool) list option * attributes

type cmt_checker =
  { cmts_before: Location.t -> bool
  ; cmts_within: Location.t -> bool
  ; cmts_after: Location.t -> bool }

module Pat : sig
  val location : pattern -> Location.t

  val is_simple : pattern -> bool
end

module Mod : sig
  val is_simple : module_expr -> bool
end

module Mty : sig
  val is_simple : module_type -> bool
end

module Cl : sig
  val is_simple : class_expr -> bool
end

module Cty : sig
  val is_simple : class_type -> bool
end

module Tyd : sig
  val is_simple : type_declaration -> bool
end

type toplevel_item =
  [`Item of structure_item | `Directive of toplevel_directive]

(** Ast terms of various forms. *)
type t =
  | Pld of payload
  | Typ of core_type
  | Td of type_declaration
  | Cty of class_type
  | Cd of class_declaration
  | Ctd of class_type_declaration
  | Pat of pattern
  | Exp of expression
  | Fpe of expr_function_param
  | Fpc of class_function_param
  | Vc of value_constraint
  | Lb of value_binding
  | Bo of binding_op
  | Mb of module_binding
  | Md of module_declaration
  | Cl of class_expr
  | Mty of module_type
  | Mod of module_expr
  | Sig of signature_item
  | Str of structure_item
  | Clf of class_field
  | Ctf of class_type_field
  | Tli of toplevel_item
  | Top
  | Rep  (** Repl phrase *)

val is_top : t -> bool

val break_between :
  Source.t -> cmt_checker -> t * Conf.t -> t * Conf.t -> bool

val attributes : t -> attributes

val location : t -> Location.t

val dump : Format.formatter -> t -> unit
(** Debug: Dump the representation of an Ast term. *)

(** Term-in-context [{ctx; ast}] records that [ast] is (considered to be) an
    immediate sub-term of [ctx]. *)
type 'a xt = private {ctx: t; ast: 'a}

val sub_typ : ctx:t -> core_type -> core_type xt
(** Construct a core_type-in-context. *)

val sub_td : ctx:t -> type_declaration -> type_declaration xt
(** Construct a type_declaration-in-context. *)

val sub_cty : ctx:t -> class_type -> class_type xt
(** Construct a class_type-in-context. *)

val sub_pat : ctx:t -> pattern -> pattern xt
(** Construct a pattern-in-context. *)

val sub_exp : ctx:t -> expression -> expression xt
(** Construct a expression-in-context. *)

val sub_cl : ctx:t -> class_expr -> class_expr xt
(** Construct a class_expr-in-context. *)

val sub_cf : ctx:t -> class_field -> class_field xt
(** Construct a class_field-in-context. *)

val sub_ctf : ctx:t -> class_type_field -> class_type_field xt
(** Construct a class_type_field-in-context. *)

val sub_mty : ctx:t -> module_type -> module_type xt
(** Construct a module_type-in-context. *)

val sub_mod : ctx:t -> module_expr -> module_expr xt
(** Construct a module_expr-in-context. *)

val sub_md : ctx:t -> module_declaration -> module_declaration xt
(** Construct a module_declaration-in-context. *)

val sub_mb : ctx:t -> module_binding -> module_binding xt
(** Construct a module_binding-in-context. *)

val sub_sig : ctx:t -> signature_item -> signature_item xt
(** Construct a signature_item-in-context. *)

val sub_str : ctx:t -> structure_item -> structure_item xt
(** Construct a structure_item-in-context. *)

val sub_fun_body : ctx:t -> function_body -> function_body xt

val is_simple : Conf.t -> (expression xt -> int) -> expression xt -> bool
(** Holds of "simple" expressions: constants and constructor and function
    applications of other simple expressions. *)

(** 'Classes' of expressions which are parenthesized differently. *)
type cls = Let_match | Match | Non_apply | Sequence | Then | ThenElse

val exposed_right_exp : cls -> expression -> bool
(** [exposed_right_exp cls exp] holds if there is a right-most subexpression
    of [exp] which is of class [cls] and is not parenthesized. *)

val prec_ast : t -> Prec.t option
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

module Ext_attrs : sig
  val has_attrs : ext_attrs -> bool
end
