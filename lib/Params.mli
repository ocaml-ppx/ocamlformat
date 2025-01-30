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

open Extended_ast
open Asttypes

val parens_if : bool -> Conf.t -> ?disambiguate:bool -> Fmt.t -> Fmt.t

val parens : Conf.t -> ?disambiguate:bool -> Fmt.t -> Fmt.t

module Exp : sig
  module Infix_op_arg : sig
    val wrap : Conf.t -> ?parens_nested:bool -> parens:bool -> Fmt.t -> Fmt.t

    val dock : Conf.t -> expression Ast.xt -> bool
    (** Whether the RHS of an infix operator should be docked. *)
  end

  val wrap :
       Conf.t
    -> ?disambiguate:bool
    -> ?fits_breaks:bool
    -> ?offset_closing_paren:int
         (** Offset of the closing paren in case the line has been broken and
             the option [indicate-multiline-delimiters] is set to
             [closing-on-separate-line]. By default the offset is 0. *)
    -> parens:bool
    -> Fmt.t
    -> Fmt.t

  val box_fun_decl_args :
       Conf.t
    -> parens:bool
    -> kw:Fmt.t
    -> args:Fmt.t
    -> annot:Fmt.t option
    -> Fmt.t
  (** Box and assemble the parts [kw] (up to the arguments), [args] and
      [annot]. *)
end

module Mod : sig
  type args =
    { dock: bool  (** Whether each argument's [pro] should be docked. *)
    ; arg_psp: Fmt.t  (** Break before every arguments. *)
    ; indent: int
    ; align: bool
          (** Whether to align argument types inside their parenthesis. *) }

  val get_args : Conf.t -> functor_parameter loc list -> args

  val break_constraint : Conf.t -> rhs:module_type -> Fmt.t
  (** The break after [:] in a [Pmod_constraint]. *)
end

module Pcty : sig
  val arrow : Conf.t -> rhs:class_type -> Fmt.t

  val break_let_open : Conf.t -> rhs:class_type -> Fmt.t
end

val get_or_pattern_sep :
  ?cmts_before:bool -> ?space:bool -> Conf.t -> ctx:Ast.t -> Fmt.t

type cases =
  { leading_space: Fmt.t
  ; bar: Fmt.t
  ; box_all: Fmt.t -> Fmt.t
  ; box_pattern_arrow: Fmt.t -> Fmt.t
  ; break_before_arrow: Fmt.t
  ; break_after_arrow: Fmt.t
  ; open_paren_branch: Fmt.t
  ; break_after_opening_paren: Fmt.t
  ; expr_parens: bool option
  ; branch_expr: expression Ast.xt  (** Expression on the RHS of the [->]. *)
  ; close_paren_branch: Fmt.t }

val get_cases :
     Conf.t
  -> ctx:Ast.t
  -> first:bool
  -> last:bool
  -> xbch:expression Ast.xt
  -> cases

val wrap_tuple :
     Conf.t
  -> unboxed:bool
  -> parens:bool
  -> no_parens_if_break:bool
  -> Fmt.t
  -> Fmt.t

type record_type =
  { docked_before: Fmt.t
  ; break_before: Fmt.t
  ; box_record: Fmt.t -> Fmt.t
  ; box_spaced: bool (* True iff [box_record] adds inner spaces *)
  ; sep_before: Fmt.t
  ; sep_after: Fmt.t
  ; break_after: Fmt.t
  ; docked_after: Fmt.t }

val get_record_type : Conf.t -> unboxed:bool -> record_type

type elements_collection =
  { box: Fmt.t -> Fmt.t
  ; sep_before: Fmt.t
  ; sep_after_non_final: Fmt.t
  ; sep_after_final: Fmt.t }

type elements_collection_record_expr = {break_after_with: Fmt.t}

type elements_collection_record_pat = {wildcard: Fmt.t}

val get_record_expr :
     Conf.t
  -> unboxed:bool
  -> elements_collection * elements_collection_record_expr

val get_list_expr : Conf.t -> elements_collection

val get_array_expr : Conf.t -> elements_collection

val get_iarray_expr : Conf.t -> elements_collection

val wrap_comprehension :
  Conf.t -> space_around:bool -> punctuation:string -> Fmt.t -> Fmt.t

val get_record_pat :
     Conf.t
  -> ctx:Ast.t
  -> unboxed:bool
  -> elements_collection * elements_collection_record_pat

val get_list_pat : Conf.t -> ctx:Ast.t -> elements_collection

val get_array_pat : Conf.t -> ctx:Ast.t -> elements_collection

val get_iarray_pat : Conf.t -> ctx:Ast.t -> elements_collection

type if_then_else =
  { box_branch: Fmt.t -> Fmt.t
  ; cond: Fmt.t
  ; box_keyword_and_expr: Fmt.t -> Fmt.t
  ; branch_pro: Fmt.t
  ; wrap_parens: Fmt.t -> Fmt.t
  ; box_expr: bool option
  ; expr_pro: Fmt.t option
  ; expr_eol: Fmt.t option
  ; branch_expr: expression Ast.xt
  ; break_end_branch: Fmt.t
  ; space_between_branches: Fmt.t }

val get_if_then_else :
     Conf.t
  -> first:bool
  -> last:bool
  -> parens_bch:bool
  -> parens_prev_bch:bool
  -> xcond:expression Ast.xt option
  -> xbch:expression Ast.xt
  -> expr_loc:Location.t
  -> fmt_extension_suffix:Fmt.t option
  -> fmt_attributes:Fmt.t
  -> fmt_cond:(expression Ast.xt -> Fmt.t)
  -> if_then_else

val match_indent : ?default:int -> Conf.t -> parens:bool -> ctx:Ast.t -> int
(** [match_indent c ~ctx ~default] returns the indentation used for the
    pattern-matching in context [ctx], depending on the `match-indent-nested`
    option, or using the [default] indentation (0 if not provided) if the
    option does not apply. *)

val comma_sep : Conf.t -> Fmt.s
(** [comma_sep c] returns the format string used to separate two elements
    with a comma, depending on the `break-separators` option. *)

module Align : sig
  (** Implement the [align_symbol_open_paren] option. *)

  val infix_op : Conf.t -> Fmt.t -> Fmt.t

  val match_ : Conf.t -> xexp:expression Ast.xt -> Fmt.t -> Fmt.t

  val function_ :
    Conf.t -> parens:bool -> ctx0:Ast.t -> self:expression -> Fmt.t -> Fmt.t
end

module Indent : sig
  (** Indentation of various nodes. *)

  (** Expressions *)

  val function_ :
    ?default:int -> Conf.t -> parens:bool -> expression Ast.xt -> int
  (** Check the [function-indent-nested] option, or return [default] (0 if
      not provided) if the option does not apply. *)

  val fun_ : ?eol:Fmt.t -> Conf.t -> int
  (** Handle [function-indent-nested]. *)

  val fun_args : Conf.t -> int

  val fun_type_annot : Conf.t -> int

  val docked_fun :
    Conf.t -> source:Source.t -> loc:Location.t -> lbl:arg_label -> int

  val docked_function : Conf.t -> parens:bool -> expression Ast.xt -> int

  val docked_function_after_fun :
    Conf.t -> parens:bool -> lbl:arg_label -> int

  val fun_args_group : Conf.t -> lbl:arg_label -> expression -> int

  val record_docstring : Conf.t -> int

  val constructor_docstring : Conf.t -> int

  val exp_constraint : Conf.t -> int

  val assignment_operator_bol : Conf.t -> int

  (** Module expressions *)

  val mod_constraint : Conf.t -> lhs:module_expr -> int

  val mod_unpack_annot : Conf.t -> int

  (** Module types *)

  val mty_with : Conf.t -> int

  (** Types *)

  val type_constr : Conf.t -> int
end
