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
end

module Mod : sig
  type args =
    { dock: bool  (** Whether each argument's [pro] should be docked. *)
    ; arg_psp: Fmt.t  (** Break before every arguments. *)
    ; indent: int
    ; align: bool
          (** Whether to align argument types inside their parenthesis. *) }

  (** Can be called from [Pmty_functor], [Pmod_functor] or [Pstr_module]. *)
  val get_args : Conf.t -> ctx:Ast.t -> functor_parameter loc list -> args
end

module Mty : sig
  val dock_functor_rhs : Conf.t -> rhs:module_type -> bool
  (** Whether functor types should be docked on the same line or break after
      the [->]. *)

  val dock_module_sig : Conf.t -> args_are_docked:bool -> module_type -> bool
  (** Whether the signature of a module decl should be docked after the [:].
      [~args_are_docked] expects the [dock] field returned by
      {!Mod.get_args}. *)

  val dock_typeof : Conf.t -> rhs:module_expr -> bool
  (** Whether to dock the RHS of a [module type of]. *)

  val box_with : Conf.t -> box:bool -> lhs:module_type -> Fmt.t -> Fmt.t
  (** The box around a [Pmty_with]. *)
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
  ; close_paren_branch: Fmt.t }

val get_cases :
     Conf.t
  -> ctx:Ast.t
  -> first:bool
  -> last:bool
  -> xbch:expression Ast.xt
  -> cases

val wrap_tuple :
  Conf.t -> parens:bool -> no_parens_if_break:bool -> Fmt.t -> Fmt.t

type record_type =
  { docked_before: Fmt.t
  ; break_before: Fmt.t
  ; box_record: Fmt.t -> Fmt.t
  ; box_spaced: bool (* True iff [box_record] adds inner spaces *)
  ; sep_before: Fmt.t
  ; sep_after: Fmt.t
  ; break_after: Fmt.t
  ; docked_after: Fmt.t }

val get_record_type : Conf.t -> record_type

type elements_collection =
  { box: Fmt.t -> Fmt.t
  ; sep_before: Fmt.t
  ; sep_after_non_final: Fmt.t
  ; sep_after_final: Fmt.t }

type elements_collection_record_expr = {break_after_with: Fmt.t}

type elements_collection_record_pat = {wildcard: Fmt.t}

val get_record_expr :
  Conf.t -> elements_collection * elements_collection_record_expr

val get_list_expr : Conf.t -> elements_collection

val get_array_expr : Conf.t -> elements_collection

val get_record_pat :
  Conf.t -> ctx:Ast.t -> elements_collection * elements_collection_record_pat

val get_list_pat : Conf.t -> ctx:Ast.t -> elements_collection

val get_array_pat : Conf.t -> ctx:Ast.t -> elements_collection

type if_then_else =
  { box_branch: Fmt.t -> Fmt.t
  ; cond: Fmt.t
  ; box_keyword_and_expr: Fmt.t -> Fmt.t
  ; branch_pro: Fmt.t
  ; wrap_parens: Fmt.t -> Fmt.t
  ; box_expr: bool option
  ; expr_pro: Fmt.t option
  ; expr_eol: Fmt.t option
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

val function_indent : ?default:int -> Conf.t -> ctx:Ast.t -> int
(** [function_indent c ~ctx ~default] returns the indentation used for the
    function in context [ctx], depending on the `function-indent-nested`
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
