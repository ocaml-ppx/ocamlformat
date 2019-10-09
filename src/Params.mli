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

module Format = Format_

type exp_wrap = Fmt.t -> Fmt.t

val get_exp_wrap :
     Conf.t
  -> ?disambiguate:bool
  -> ?fits_breaks:bool
  -> parens:bool
  -> exp_grouping:[`Parens | `Begin_end]
  -> exp_wrap

type cases =
  { leading_space: Fmt.t
  ; bar: Fmt.t
  ; box_all: Fmt.t -> Fmt.t
  ; box_pattern_arrow: Fmt.t -> Fmt.t
  ; break_before_arrow: Fmt.t
  ; break_after_arrow: Fmt.t
  ; break_after_opening_paren: Fmt.t }

val get_cases :
  Conf.t -> first:bool -> indent:int -> parens_here:bool -> cases

val wrap_tuple :
  Conf.t -> parens:bool -> no_parens_if_break:bool -> Fmt.t -> Fmt.t

type record_type =
  { docked_before: Fmt.t
  ; break_before: Fmt.t
  ; box_record: Fmt.t -> Fmt.t
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
  ; expr_pro: Fmt.t option
  ; expr_eol: Fmt.t option
  ; break_end_branch: Fmt.t
  ; space_between_branches: Fmt.t }

val get_if_then_else :
     Conf.t
  -> first:bool
  -> last:bool
  -> parens:bool
  -> parens_bch:bool
  -> parens_prev_bch:bool
  -> xcond:Migrate_ast.Parsetree.expression Ast.xt option
  -> expr_loc:Warnings.loc
  -> fmt_extension_suffix:Fmt.t
  -> fmt_attributes:Fmt.t
  -> fmt_cond:(Migrate_ast.Parsetree.expression Ast.xt -> Fmt.t)
  -> exp_grouping:[`Parens | `Begin_end]
  -> exp_grouping_bch:[`Parens | `Begin_end]
  -> if_then_else

val match_indent : ?default:int -> Conf.t -> ctx:Ast.t -> int
(** [match_indent c ~ctx ~default] returns the indentation used for the
    pattern-matching in context [ctx], depending on the `match-indent-nested`
    option, or using the [default] indentation (0 if not provided) if the
    option does not apply. *)

val function_indent : ?default:int -> Conf.t -> ctx:Ast.t -> int
(** [function_indent c ~ctx ~default] returns the indentation used for the
    function in context [ctx], depending on the `function-indent-nested`
    option, or using the [default] indentation (0 if not provided) if the
    option does not apply. *)
