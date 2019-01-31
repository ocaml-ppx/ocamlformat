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

module Cases : sig
  type t =
    { leading_space: Fmt.t
    ; bar: Fmt.t
    ; box_all: Fmt.t -> Fmt.t
    ; box_pattern_arrow: Fmt.t -> Fmt.t
    ; box_pattern_guard: Fmt.t -> Fmt.t
    ; break_before_arrow: Fmt.t
    ; break_after_arrow: Fmt.t
    ; break_after_opening_paren: Fmt.t
    ; box_rhs: Fmt.t -> Fmt.t }

  val get : Conf.t -> first:bool -> indent:int -> parens_here:bool -> t
end
