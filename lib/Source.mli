(**********************************************************************
 *                                                                    *
 *                            OCamlFormat                             *
 *                                                                    *
 *  Copyright (c) 2017-present, Facebook, Inc.  All rights reserved.  *
 *                                                                    *
 *  This source code is licensed under the MIT license found in the   *
 *  LICENSE file in the root directory of this source tree.           *
 *                                                                    *
 **********************************************************************)

open Migrate_ast

type t

val create : string -> t

val empty_line_between : t -> Lexing.position -> Lexing.position -> bool
(** [empty_line_between t p1 p2] is [true] if there is an empty line between
    [p1] and [p2]. The lines containing [p1] and [p2] are not considered
    empty. *)

val string_between : t -> Lexing.position -> Lexing.position -> string option

val has_cmt_same_line_after : t -> Location.t -> bool

val string_at : t -> Lexing.position -> Lexing.position -> string

val string_literal :
  t -> [`Normalize | `Preserve] -> Location.t -> string option

val char_literal : t -> Location.t -> string option

val tokens_at :
     t
  -> ?filter:(Parser.token -> bool)
  -> Location.t
  -> (Parser.token * Location.t) list

val position_before : t -> Lexing.position -> Lexing.position option
(** [position_before s pos] returns the starting position of the token
    preceding the position [pos]. *)

val tokens_between :
     t
  -> ?filter:(Parser.token -> bool)
  -> Lexing.position
  -> Lexing.position
  -> (Parser.token * Location.t) list
(** [tokens_between s ~filter from upto] returns the list of tokens starting
    from [from] and ending before [upto] and respecting the [filter]
    property. [from] must start before [upto]. *)

val is_long_pexp_open : t -> Parsetree.expression -> bool
(** [is_long_pexp_open source exp] holds if [exp] is a [Pexp_open] expression
    that is expressed in long ('let open') form in source. *)

val is_long_pmod_functor : t -> Parsetree.module_expr -> bool
(** [is_long_pmod_functor source mod_exp] holds if [mod_exp] is a
    [Pmod_functor] expression that is expressed in long ('functor (M) ->')
    form in source. *)

val begins_line : ?ignore_spaces:bool -> t -> Location.t -> bool

val ends_line : t -> Location.t -> bool

val extension_using_sugar :
  name:string Location.loc -> payload:Parsetree.expression -> bool

val extend_loc_to_include_attributes :
  t -> Location.t -> Parsetree.attributes -> Location.t

val typed_expression :
  Parsetree.core_type -> Parsetree.expression -> [`Type_first | `Expr_first]

val typed_pattern :
  Parsetree.core_type -> Parsetree.pattern -> [`Type_first | `Pat_first]

val loc_of_underscore :
  t -> ('a * Parsetree.pattern) list -> Location.t -> Location.t option
(** [loc_of_underscore source fields loc] returns the location of the
    underscore at the end of the record pattern of location [loc] with fields
    [fields], if the record pattern is open (it ends with an underscore),
    otherwise returns [None]. *)
