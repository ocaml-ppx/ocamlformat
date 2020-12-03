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

type t

val create : text:string -> tokens:(Parser.token * Location.t) list -> t

val empty_line_between : t -> Lexing.position -> Lexing.position -> bool
(** [empty_line_between t p1 p2] is [true] if there is an empty line between
    [p1] and [p2]. The lines containing [p1] and [p2] are not considered
    empty. *)

val string_between : t -> Lexing.position -> Lexing.position -> string option

val tokens_between :
     t
  -> filter:(Parser.token -> bool)
  -> Lexing.position
  -> Lexing.position
  -> (Parser.token * Location.t) list

val string_at : t -> Location.t -> string

val has_cmt_same_line_after : t -> Location.t -> bool

val find_token_after :
     t
  -> filter:(Parser.token -> bool)
  -> Lexing.position
  -> (Parser.token * Location.t) option

val find_token_before :
     t
  -> filter:(Parser.token -> bool)
  -> Lexing.position
  -> (Parser.token * Location.t) option

val string_literal : t -> [`Normalize | `Preserve] -> Location.t -> string

val char_literal : t -> Location.t -> string

val is_long_pexp_open : t -> Parsetree.expression -> bool
(** [is_long_pexp_open source exp] holds if [exp] is a [Pexp_open] expression
    that is expressed in long ('let open') form in source. *)

val is_long_pmod_functor : t -> Parsetree.module_expr -> bool
(** [is_long_pmod_functor source mod_exp] holds if [mod_exp] is a
    [Pmod_functor] expression that is expressed in long ('functor (M) ->')
    form in source. *)

val is_long_pmty_functor : t -> Parsetree.module_type -> bool
(** [is_long_pmty_functor source mod_type] holds if [mod_type] is a
    [Pmty_functor] type that is expressed in long ('functor (M) ->') form in
    source. *)

val begins_line : ?ignore_spaces:bool -> t -> Location.t -> bool

val ends_line : t -> Location.t -> bool

val extension_using_sugar :
  name:string Location.loc -> payload:Location.t -> bool

val extend_loc_to_include_attributes :
  Location.t -> Parsetree.attributes -> Location.t

val type_constraint_is_first : Parsetree.core_type -> Location.t -> bool

val loc_of_underscore :
  t -> ('a * Parsetree.pattern) list -> Location.t -> Location.t option
(** [loc_of_underscore source fields loc] returns the location of the
    underscore at the end of the record pattern of location [loc] with fields
    [fields], if the record pattern is open (it ends with an underscore),
    otherwise returns [None]. *)

val locs_of_interval : t -> Location.t -> Location.t * Location.t
(** Given the location of an interval pattern ['a'..'b'], return the
    locations of the constants that represent the bounds (['a'] and ['b']). *)

val loc_of_pat_constant : t -> Parsetree.pattern -> Location.t

val loc_of_expr_constant : t -> Parsetree.expression -> Location.t

val is_quoted_string : t -> Location.t -> bool

val loc_of_first_token_at :
  t -> Location.t -> Parser.token -> Location.t option

(** Whether the starting column of the comment is before, after or is the
    same as the starting column of the other item. *)
type column_diff = Before | Same | After

type attached = bool

type prev = Same_line | After of attached * column_diff

type next = Same_line | Before of attached * column_diff

type 'a relative_info =
  { position: 'a
  ; tokens_between: Parser.token list
        (** List of tokens between the comment and the other item. *) }

val relative_info_from_prev :
  t -> curr:Location.t -> prev:Location.t -> prev relative_info
(** [relative_info_from_prev src curr prev] returns the source information
    related to the item of location [curr] and another AST node of location
    [prev] positioned before. *)

val relative_info_from_next :
  t -> curr:Location.t -> next:Location.t -> next relative_info
(** [relative_info_from_next src curr next] returns the source information
    related to the item of location [curr] and another AST node of location
    [next] positioned after. *)

type cmt_placement = After_prev | Before_next

val cmt_placement :
     prev:prev relative_info
  -> prev_loc:Location.t
  -> next:next relative_info
  -> next_loc:Location.t
  -> t
  -> cmt_placement
(** Heuristic to choose between placing a comment after the previous location
    or before the next one. *)
