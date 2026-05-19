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

(** Placing and formatting comments in a parsetree.

    This module provides an interface to the global mutable data structure
    that maintains the relationship between comments and Ast terms within a
    parsetree.

    Each comment is placed, by one of the [init] functions, either before or
    after a location appearing in the parsetree. The [relocate] function can
    be used to adjust this placement.

    When comments are formatted by one of the [fmt] functions, they are
    removed from the data structure. This is significant in cases where there
    are multiple Ast terms with the same location. *)

type t

val source : t -> Source.t
(** The [Source.t] used to place comments. *)

val copy : t -> t
(** Deep-copy the placement state so it can be consumed independently. *)

val init :
     debug:bool
  -> source:Source.t
  -> ast:'a
  -> comments:Cmt.t list
  -> traverse:(Ocamlformat_parser_extended.Ast_mapper.mapper -> 'a -> 'a)
  -> print_ast:(Format.formatter -> 'a -> unit)
  -> t
(** Associate each [comment] with a source location appearing in [ast]. Uses
    [traverse] to walk the AST. [print_ast] is only used in debug mode.
    Initializes the state used by the [fmt] functions. *)

val dedup_cmts :
     traverse:(Ocamlformat_parser_extended.Ast_mapper.mapper -> 'a -> 'a)
  -> 'a
  -> Cmt.t list
  -> Cmt.t list
(** Drop comments that are already represented as docstring attributes in
    the AST (so they don't get double-printed). *)

val all_comments : t -> Cmt.t list
(** All comments associated with this state — placed (before/within/after)
    plus any not-yet-formatted ones. *)

val relocate :
  t -> src:Location.t -> before:Location.t -> after:Location.t -> unit
(** [relocate src before after] moves (changes the association with
    locations) comments before [src] to [before] and comments after [src] to
    [after]. *)

val relocate_wrongfully_attached_cmts :
  t -> Source.t -> Ocamlformat_parser_extended.Parsetree.expression -> unit
(** [relocate_wrongfully_attached_cmts] relocates wrongfully attached
    comments, e.g. comments that should be attached to the whole
    pattern-matching expressions ([match-with] or [try-with] expressions) but
    are wrongfully attached to the matched expression. *)

val fmt_before :
     t
  -> Conf.t
  -> fmt_code:Fmt_odoc.fmt_code
  -> ?pro:Fmt.t
  -> ?epi:Fmt.t
  -> ?eol:Fmt.t
  -> ?adj:Fmt.t
  -> Location.t
  -> Fmt.t
(** [fmt_before loc] formats the comments associated with [loc] that appear
    before [loc]. *)

val fmt_after :
     t
  -> Conf.t
  -> fmt_code:Fmt_odoc.fmt_code
  -> ?pro:Fmt.t
  -> ?epi:Fmt.t
  -> ?filter:(Cmt.t -> bool)
  -> Location.t
  -> Fmt.t
(** [fmt_after loc] formats the comments associated with [loc] that appear
    after [loc]. *)

val fmt_within :
     t
  -> Conf.t
  -> fmt_code:Fmt_odoc.fmt_code
  -> ?pro:Fmt.t
  -> ?epi:Fmt.t
  -> Location.t
  -> Fmt.t
(** [fmt_within loc] formats the comments associated with [loc] that appear
    within [loc]. *)

module Toplevel : sig
  val fmt_before :
    t -> Conf.t -> fmt_code:Fmt_odoc.fmt_code -> Location.t -> Fmt.t
  (** [fmt_before loc] formats the comments associated with [loc] that appear
      before [loc]. *)

  val fmt_after :
    t -> Conf.t -> fmt_code:Fmt_odoc.fmt_code -> Location.t -> Fmt.t
  (** [fmt_after loc] formats the comments associated with [loc] that appear
      after [loc]. *)
end

val drop_inside : t -> Location.t -> unit

val drop_before : t -> Location.t -> t

val has_before : t -> Location.t -> bool
(** [has_before t loc] holds if [t] contains some comment before [loc]. *)

val has_within : t -> Location.t -> bool
(** [has_within t loc] holds if [t] contains some comment within [loc]. *)

val has_after : t -> Location.t -> bool
(** [has_after t loc] holds if [t] contains some comment after [loc]. *)

val remaining_comments : t -> Cmt.t list
(** Returns comments that have not been formatted yet. *)

val remaining_locs : t -> Location.t list

val remaining_before : t -> Location.t -> Cmt.t list
(** [remaining_before c loc] returns the comments before [loc] *)

type layout_cache_key =
  | Arg of Asttypes.arg_label * Parsetree.expression
  | Pattern of Parsetree.pattern
  | Expression of Parsetree.expression

val preserve : cache_key:layout_cache_key -> (unit -> Fmt.t) -> t -> string
(** [preserve f t] formats like [f ()] but returns a string and does not
    consume comments from [t]. *)
