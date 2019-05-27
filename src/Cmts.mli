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

(** Placing and formatting comments in a parsetree.

    This module provides an interface to the global mutable data structure
    that maintains the relationship between comments and Ast terms within a
    parsetree.

    Each comment is placed, by one of the [init] functions, either before or
    after a location appearing in the parsetree. The [relocate] function can
    be used to adjust this placement.

    When comments are formatted by one of the [fmt] functions, they are
    removed from the data structure. This is significant in cases where
    there are multiple Ast terms with the same location. *)

module Format = Format_
open Migrate_ast

type t

val init_impl :
  Source.t -> Parsetree.structure -> (string * Location.t) list -> t
(** [init_impl source structure comments] associates each comment in
    [comments] with a source location appearing in [structure]. It uses
    [Source] to help resolve ambiguities. Initializes the state used by the
    [fmt] functions. *)

val init_intf :
  Source.t -> Parsetree.signature -> (string * Location.t) list -> t
(** [init_inft source signature comments] associates each comment in
    [comments] with a source location appearing in [signature]. It uses
    [Source] to help resolve ambiguities. Initializes the state used by the
    [fmt] functions. *)

val init_use_file :
     Source.t
  -> Parsetree.toplevel_phrase list
  -> (string * Location.t) list
  -> t
(** [init_use_file source use_file comments] associates each comment in
    [comments] with a source location appearing in [use_file]. It uses
    [Source] to help resolve ambiguities. Initializes the state used by the
    [fmt] functions. *)

val relocate :
  t -> src:Location.t -> before:Location.t -> after:Location.t -> unit
(** [relocate src before after] moves (changes the association with
    locations) comments before [src] to [before] and comments after [src] to
    [after]. *)

val fmt_before :
     t
  -> Conf.t
  -> ?pro:Fmt.t
  -> ?epi:Fmt.t
  -> ?eol:Fmt.t
  -> ?adj:Fmt.t
  -> Location.t
  -> Fmt.t
(** [fmt_before loc] formats the comments associated with [loc] that appear
    before [loc]. *)

val fmt_after :
  t -> Conf.t -> ?pro:Fmt.t -> ?epi:Fmt.t -> Location.t -> Fmt.t
(** [fmt_after loc] formats the comments associated with [loc] that appear
    after [loc]. *)

val fmt_within :
  t -> Conf.t -> ?pro:Fmt.t -> ?epi:Fmt.t -> Location.t -> Fmt.t
(** [fmt_within loc] formats the comments associated with [loc] that appear
    within [loc]. *)

val fmt :
     t
  -> Conf.t
  -> ?pro:Fmt.t
  -> ?epi:Fmt.t
  -> ?eol:Fmt.t
  -> ?adj:Fmt.t
  -> Location.t
  -> Fmt.t
  -> Fmt.t
(** [fmt loc format_thunk] wraps [fmt_before] and [fmt_after] around
    [format_thunk]. *)

val fmt_list :
     t
  -> Conf.t
  -> ?pro:Fmt.t
  -> ?epi:Fmt.t
  -> ?eol:Fmt.t
  -> Location.t list
  -> Fmt.t
  -> Fmt.t
(** [fmt_list locs] formats as per [fmt] for each loc in [locs]. *)

val drop_inside : t -> Location.t -> unit

val has_before : t -> Location.t -> bool
(** [has_before t loc] holds if [t] contains some comment before [loc]. *)

val has_within : t -> Location.t -> bool
(** [has_within t loc] holds if [t] contains some comment within [loc]. *)

val has_after : t -> Location.t -> bool
(** [has_after t loc] holds if [t] contains some comment after [loc]. *)

val remaining_comments : t -> (Location.t * string * string * Sexp.t) list
(** Returns comments that have not been formatted yet. *)

val remaining_locs : t -> Location.t list

val diff :
     (string * Location.t) list
  -> (string * Location.t) list
  -> (string, string) Either.t Sequence.t
(** Difference between two lists of comments. *)

val preserve : ('a -> Fmt.t) -> 'a -> string
(** [preserve fmt_x x] formats like [fmt_x x] but returns a string and does
    not consume comments from the internal state. *)
