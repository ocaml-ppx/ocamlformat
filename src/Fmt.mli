(**********************************************************************)
(*                                                                    *)
(*                            OCamlFormat                             *)
(*                                                                    *)
(*  Copyright (c) 2017-present, Facebook, Inc.  All rights reserved.  *)
(*                                                                    *)
(*  This source code is licensed under the MIT license found in the   *)
(*  LICENSE file in the root directory of this source tree.           *)
(*                                                                    *)
(**********************************************************************)

(** Formatting combinators *)

module Format = Format_

(** Format strings that accept no arguments. *)
type s = (unit, Format.formatter, unit) format

(** Format thunks, which accept a formatter buffer and write to it. *)
type t = Format.formatter -> unit

val ( >$ ) : t -> ('b -> t) -> 'b -> t
(** Pre-compose a format thunk onto a function returning a format thunk. *)

val set_margin : int -> t
(** Set the margin. *)

(** Break hints and format strings --------------------------------------*)

val break : int -> int -> t
(** Format a break hint. *)

val fmt : s -> t
(** Format a format string. *)

(** Primitive types -----------------------------------------------------*)

val char : char -> t
(** Format a char. *)

val str : string -> t
(** Format a string. *)

(** Primitive containers ------------------------------------------------*)

val opt : 'a option -> ('a -> t) -> t
(** Format an option using provided formatter for the element. *)

val list : 'a list -> s -> ('a -> t) -> t
(** Format a list separated by a format string using provided function for
    the elements. *)

val list_fl : 'a list -> (first:bool -> last:bool -> 'a -> t) -> t
(** Format a list using provided function for the elements, which is passed
    the flags indicating if the element is the first or last. *)

val list_pn : 'a list -> (?prev:'a -> 'a -> ?next:'a -> t) -> t
(** Format a list using provided function for the elements, which is passed
    the previous and next elements, if any. *)

val list_k : 'a list -> t -> ('a -> t) -> t
(** Format a list using the format thunk for the separators between
    elements. *)

(** Conditional formatting ----------------------------------------------*)

val fmt_if : bool -> s -> t
(** Conditionally format. *)

val fmt_if_k : bool -> t -> t
(** Conditionally format thunk. *)

val fmt_or : bool -> s -> s -> t
(** Conditionally select between two format strings. *)

val fmt_or_k : bool -> t -> t -> t
(** Conditionally select between two format thunks. *)

(** Conditional on immediately following a line break -------------------*)

val if_newline : string -> t
(** Format a string if the line has just been broken. *)

val break_unless_newline : int -> int -> t
(** Format a break unless the line has just been broken. *)

val or_newline : string -> string -> t
(** [or_newline fits breaks] prints [fits] if the line has not just been
    broken, and otherwise prints [breaks]. *)

(** Conditional on breaking of enclosing box ----------------------------*)

val fits_breaks :
  ?force_fit_if:bool -> ?force_break_if:bool -> string -> string -> t
(** [fits_breaks fits breaks] prints [fits] if the enclosing box fits on one
    line, and otherwise prints [breaks], which is a string that optionally
    starts with a break hint specification such as ["@ "], ["@,"], or
    ["@;<nspaces offset>"]. *)

val fits_breaks_if :
  ?force_fit_if:bool -> ?force_break_if:bool -> bool -> string -> string
  -> t
(** As [fits_breaks], but conditional. *)

(** Wrapping ------------------------------------------------------------*)

val wrap : s -> s -> t -> t
(** [wrap prologue epilogue body] formats [prologue] then [body] then
    [epilogue]. *)

val wrap_k : t -> t -> t -> t
(** As [wrap], but prologue and epilogue may be arbitrary format thunks. *)

val wrap_if : bool -> s -> s -> t -> t
(** As [wrap], but prologue and epilogue are only formatted conditionally. *)

val wrap_if_breaks : string -> string -> t -> t
(** As [wrap], but prologue and epilogue are only formatted if the enclosing
    box breaks. *)

val wrap_if_fits_and : bool -> string -> string -> t -> t
(** As [wrap_if_fits], but prologue and epilogue are formatted subject to
    the additional condition. *)

val wrap_fits_breaks : string -> string -> t -> t
(** As [wrap], but a space is added after prologue and a space hint is added
    before epilogue in case the enclosing box breaks. *)

val wrap_fits_breaks_if : bool -> string -> string -> t -> t
(** As [wrap_fits_breaks], but prologue and epilogue are formatted subject
    to the additional condition. *)

(** Boxes ---------------------------------------------------------------*)

val open_hvbox : int -> t
(** Open an hvbox with specified indentation. *)

val open_hovbox : int -> t
(** Open an hovbox with specified indentation. *)

val close_box : t
(** Close an arbitrary box. *)

(** Wrapping boxes ------------------------------------------------------*)

val cbox : int -> t -> t
(** Wrap a format thunk with a compacting box with specified indentation. *)

val vbox : int -> t -> t
(** Wrap a format thunk with a vbox with specified indentation. *)

val hvbox : int -> t -> t
(** Wrap a format thunk with an hvbox with specified indentation. *)

val hovbox : int -> t -> t
(** Wrap a format thunk with an hovbox with specified indentation. *)

val cbox_if : bool -> int -> t -> t
(** Conditionally wrap a format thunk with a compacting sbox with specified
    indentation. *)

val vbox_if : bool -> int -> t -> t
(** Conditionally wrap a format thunk with a vbox with specified
    indentation. *)

val hvbox_if : bool -> int -> t -> t
(** Conditionally wrap a format thunk with an hvbox with specified
    indentation. *)

val hovbox_if : bool -> int -> t -> t
(** Conditionally wrap a format thunk with an hovbox with specified
    indentation. *)
