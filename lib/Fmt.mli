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

(** Formatting combinators *)

(** Format thunks. *)
type t

type sp =
  | Blank  (** [ ] *)
  | Cut  (** [@,] *)
  | Space  (** [@ ] *)
  | Break of int * int  (** [@;] *)

val sp : sp -> t

val ( $ ) : t -> t -> t
(** Format concatenation: [a $ b] formats [a], then [b]. *)

val sequence : t list -> t
(** Format concatenation of n elements. *)

val ( >$ ) : t -> ('b -> t) -> 'b -> t
(** Pre-compose a format thunk onto a function returning a format thunk. *)

val lazy_ : (unit -> t) -> t
(** Defer the evaluation of some side effects until formatting happens. *)

val set_margin : int -> t
(** Set the margin. *)

val set_max_indent : int option -> t
(** Set the maximum indentation. *)

val eval : Format_.formatter -> t -> unit
(** [eval fs t] runs format thunk [t] outputting to [fs] *)

val protect : t -> on_error:(exn -> unit) -> t
(** Catch exceptions raised while formatting. *)

val str_length : string -> int
(** Length that will be accounted when the given string is outputted. *)

(** Break hints and format strings --------------------------------------*)

val break : int -> int -> t
(** Format a break hint. *)

val force_break : t
(** [force_break] forces a break with indentation 0. Equivalent to [break 1000 0].*)

val space_break : t
(** [space_break] is either a single space or a newline.
    See {!Stdlib.Format.print_space_break}.
    Equivalement to format syntax ["@ "].*)

val cut_break : t
(** [cut_break] is either a newline or a {!noop}.
    See {!Stdlib.Format.print_cut}.
    Equivalement to format syntax ["@,"].*)

val force_newline : t
(** [force_newline] force a new line in the current pretty-printing box.
    See {!Stdlib.Format.force_newline}.
    Equivalement to format syntax ["@\n"].*)

val cbreak : fits:string * int * string -> breaks:string * int * string -> t
(** Format a custom break.

    - [fits = (a, b, c)] formats a string [a], [b] spaces and a string [c] if
      the line does not break.
    - [breaks = (d, e, f)] formats a string [d], [e] spaces and a string [f]
      if the line breaks. *)

val noop : t
(** Format nothing. *)

(** Primitive types -----------------------------------------------------*)

val char : char -> t
(** Format a char. *)

val str : string -> t
(** Format a string. *)

val str_as : int -> string -> t
(** [str_as a len] formats a string as if it were of length [len]. *)

(** Primitive containers ------------------------------------------------*)

val opt : 'a option -> ('a -> t) -> t
(** Format an option using provided formatter for the element. *)

val list_fl : 'a list -> (first:bool -> last:bool -> 'a -> t) -> t
(** Format a list using provided function for the elements, which is passed
    the flags indicating if the element is the first or last. *)

val list_pn : 'a list -> (prev:'a option -> 'a -> next:'a option -> t) -> t
(** Format a list using provided function for the elements, which is passed
    the previous and next elements, if any. *)

val list : 'a list -> t -> ('a -> t) -> t
(** Format a list using the format thunk for the separators between elements. *)

(** Conditional formatting ----------------------------------------------*)

val fmt_if : bool -> t -> t
(** Conditionally format thunk. *)

val fmt_or : bool -> t -> t -> t
(** Conditionally select between two format thunks. *)

val fmt_opt : t option -> t
(** Optionally format. [fmt_opt (Some t)] is [t] and [fmt_opt None] is
    [noop]. *)

(** Conditional on immediately following a line break -------------------*)

val if_newline : string -> t
(** Format a string if the line has just been broken. *)

val break_unless_newline : int -> int -> t
(** Format a break unless the line has just been broken. *)

(** Conditional on breaking of enclosing box ----------------------------*)

type behavior = Fit | Break

val fits_breaks :
  ?force:behavior -> ?hint:int * int -> ?level:int -> string -> string -> t
(** [fits_breaks fits nspaces offset breaks] prints [fits] if the enclosing
    box fits on one line, and otherwise prints [breaks], which is a string
    that optionally follows a break [hint] (that is a pair
    [(nspaces, offset)] equivalent to the break hint ["@;<nspaces offset>"]). *)

val fits_breaks_if :
     ?force:behavior
  -> ?hint:int * int
  -> ?level:int
  -> bool
  -> string
  -> string
  -> t
(** As [fits_breaks], but conditional. *)

(** Wrapping ------------------------------------------------------------*)

val wrap : t -> t -> t -> t
(** As [wrap], but prologue and epilogue may be arbitrary format thunks. *)

val wrap_if : bool -> t -> t -> t -> t
(** As [wrap_if], but prologue and epilogue may be arbitrary format thunks. *)

val wrap_if_fits_or : bool -> string -> string -> t -> t
(** As [wrap_if_fits], but prologue and epilogue can be forced by the
    additional condition. *)

val wrap_fits_breaks : ?space:bool -> Conf.t -> string -> string -> t -> t
(** As [wrap], but if [space] is provided, a space is added after prologue
    and a space hint is added before epilogue in case the enclosing box
    breaks. *)

val wrap_fits_breaks_if :
  ?space:bool -> Conf.t -> bool -> string -> string -> t -> t
(** As [wrap_fits_breaks], but prologue and epilogue are formatted subject to
    the additional condition. *)

(** Boxes ---------------------------------------------------------------*)

val with_box_debug : t -> t
(** Represent boxes inside a format thunk with colored brackets. For debug
    purposes *)

val open_vbox : ?name:string -> int -> t
(** Open an vbox with specified indentation. *)

val open_hvbox : ?name:string -> int -> t
(** Open an hvbox with specified indentation. *)

val open_hovbox : ?name:string -> int -> t
(** Open an hovbox with specified indentation. *)

val close_box : t
(** Close an arbitrary box. *)

(** Wrapping boxes ------------------------------------------------------*)

val cbox : ?name:string -> int -> t -> t
(** Wrap a format thunk with a compacting box with specified indentation. *)

val vbox : ?name:string -> int -> t -> t
(** Wrap a format thunk with a vbox with specified indentation. *)

val hvbox : ?name:string -> int -> t -> t
(** Wrap a format thunk with an hvbox with specified indentation. *)

val hovbox : ?name:string -> int -> t -> t
(** Wrap a format thunk with an hovbox with specified indentation. *)

val cbox_if : ?name:string -> bool -> int -> t -> t
(** Conditionally wrap a format thunk with a compacting sbox with specified
    indentation. *)

val vbox_if : ?name:string -> bool -> int -> t -> t
(** Conditionally wrap a format thunk with a vbox with specified indentation. *)

val hvbox_if : ?name:string -> bool -> int -> t -> t
(** Conditionally wrap a format thunk with an hvbox with specified
    indentation. *)

val hovbox_if : ?name:string -> bool -> int -> t -> t
(** Conditionally wrap a format thunk with an hovbox with specified
    indentation. *)
