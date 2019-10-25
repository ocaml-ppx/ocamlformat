(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Pierre Weis, projet Cristal, INRIA Rocquencourt            *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Pretty-printing.

   This module implements a pretty-printing facility to format values
   within {{!boxes}'pretty-printing boxes'}
   combined with a set of {{!fpp}printf-like functions}.
   The pretty-printer splits lines at specified {{!breaks}break hints},
   and indents lines according to the box structure.

   This pretty-printing facility is implemented as an overlay on top of
   abstract {{!section:formatter}formatters} which provide basic output
   functions.
   Some formatters are predefined, notably:
   - {!std_formatter} outputs to {{!Stdlib.stdout}stdout}
   - {!err_formatter} outputs to {{!Stdlib.stderr}stderr}

   Most functions in the {!Format} module come in two variants:
   a short version that operates on {!std_formatter} and the
   generic version prefixed by [pp_] that takes a formatter
   as its first argument.

   More formatters can be created with {!formatter_of_out_channel},
   {!formatter_of_buffer}, {!formatter_of_symbolic_output_buffer}
   or using {{!section:formatter}custom formatters}.

*)

(** {1 Introduction}
   For a gentle introduction to the basics of pretty-printing using
   [Format], read
   {{:http://caml.inria.fr/resources/doc/guides/format.en.html}
    http://caml.inria.fr/resources/doc/guides/format.en.html}.

   You may consider this module as providing an extension to the
   [printf] facility to provide automatic line splitting. The addition of
   pretty-printing annotations to your regular [printf] format strings gives
   you fancy indentation and line breaks.
   Pretty-printing annotations are described below in the documentation of
   the function {!Format.fprintf}.

   You may also use the explicit pretty-printing box management and printing
   functions provided by this module. This style is more basic but more
   verbose than the concise [fprintf] format strings.

   For instance, the sequence
   [open_box 0; print_string "x ="; print_space ();
    print_int 1; close_box (); print_newline ()]
   that prints [x = 1] within a pretty-printing box, can be
   abbreviated as [printf "@[%s@ %i@]@." "x =" 1], or even shorter
   [printf "@[x =@ %i@]@." 1].

   Rule of thumb for casual users of this library:
 - use simple pretty-printing boxes (as obtained by [open_box 0]);
 - use simple break hints as obtained by [print_cut ()] that outputs a
   simple break hint, or by [print_space ()] that outputs a space
   indicating a break hint;
 - once a pretty-printing box is open, display its material with basic
   printing functions (e. g. [print_int] and [print_string]);
 - when the material for a pretty-printing box has been printed, call
   [close_box ()] to close the box;
 - at the end of pretty-printing, flush the pretty-printer to display all
   the remaining material, e.g. evaluate [print_newline ()].

   The behavior of pretty-printing commands is unspecified
   if there is no open pretty-printing box. Each box opened by
   one of the [open_] functions below must be closed using [close_box]
   for proper formatting. Otherwise, some of the material printed in the
   boxes may not be output, or may be formatted incorrectly.

   In case of interactive use, each phrase is executed in the initial state
   of the standard pretty-printer: after each phrase execution, the
   interactive system closes all open pretty-printing boxes, flushes all
   pending text, and resets the standard pretty-printer.

   Warning: mixing calls to pretty-printing functions of this module with
   calls to {!Stdlib} low level output functions is error prone.

   The pretty-printing functions output material that is delayed in the
   pretty-printer queue and stacks in order to compute proper line
   splitting. In contrast, basic I/O output functions write directly in
   their output device. As a consequence, the output of a basic I/O function
   may appear before the output of a pretty-printing function that has been
   called before. For instance,
   [
    Stdlib.print_string "<";
    Format.print_string "PRETTY";
    Stdlib.print_string ">";
    Format.print_string "TEXT";
   ]
   leads to output [<>PRETTYTEXT].

*)

open! Caml

type formatter
(** Abstract data corresponding to a pretty-printer (also called a
    formatter) and all its machinery. See also {!section:formatter}. *)

(** {1:boxes Pretty-printing boxes} *)

(** The pretty-printing engine uses the concepts of pretty-printing box and
  break hint to drive indentation and line splitting behavior of the
  pretty-printer.

  Each different pretty-printing box kind introduces a specific line splitting
  policy:

  - within an {e horizontal} box, break hints never split the line (but the
    line may be split in a box nested deeper),
  - within a {e vertical} box, break hints always split the line,
  - within an {e horizontal/vertical} box, if the box fits on the current line
    then break hints never split the line, otherwise break hint always split
    the line,
  - within a {e compacting} box, a break hint never splits the line,
    unless there is no more room on the current line.

  Note that line splitting policy is box specific: the policy of a box does
  not rule the policy of inner boxes. For instance, if a vertical box is
  nested in an horizontal box, all break hints within the vertical box will
  split the line.
*)

val pp_open_box : formatter -> int -> unit
(** [pp_open_box ppf d] opens a new compacting pretty-printing box with
    offset [d] in the formatter [ppf].

   Within this box, the pretty-printer prints as much as possible material on
   every line.

   A break hint splits the line if there is no more room on the line to
   print the remainder of the box.

   Within this box, the pretty-printer emphasizes the box structure: a break
   hint also splits the line if the splitting ``moves to the left''
   (i.e. the new line gets an indentation smaller than the one of the current
   line).

   This box is the general purpose pretty-printing box.

   If the pretty-printer splits the line in the box, offset [d] is added to
   the current indentation.
*)


val pp_close_box : formatter -> unit -> unit
(** Closes the most recently open pretty-printing box. *)

val pp_open_hbox : formatter -> unit -> unit
(** [pp_open_hbox ppf ()] opens a new 'horizontal' pretty-printing box.

  This box prints material on a single line.

  Break hints in a horizontal box never split the line.
  (Line splitting may still occur inside boxes nested deeper).
*)

val pp_open_vbox : formatter -> int -> unit
(** [pp_open_vbox ppf d] opens a new 'vertical' pretty-printing box
  with offset [d].

  This box prints material on as many lines as break hints in the box.

  Every break hint in a vertical box splits the line.

  If the pretty-printer splits the line in the box, [d] is added to the
  current indentation.
*)

val pp_open_hvbox : formatter -> int -> unit
(** [pp_open_hvbox ppf d] opens a new 'horizontal/vertical' pretty-printing box
  with offset [d].

  This box behaves as an horizontal box if it fits on a single line,
  otherwise it behaves as a vertical box.

  If the pretty-printer splits the line in the box, [d] is added to the
  current indentation.
*)

val pp_open_hovbox : formatter -> int -> unit
(** [pp_open_hovbox ppf d] opens a new 'horizontal-or-vertical'
  pretty-printing box with offset [d].

  This box prints material as much as possible on every line.

  A break hint splits the line if there is no more room on the line to
  print the remainder of the box.

  If the pretty-printer splits the line in the box, [d] is added to the
  current indentation.
*)

(** {1 Formatting functions} *)

val pp_print_string : formatter -> string -> unit
(** [pp_print_string ppf s] prints [s] in the current pretty-printing box. *)

val pp_print_as : formatter -> int -> string -> unit
(** [pp_print_as ppf len s] prints [s] in the current pretty-printing box.
  The pretty-printer formats [s] as if it were of length [len].
*)

val pp_print_int : formatter -> int -> unit
(** Print an integer in the current pretty-printing box. *)

val pp_print_float : formatter -> float -> unit
(** Print a floating point number in the current pretty-printing box. *)

val pp_print_char : formatter -> char -> unit
(** Print a character in the current pretty-printing box. *)

val pp_print_bool : formatter -> bool -> unit
(** Print a boolean in the current pretty-printing box. *)

(** {1:breaks Break hints} *)

(** A 'break hint' tells the pretty-printer to output some space or split the
  line whichever way is more appropriate to the current pretty-printing box
  splitting rules.

  Break hints are used to separate printing items and are mandatory to let
  the pretty-printer correctly split lines and indent items.

  Simple break hints are:
  - the 'space': output a space or split the line if appropriate,
  - the 'cut': split the line if appropriate.

  Note: the notions of space and line splitting are abstract for the
  pretty-printing engine, since those notions can be completely redefined
  by the programmer.
  However, in the pretty-printer default setting, ``output a space'' simply
  means printing a space character (ASCII code 32) and ``split the line''
  means printing a newline character (ASCII code 10).
*)

val pp_print_space : formatter -> unit -> unit
(** [pp_print_space ppf ()] emits a 'space' break hint:
  the pretty-printer may split the line at this point,
  otherwise it prints one space.

  [pp_print_space ppf ()] is equivalent to [pp_print_break ppf 1 0].
*)

val pp_print_cut : formatter -> unit -> unit
(** [pp_print_cut ppf ()] emits a 'cut' break hint:
  the pretty-printer may split the line at this point,
  otherwise it prints nothing.

  [pp_print_cut ppf ()] is equivalent to [pp_print_break ppf 0 0].
*)

val pp_print_break : formatter -> int -> int -> unit
(** [pp_print_break ppf nspaces offset] emits a 'full' break hint:
  the pretty-printer may split the line at this point,
  otherwise it prints [nspaces] spaces.

  If the pretty-printer splits the line, [offset] is added to
  the current indentation.
*)

val pp_print_custom_break :
  formatter ->
  fits:(string * int * string) ->
  breaks:(string * int * string) ->
  unit
(** [pp_print_custom_break ppf ~fits:(s1, n, s2) ~breaks:(s3, m, s4)] emits a
   custom break hint: the pretty-printer may split the line at this point.

   If it does not split the line, then the [s1] is emitted, then [n] spaces,
   then [s2].

   If it splits the line, then it emits the [s3] string, then an indent
   (according to the box rules), then an offset of [m] spaces, then the [s4]
   string.

   While [n] and [m] are handled by [formatter_out_functions.out_indent], the
   strings will be handled by [formatter_out_functions.out_string]. This allows
   for a custom formatter that handles indentation distinctly, for example,
   outputs [<br/>] tags or [&nbsp;] entities.

   The custom break is useful if you want to change which visible
   (non-whitespace) characters are printed in case of break or no break. For
   example, when printing a list {[ [a; b; c] ]}, you might want to add a
   trailing semicolon when it is printed vertically:

   {[
[
  a;
  b;
  c;
]
   ]}

   You can do this as follows:
   {[
printf "@[<v 0>[@;<0 2>@[<v 0>a;@,b;@,c@]%t]@]@\n"
  (pp_print_custom_break ~fits:("", 0, "") ~breaks:(";", 0, ""))
   ]}

  @since 4.08.0
*)

val pp_force_newline : formatter -> unit -> unit
(** Force a new line in the current pretty-printing box.

  The pretty-printer must split the line at this point,

  Not the normal way of pretty-printing, since imperative line splitting may
  interfere with current line counters and box size calculation.
  Using break hints within an enclosing vertical box is a better
  alternative.
*)

val pp_print_if_newline : formatter -> unit -> unit
(** Execute the next formatting command if the preceding line
  has just been split. Otherwise, ignore the next formatting
  command.
*)

val pp_print_string_if_newline : formatter -> string -> unit
(** Similar to [print_if_newline] followed by [print_string] except that the
  length of the string does not contribute to the width of the enclosing
  box. *)

val pp_print_or_newline : formatter -> int -> int -> string -> string -> unit
(** Print a full break hint and the first string if the preceding line has
  not just been split. Otherwise, print the second string. *)

val pp_print_fits_or_breaks :
  formatter -> ?level:int -> string -> int -> int -> string -> unit
(** [pp_print_fits_or_breaks fmt ?level fits nspaces offset breaks] prints
  [fits] if the enclosing boxes fits on one line ([level] being the depth of
  boxes that are checked in the stack). Otherwise, prints a break as per
  [print_break nspaces offset] followed by [breaks]. *)

(** {1 Pretty-printing termination} *)

val pp_print_flush : formatter -> unit -> unit
(** End of pretty-printing: resets the pretty-printer to initial state.

  All open pretty-printing boxes are closed, all pending text is printed.
  In addition, the pretty-printer low level output device is flushed to
  ensure that all pending text is really displayed.

  Note: never use [print_flush] in the normal course of a pretty-printing
  routine, since the pretty-printer uses a complex buffering machinery to
  properly indent the output; manually flushing those buffers at random
  would conflict with the pretty-printer strategy and result to poor
  rendering.

  Only consider using [print_flush] when displaying all pending material is
  mandatory (for instance in case of interactive use when you want the user
  to read some text) and when resetting the pretty-printer state will not
  disturb further pretty-printing.

  Warning: If the output device of the pretty-printer is an output channel,
  repeated calls to [print_flush] means repeated calls to {!Stdlib.flush}
  to flush the out channel; these explicit flush calls could foil the
  buffering strategy of output channels and could dramatically impact
  efficiency.
*)

val pp_print_newline : formatter -> unit -> unit
(** End of pretty-printing: resets the pretty-printer to initial state.

  All open pretty-printing boxes are closed, all pending text is printed.

  Equivalent to {!print_flush} followed by a new line.
  See corresponding words of caution for {!print_flush}.

  Note: this is not the normal way to output a new line;
  the preferred method is using break hints within a vertical pretty-printing
  box.
*)

(** {1 Margin} *)

val pp_set_margin : formatter -> int -> unit
(** [pp_set_margin ppf d] sets the right margin to [d] (in characters):
  the pretty-printer splits lines that overflow the right margin according to
  the break hints given.
  Nothing happens if [d] is smaller than 2.
  If [d] is too large, the right margin is set to the maximum
  admissible value (which is greater than [10 ^ 9]).
  If [d] is less than the current maximum indentation limit, the
  maximum indentation limit is decreased while trying to preserve
  a minimal ratio [max_indent/margin>=50%] and if possible
  the current difference [margin - max_indent].

  See also {!pp_set_geometry}.
*)

val pp_get_margin : formatter -> unit -> int
(** Returns the position of the right margin. *)

(** {1 Maximum indentation limit} *)

val pp_set_max_indent : formatter -> int -> unit
(** [pp_set_max_indent ppf d] sets the maximum indentation limit of lines
  to [d] (in characters):
  once this limit is reached, new pretty-printing boxes are rejected to the
  left, unless the enclosing box fully fits on the current line.
  As an illustration,
  {[ set_margin 10; set_max_indent 5; printf "@[123456@[7@]89A@]@." ]}
  yields
  {[
    123456
    789A
  ]}
  because the nested box ["@[7@]"] is opened after the maximum indentation
  limit ([7>5]) and its parent box does not fit on the current line.
  Either decreasing the length of the parent box to make it fit on a line:
  {[ printf "@[123456@[7@]89@]@." ]}
  or opening an intermediary box before the maximum indentation limit which
  fits on the current line
  {[ printf "@[123@[456@[7@]89@]A@]@." ]}
  avoids the rejection to the left of the inner boxes and print respectively
  ["123456789"] and ["123456789A"] .
  Note also that vertical boxes never fit on a line whereas horizontal boxes
  always fully fit on the current line.

  Nothing happens if [d] is smaller than 2.

  If [d] is too large, the limit is set to the maximum
  admissible value (which is greater than [10 ^ 9]).

  If [d] is greater or equal than the current margin, it is ignored,
  and the current maximum indentation limit is kept.

  See also {!pp_set_geometry}.
*)

val pp_get_max_indent : formatter -> unit -> int
(** Return the maximum indentation limit (in characters). *)

(** {1 Geometry }

Geometric functions can be used to manipulate simultaneously the
coupled variables, margin and maxixum indentation limit.

*)

type geometry = { max_indent:int; margin: int}

val check_geometry: geometry -> bool
(** Check if the formatter geometry is valid: [1 < max_indent < margin] *)

val pp_set_geometry : formatter -> max_indent:int -> margin:int -> unit
val pp_safe_set_geometry : formatter -> max_indent:int -> margin:int -> unit
(**
   [pp_set_geometry ppf ~max_indent ~margin] sets both the margin
   and maximum indentation limit for [ppf].

   When [1 < max_indent < margin],
   [pp_set_geometry ppf ~max_indent ~margin]
   is equivalent to
   [pp_set_margin ppf margin; pp_set_max_indent ppf max_indent];
   and avoids the subtly incorrect
   [pp_set_max_indent ppf max_indent; pp_set_margin ppf margin];

   Outside of this domain, [pp_set_geometry] raises an invalid argument
   exception whereas [pp_safe_set_geometry] does nothing.

   @since 4.08.0
*)

val pp_get_geometry: formatter -> unit -> geometry
(** Return the current geometry of the formatter

    @since 4.08.0
*)



(** {1 Maximum formatting depth} *)

(** The maximum formatting depth is the maximum number of pretty-printing
  boxes simultaneously open.

  Material inside boxes nested deeper is printed as an ellipsis (more
  precisely as the text returned by {!get_ellipsis_text} [()]).
*)

val pp_set_max_boxes : formatter -> int -> unit
(** [pp_set_max_boxes ppf max] sets the maximum number of pretty-printing
    boxes simultaneously open.

  Material inside boxes nested deeper is printed as an ellipsis (more
  precisely as the text returned by {!get_ellipsis_text} [()]).

  Nothing happens if [max] is smaller than 2.
*)

val pp_get_max_boxes : formatter -> unit -> int
(** Returns the maximum number of pretty-printing boxes allowed before
  ellipsis.
*)

val pp_over_max_boxes : formatter -> unit -> bool
(** Tests if the maximum number of pretty-printing boxes allowed have already
  been opened.
*)

(** {1 Redirecting the standard formatter output} *)
val pp_set_formatter_out_channel :
  formatter -> Stdlib.out_channel -> unit
(** Redirect the standard pretty-printer output to the given channel.
  (All the output functions of the standard formatter are set to the
   default output functions printing to the given channel.)
*)

val pp_set_formatter_output_functions :
  formatter -> (string -> int -> int -> unit) -> (unit -> unit) -> unit
(** [pp_set_formatter_output_functions ppf out flush] redirects the
  standard pretty-printer output functions to the functions [out] and
  [flush].

  The [out] function performs all the pretty-printer string output.
  It is called with a string [s], a start position [p], and a number of
  characters [n]; it is supposed to output characters [p] to [p + n - 1] of
  [s].

  The [flush] function is called whenever the pretty-printer is flushed
  (via conversion [%!], or pretty-printing indications [@?] or [@.], or
  using low level functions [print_flush] or [print_newline]).
*)

val pp_get_formatter_output_functions :
  formatter -> unit -> (string -> int -> int -> unit) * (unit -> unit)
(** Return the current output functions of the standard pretty-printer. *)

(** {1:meaning Redefining formatter output} *)

(** The [Format] module is versatile enough to let you completely redefine
  the meaning of pretty-printing output: you may provide your own functions
  to define how to handle indentation, line splitting, and even printing of
  all the characters that have to be printed!
*)

(** {2 Redefining output functions} *)

type formatter_out_functions = {
  out_string : string -> int -> int -> unit;
  out_flush : unit -> unit;
  out_newline : unit -> unit;
  out_spaces : int -> unit;
  out_indent : int -> unit;
}
(** The set of output functions specific to a formatter:
- the [out_string] function performs all the pretty-printer string output.
  It is called with a string [s], a start position [p], and a number of
  characters [n]; it is supposed to output characters [p] to [p + n - 1] of
  [s].
- the [out_flush] function flushes the pretty-printer output device.
- [out_newline] is called to open a new line when the pretty-printer splits
  the line.
- the [out_spaces] function outputs spaces when a break hint leads to spaces
  instead of a line split. It is called with the number of spaces to output.
- the [out_indent] function performs new line indentation when the
  pretty-printer splits the line. It is called with the indentation value of
  the new line.

  By default:
- fields [out_string] and [out_flush] are output device specific;
  (e.g. {!Stdlib.output_string} and {!Stdlib.flush} for a
   {!Stdlib.out_channel} device, or [Buffer.add_substring] and
   {!Stdlib.ignore} for a [Buffer.t] output device),
- field [out_newline] is equivalent to [out_string "\n" 0 1];
- fields [out_spaces] and [out_indent] are equivalent to
  [out_string (String.make n ' ') 0 n].
  @since 4.01.0
*)

val pp_set_formatter_out_functions :
  formatter -> formatter_out_functions -> unit
(** [pp_set_formatter_out_functions ppf out_funs]
  Set all the pretty-printer output functions of [ppf] to those of
  argument [out_funs],

  This way, you can change the meaning of indentation (which can be
  something else than just printing space characters) and the meaning of new
  lines opening (which can be connected to any other action needed by the
  application at hand).

  Reasonable defaults for functions [out_spaces] and [out_newline] are
  respectively [out_funs.out_string (String.make n ' ') 0 n] and
  [out_funs.out_string "\n" 0 1].
  @since 4.01.0
*)

val pp_get_formatter_out_functions :
  formatter -> unit -> formatter_out_functions
(** Return the current output functions of the pretty-printer,
  including line splitting and indentation functions. Useful to record the
  current setting and restore it afterwards.
  @since 4.01.0
*)

(** {1:formatter Defining formatters}

  Defining new formatters permits unrelated output of material in
  parallel on several output devices.
  All the parameters of a formatter are local to the formatter:
  right margin, maximum indentation limit, maximum number of pretty-printing
  boxes simultaneously open, ellipsis, and so on, are specific to
  each formatter and may be fixed independently.

  For instance, given a {!Buffer.t} buffer [b], {!formatter_of_buffer} [b]
  returns a new formatter using buffer [b] as its output device.
  Similarly, given a {!Stdlib.out_channel} output channel [oc],
  {!formatter_of_out_channel} [oc] returns a new formatter using
  channel [oc] as its output device.

  Alternatively, given [out_funs], a complete set of output functions for a
  formatter, then {!formatter_of_out_functions} [out_funs] computes a new
  formatter using those functions for output.
*)

val formatter_of_out_channel : out_channel -> formatter
(** [formatter_of_out_channel oc] returns a new formatter writing
  to the corresponding output channel [oc].
*)

val std_formatter : formatter
(** The standard formatter to write to standard output.

  It is defined as {!formatter_of_out_channel} {!Stdlib.stdout}.
*)

val err_formatter : formatter
(** A formatter to write to standard error.

  It is defined as {!formatter_of_out_channel} {!Stdlib.stderr}.
*)

val formatter_of_buffer : Buffer.t -> formatter
(** [formatter_of_buffer b] returns a new formatter writing to
  buffer [b]. At the end of pretty-printing, the formatter must be flushed
  using {!pp_print_flush} or {!pp_print_newline}, to print all the
  pending material into the buffer.
*)

val stdbuf : Buffer.t
(** The string buffer in which [str_formatter] writes. *)

val str_formatter : formatter
(** A formatter to output to the {!stdbuf} string buffer.

  [str_formatter] is defined as {!formatter_of_buffer} {!stdbuf}.
*)

val flush_str_formatter : unit -> string
(** Returns the material printed with [str_formatter], flushes
  the formatter and resets the corresponding buffer.
*)

val make_formatter :
  (string -> int -> int -> unit) -> (unit -> unit) -> formatter
(** [make_formatter out flush] returns a new formatter that outputs with
  function [out], and flushes with function [flush].

  For instance, {[
    make_formatter
      (Stdlib.output oc)
      (fun () -> Stdlib.flush oc) ]}
  returns a formatter to the {!Stdlib.out_channel} [oc].
*)

val formatter_of_out_functions :
  formatter_out_functions -> formatter
(** [formatter_of_out_functions out_funs] returns a new formatter that writes
  with the set of output functions [out_funs].

  See definition of type {!formatter_out_functions} for the meaning of argument
  [out_funs].

  @since 4.06.0
*)

val pp_set_max_newline_offset : formatter -> int -> unit

(** {1 Convenience formatting functions.} *)

val pp_print_list:
  ?pp_sep:(formatter -> unit -> unit) ->
  (formatter -> 'a -> unit) -> (formatter -> 'a list -> unit)
(** [pp_print_list ?pp_sep pp_v ppf l] prints items of list [l],
  using [pp_v] to print each item, and calling [pp_sep]
  between items ([pp_sep] defaults to {!pp_print_cut}.
  Does nothing on empty lists.

  @since 4.02.0
*)

val pp_print_text : formatter -> string -> unit
(** [pp_print_text ppf s] prints [s] with spaces and newlines respectively
  printed using {!pp_print_space} and {!pp_force_newline}.

  @since 4.02.0
*)

val pp_print_option :
  ?none:(formatter -> unit -> unit) ->
  (formatter -> 'a -> unit) -> (formatter -> 'a option -> unit)
(** [pp_print_option ?none pp_v ppf o] prints [o] on [ppf]
    using [pp_v] if [o] is [Some v] and [none] if it is [None]. [none]
    prints nothing by default.

    @since 4.08 *)

val pp_print_result :
  ok:(formatter -> 'a -> unit) -> error:(formatter -> 'e -> unit) ->
  formatter -> ('a, 'e) result -> unit
(** [pp_print_result ~ok ~error ppf r] prints [r] on [ppf] using
    [ok] if [r] is [Ok _] and [error] if [r] is [Error _].

    @since 4.08 *)

(** {1:fpp Formatted pretty-printing} *)

(**
  Module [Format] provides a complete set of [printf] like functions for
  pretty-printing using format string specifications.

  Specific annotations may be added in the format strings to give
  pretty-printing commands to the pretty-printing engine.

  Those annotations are introduced in the format strings using the [@]
  character. For instance, [@ ] means a space break, [@,] means a cut,
  [@\[] opens a new box, and [@\]] closes the last open box.

*)

val fprintf : formatter -> ('a, formatter, unit) format -> 'a

(** [fprintf ff fmt arg1 ... argN] formats the arguments [arg1] to [argN]
  according to the format string [fmt], and outputs the resulting string on
  the formatter [ff].

  The format string [fmt] is a character string which contains three types of
  objects: plain characters and conversion specifications as specified in
  the {!Printf} module, and pretty-printing indications specific to the
  [Format] module.

  The pretty-printing indication characters are introduced by
  a [@] character, and their meanings are:
  - [@\[]: open a pretty-printing box. The type and offset of the
    box may be optionally specified with the following syntax:
    the [<] character, followed by an optional box type indication,
    then an optional integer offset, and the closing [>] character.
    Pretty-printing box type is one of [h], [v], [hv], [b], or [hov].
    '[h]' stands for an 'horizontal' pretty-printing box,
    '[v]' stands for a 'vertical' pretty-printing box,
    '[hv]' stands for an 'horizontal/vertical' pretty-printing box,
    '[b]' stands for an 'horizontal-or-vertical' pretty-printing box
    demonstrating indentation,
    '[hov]' stands a simple 'horizontal-or-vertical' pretty-printing box.
    For instance, [@\[<hov 2>] opens an 'horizontal-or-vertical'
    pretty-printing box with indentation 2 as obtained with [open_hovbox 2].
    For more details about pretty-printing boxes, see the various box opening
    functions [open_*box].
  - [@\]]: close the most recently opened pretty-printing box.
  - [@,]: output a 'cut' break hint, as with [print_cut ()].
  - [@ ]: output a 'space' break hint, as with [print_space ()].
  - [@;]: output a 'full' break hint as with [print_break]. The
    [nspaces] and [offset] parameters of the break hint may be
    optionally specified with the following syntax:
    the [<] character, followed by an integer [nspaces] value,
    then an integer [offset], and a closing [>] character.
    If no parameters are provided, the good break defaults to a
    'space' break hint.
  - [@.]: flush the pretty-printer and split the line, as with
    [print_newline ()].
  - [@<n>]: print the following item as if it were of length [n].
    Hence, [printf "@<0>%s" arg] prints [arg] as a zero length string.
    If [@<n>] is not followed by a conversion specification,
    then the following character of the format is printed as if
    it were of length [n].
  - [@?]: flush the pretty-printer as with [print_flush ()].
    This is equivalent to the conversion [%!].
  - [@\n]: force a newline, as with [force_newline ()], not the normal way
    of pretty-printing, you should prefer using break hints inside a vertical
    pretty-printing box.

  Note: To prevent the interpretation of a [@] character as a
  pretty-printing indication, escape it with a [%] character.
  Old quotation mode [@@] is deprecated since it is not compatible with
  formatted input interpretation of character ['@'].

  Example: [printf "@[%s@ %d@]@." "x =" 1] is equivalent to
  [open_box (); print_string "x ="; print_space ();
   print_int 1; close_box (); print_newline ()].
  It prints [x = 1] within a pretty-printing 'horizontal-or-vertical' box.

*)

val printf : ('a, formatter, unit) format -> 'a
(** Same as [fprintf] above, but output on [std_formatter]. *)

val eprintf : ('a, formatter, unit) format -> 'a
(** Same as [fprintf] above, but output on [err_formatter]. *)

val sprintf : ('a, unit, string) format -> 'a
(** Same as [printf] above, but instead of printing on a formatter,
  returns a string containing the result of formatting the arguments.
  Note that the pretty-printer queue is flushed at the end of {e each
  call} to [sprintf].

  In case of multiple and related calls to [sprintf] to output
  material on a single string, you should consider using [fprintf]
  with the predefined formatter [str_formatter] and call
  [flush_str_formatter ()] to get the final result.

  Alternatively, you can use [Format.fprintf] with a formatter writing to a
  buffer of your own: flushing the formatter and the buffer at the end of
  pretty-printing returns the desired string.
*)

val asprintf : ('a, formatter, unit, string) format4 -> 'a
(** Same as [printf] above, but instead of printing on a formatter,
  returns a string containing the result of formatting the arguments.
  The type of [asprintf] is general enough to interact nicely with [%a]
  conversions.

  @since 4.01.0
*)

(** Formatted Pretty-Printing with continuations. *)

val kfprintf :
  (formatter -> 'a) -> formatter ->
  ('b, formatter, unit, 'a) format4 -> 'b
(** Same as [fprintf] above, but instead of returning immediately,
  passes the formatter to its first argument at the end of printing. *)

val ikfprintf :
  (formatter -> 'a) -> formatter ->
  ('b, formatter, unit, 'a) format4 -> 'b
(** Same as [kfprintf] above, but does not print anything.
  Useful to ignore some material when conditionally printing.

  @since 3.12.0
*)

val ksprintf : (string -> 'a) -> ('b, unit, string, 'a) format4 -> 'b
(** Same as [sprintf] above, but instead of returning the string,
  passes it to the first argument. *)

val kasprintf : (string -> 'a) -> ('b, formatter, unit, 'a) format4 -> 'b
(** Same as [asprintf] above, but instead of returning the string,
  passes it to the first argument.

  @since 4.03
*)
