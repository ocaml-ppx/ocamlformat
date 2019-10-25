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

(* A pretty-printing facility and definition of formatters for 'parallel'
   (i.e. unrelated or independent) pretty-printing on multiple out channels. *)

(*
   The pretty-printing engine internal data structures.
*)

let id x = x

(* A devoted type for sizes to avoid confusion
   between sizes and mere integers. *)
module Size : sig
  type t

  val to_int : t -> int
  val of_int : int -> t
  val zero : t
  val unknown : t
  val is_known : t -> bool
end  = struct
  type t = int

  let to_int = id
  let of_int = id
  let zero = 0
  let unknown = -1
  let is_known n = n >= 0
end

open! Caml

(* The pretty-printing boxes definition:
   a pretty-printing box is either
   - hbox: horizontal box (no line splitting)
   - vbox: vertical box (every break hint splits the line)
   - hvbox: horizontal/vertical box
     (the box behaves as an horizontal box if it fits on
      the current line, otherwise the box behaves as a vertical box)
   - hovbox: horizontal or vertical compacting box
     (the box is compacting material, printing as much material as possible
      on every lines)
   - box: horizontal or vertical compacting box with enhanced box structure
     (the box behaves as an horizontal or vertical box but break hints split
      the line if splitting would move to the left)
*)
type box_type = CamlinternalFormatBasics.block_type =
  | Pp_hbox | Pp_vbox | Pp_hvbox | Pp_hovbox | Pp_box | Pp_fits


type fits_or_breaks = {
  fits: string * int * string;   (* when the line is not split *)
  breaks: string * int * string; (* when the line is split *)
}


(* The pretty-printing tokens definition:
   are either text to print or pretty printing
   elements that drive indentation and line splitting. *)
type pp_token =
  | Pp_text of string          (* normal text *)
  | Pp_break of fits_or_breaks (* complete break *)
  | Pp_begin of int * box_type (* beginning of a box *)
  | Pp_end                     (* end of a box *)
  | Pp_newline                 (* to force a newline inside a box *)
  | Pp_if_newline              (* to do something only if this very
                                  line has been broken *)
  | Pp_string_if_newline of string
                               (* print a string only if this very
                                  line has been broken *)
  | Pp_or_newline of int * int * string * string
                               (* print a break and the first string if this
                                  very line has not been broken, otherwise
                                  print the second string *)
  | Pp_fits_or_breaks of int * string * int * int * string
                               (* print a string if the enclosing box fits,
                                  otherwise print a break and a string *)

(* The pretty-printer queue:
   pretty-printing material is not written in the output as soon as emitted;
   instead, the material is simply recorded in the pretty-printer queue,
   until the enclosing box has a known computed size and proper splitting
   decisions can be made.

   The pretty-printer queue contains formatting elements to be printed.
   Each formatting element is a tuple (size, token, length), where
   - length is the declared length of the token,
   - size is effective size of the token when it is printed
     (size is set when the size of the box is known, so that size of break
      hints are definitive). *)
type pp_queue_elem = {
  mutable size : Size.t;
  token : pp_token;
  length : int;
}


(* The pretty-printer queue definition. *)
type pp_queue = pp_queue_elem Queue.t

(* The pretty-printer scanning stack. *)

(* The pretty-printer scanning stack: scanning element definition. *)
type pp_scan_elem = {
  left_total : int; (* Value of pp_left_total when the element was enqueued. *)
  queue_elem : pp_queue_elem
}

(* The pretty-printer formatting stack:
   the formatting stack contains the description of all the currently active
   boxes; the pretty-printer formatting stack is used to split the lines
   while printing tokens. *)

(* The pretty-printer formatting stack: formatting stack element definition.
   Each stack element describes a pretty-printing box. *)
type pp_format_elem = { box_type : box_type; width : int }

(* The formatter definition.
   Each formatter value is a pretty-printer instance with all its
   machinery. *)
type formatter = {
  (* The pretty-printer scanning stack. *)
  pp_scan_stack : pp_scan_elem Stack.t;
  (* The pretty-printer formatting stack. *)
  pp_format_stack : pp_format_elem Stack.t;
  (* Value of right margin. *)
  mutable pp_margin : int;
  (* Minimal space left before margin, when opening a box. *)
  mutable pp_min_space_left : int;
  (* Maximum value of indentation:
     no box can be opened further. *)
  mutable pp_max_indent : int;
  (* Maximum offset added to a new line in addition to the offset of
     the previous line. *)
  mutable pp_max_newline_offset : int;
  (* Space remaining on the current line. *)
  mutable pp_space_left : int;
  (* Current value of indentation. *)
  mutable pp_current_indent : int;
  (* True when the line has been broken by the pretty-printer. *)
  mutable pp_is_new_line : bool;
  (* Total width of tokens already printed. *)
  mutable pp_left_total : int;
  (* Total width of tokens ever put in queue. *)
  mutable pp_right_total : int;
  (* Current number of open boxes. *)
  mutable pp_curr_depth : int;
  (* Maximum number of boxes which can be simultaneously open. *)
  mutable pp_max_boxes : int;
  (* Ellipsis string. *)
  mutable pp_ellipsis : string;
  (* Output function. *)
  mutable pp_out_string : string -> int -> int -> unit;
  (* Flushing function. *)
  mutable pp_out_flush : unit -> unit;
  (* Output of new lines. *)
  mutable pp_out_newline : unit -> unit;
  (* Output of break hints spaces. *)
  mutable pp_out_spaces : int -> unit;
  (* Output of indentation of new lines. *)
  mutable pp_out_indent : int -> unit;
  (* The pretty-printer queue. *)
  pp_queue : pp_queue;
}

(* The formatter functions to output material. *)
type formatter_out_functions = {
  out_string : string -> int -> int -> unit;
  out_flush : unit -> unit;
  out_newline : unit -> unit;
  out_spaces : int -> unit;
  out_indent : int -> unit;
}


(*

  Auxiliaries and basic functions.

*)

(* Enter a token in the pretty-printer queue. *)
let pp_enqueue state token =
  state.pp_right_total <- state.pp_right_total + token.length;
  Queue.add token state.pp_queue


let pp_clear_queue state =
  state.pp_left_total <- 1; state.pp_right_total <- 1;
  Queue.clear state.pp_queue


(* Pp_infinity: large value for default tokens size.

   Pp_infinity is documented as being greater than 1e10; to avoid
   confusion about the word 'greater', we choose pp_infinity greater
   than 1e10 + 1; for correct handling of tests in the algorithm,
   pp_infinity must be even one more than 1e10 + 1; let's stand on the
   safe side by choosing 1.e10+10.

   Pp_infinity could probably be 1073741823 that is 2^30 - 1, that is
   the minimal upper bound for integers; now that max_int is defined,
   this limit could also be defined as max_int - 1.

   However, before setting pp_infinity to something around max_int, we
   must carefully double-check all the integer arithmetic operations
   that involve pp_infinity, since any overflow would wreck havoc the
   pretty-printing algorithm's invariants. Given that this arithmetic
   correctness check is difficult and error prone and given that 1e10
   + 1 is in practice large enough, there is no need to attempt to set
   pp_infinity to the theoretically maximum limit. It is not worth the
   burden ! *)
let pp_infinity = 1000000010

(* Output functions for the formatter. *)
let pp_output_string state s = state.pp_out_string s 0 (String.length s)
and pp_output_newline state = state.pp_out_newline ()
and pp_output_spaces state n = state.pp_out_spaces n
and pp_output_indent state n = state.pp_out_indent n

(* Format a textual token *)
let format_pp_text state size text =
  state.pp_space_left <- state.pp_space_left - size;
  pp_output_string state text;
  state.pp_is_new_line <- false

(* Format a string by its length, if not empty *)
let format_string state s =
  if s <> "" then format_pp_text state (String.length s) s

(* To format a break, indenting a new line. *)
let break_new_line state (before, offset, after) width =
  format_string state before;
  pp_output_newline state;
  state.pp_is_new_line <- true;
  let indent = state.pp_margin - width + offset in
  (* Don't indent more than pp_max_indent. *)
  let real_indent = min state.pp_max_indent indent in
  let real_indent =
    min real_indent (state.pp_current_indent + state.pp_max_newline_offset) in
  state.pp_current_indent <- real_indent;
  state.pp_space_left <- state.pp_margin - state.pp_current_indent;
  pp_output_indent state state.pp_current_indent;
  format_string state after


(* To force a line break inside a box: no offset is added. *)
let break_line state width = break_new_line state ("", 0, "") width

(* To format a break that fits on the current line. *)
let break_same_line state (before, width, after) =
  format_string state before;
  state.pp_space_left <- state.pp_space_left - width;
  pp_output_spaces state width;
  format_string state after


(* To indent no more than pp_max_indent, if one tries to open a box
   beyond pp_max_indent, then the box is rejected on the left
   by simulating a break. *)
let pp_force_break_line state =
  match Stack.top_opt state.pp_format_stack with
  | None -> pp_output_newline state
  | Some { box_type; width } ->
    if width > state.pp_space_left then
      match box_type with
      | Pp_fits | Pp_hbox -> ()
      | Pp_vbox | Pp_hvbox | Pp_hovbox | Pp_box -> break_line state width


(* To skip a token, if the previous line has been broken. *)
let pp_skip_token state =
  match Queue.take_opt state.pp_queue with
  | None -> () (* print_if_newline must have been the last printing command *)
  | Some { size; length; _ } ->
    state.pp_left_total <- state.pp_left_total - length;
    state.pp_space_left <- state.pp_space_left + Size.to_int size


(*

  The main pretty printing functions.

*)

let format_pp_break state size fits breaks =
  let before, off, _ = breaks in
  begin match Stack.top_opt state.pp_format_stack with
  | None -> () (* No open box. *)
  | Some { box_type; width } ->
    begin match box_type with
    | Pp_hovbox ->
      if size + String.length before > state.pp_space_left
      then break_new_line state breaks width
      else break_same_line state fits
    | Pp_box ->
      (* Have the line just been broken here ? *)
      if state.pp_is_new_line then break_same_line state fits else
      if size + String.length before > state.pp_space_left
      then break_new_line state breaks width else
      (* break the line here leads to new indentation ? *)
      if state.pp_current_indent > state.pp_margin - width + off
      then break_new_line state breaks width
      else break_same_line state fits
    | Pp_hvbox -> break_new_line state breaks width
    | Pp_fits -> break_same_line state fits
    | Pp_vbox -> break_new_line state breaks width
    | Pp_hbox -> break_same_line state fits
    end
  end


(* Formatting a token with a given size. *)
let format_pp_token state size = function

  | Pp_text s ->
    format_pp_text state size s

  | Pp_begin (off, ty) ->
    let insertion_point = state.pp_margin - state.pp_space_left in
    if insertion_point > state.pp_max_indent then
      (* can not open a box right there. *)
      begin pp_force_break_line state end;
    let width = state.pp_space_left - off in
    let box_type =
      match ty with
      | Pp_vbox -> Pp_vbox
      | Pp_hbox | Pp_hvbox | Pp_hovbox | Pp_box | Pp_fits ->
        if size > state.pp_space_left then ty else Pp_fits in
    Stack.push { box_type; width } state.pp_format_stack

  | Pp_end ->
    Stack.pop_opt state.pp_format_stack |> ignore

  | Pp_newline ->
    begin match Stack.top_opt state.pp_format_stack with
    | None -> pp_output_newline state (* No open box. *)
    | Some { width; _} -> break_line state width
    end

  | Pp_if_newline ->
    if state.pp_current_indent != state.pp_margin - state.pp_space_left
    then pp_skip_token state

  | Pp_string_if_newline s ->
    if state.pp_is_new_line
    then format_string state s

  | Pp_break { fits; breaks } ->
    format_pp_break state size fits breaks


  | Pp_or_newline (n, off, fits, breaks) ->
    if state.pp_is_new_line
    then format_string state breaks
    else format_pp_break state size ("", n, fits) ("", off, breaks)

  | Pp_fits_or_breaks (level, fits, n, off, breaks) ->
     let check_level level { box_type= ty; width } =
       if level < 0 then level
       else if ty = Pp_fits then
         begin
           if level = 0 then format_string state fits ;
           level - 1
         end
       else
         begin
           if off > min_int then
             begin
               if size + n + String.length breaks >= state.pp_space_left
               then break_new_line state ("", off, "") width
               else break_same_line state ("", n, "")
             end;
           format_string state breaks;
           - 1
         end
     in
     ignore (Stack.fold check_level level state.pp_format_stack)

(* Print if token size is known else printing is delayed.
   Printing is delayed when the text waiting in the queue requires
   more room to format than exists on the current line. *)
let rec advance_left state =
  match Queue.peek_opt state.pp_queue with
  | None -> () (* No tokens to print *)
  | Some { size; token; length } ->
    let pending_count = state.pp_right_total - state.pp_left_total in
    if Size.is_known size || pending_count >= state.pp_space_left then begin
      Queue.take state.pp_queue |> ignore; (* Not empty: we peek into it *)
      let size = if Size.is_known size then Size.to_int size else pp_infinity in
      format_pp_token state size token;
      state.pp_left_total <- length + state.pp_left_total;
      (advance_left [@tailcall]) state
    end


(* To enqueue a token : try to advance. *)
let enqueue_advance state tok = pp_enqueue state tok; advance_left state


(* To enqueue strings. *)
let enqueue_string_as state size s =
  enqueue_advance state { size; token = Pp_text s; length = Size.to_int size }


let enqueue_string state s =
  enqueue_string_as state (Size.of_int (String.length s)) s


(* Routines for scan stack
   determine size of boxes. *)

(* The scan_stack is never empty. *)
let initialize_scan_stack stack =
  Stack.clear stack;
  let queue_elem = { size = Size.unknown; token = Pp_text ""; length = 0 } in
  Stack.push { left_total = -1; queue_elem } stack

(* Setting the size of boxes on scan stack:
   if ty = true then size of break is set else size of box is set;
   in each case pp_scan_stack is popped.

   Note:
   Pattern matching on scan stack is exhaustive, since scan_stack is never
   empty.
   Pattern matching on token in scan stack is also exhaustive,
   since scan_push is used on breaks and opening of boxes. *)
let set_size state ty =
  match Stack.top_opt state.pp_scan_stack with
  | None -> () (* scan_stack is never empty. *)
  | Some { left_total; queue_elem } ->
    let size = Size.to_int queue_elem.size in
    (* test if scan stack contains any data that is not obsolete. *)
    if left_total < state.pp_left_total then
      initialize_scan_stack state.pp_scan_stack
    else
      match queue_elem.token with
      | Pp_break _ | Pp_or_newline _ | Pp_fits_or_breaks _ ->
        if ty then begin
          queue_elem.size <- Size.of_int (state.pp_right_total + size);
          Stack.pop_opt state.pp_scan_stack |> ignore
        end
      | Pp_begin (_, _) ->
        if not ty then begin
          queue_elem.size <- Size.of_int (state.pp_right_total + size);
          Stack.pop_opt state.pp_scan_stack |> ignore
        end
      | Pp_text _ | Pp_end
      | Pp_newline | Pp_if_newline | Pp_string_if_newline _ ->
        () (* scan_push is only used for breaks and boxes. *)


(* Push a token on pretty-printer scanning stack.
   If b is true set_size is called. *)
let scan_push state b token =
  pp_enqueue state token;
  if b then set_size state true;
  let elem = { left_total = state.pp_right_total; queue_elem = token } in
  Stack.push elem state.pp_scan_stack


(* To open a new box :
   the user may set the depth bound pp_max_boxes
   any text nested deeper is printed as the ellipsis string. *)
let pp_open_box_gen state indent br_ty =
  state.pp_curr_depth <- state.pp_curr_depth + 1;
  if state.pp_curr_depth < state.pp_max_boxes then
    let size = Size.of_int (- state.pp_right_total) in
    let elem = { size; token = Pp_begin (indent, br_ty); length = 0 } in
    scan_push state false elem else
  if state.pp_curr_depth = state.pp_max_boxes
  then enqueue_string state state.pp_ellipsis


(* The box which is always open. *)
let pp_open_sys_box state = pp_open_box_gen state 0 Pp_hovbox

(* Close a box, setting sizes of its sub boxes. *)
let pp_close_box state () =
  if state.pp_curr_depth > 1 then
  begin
    if state.pp_curr_depth < state.pp_max_boxes then
    begin
      pp_enqueue state { size = Size.zero; token = Pp_end; length = 0 };
      set_size state true; set_size state false
    end;
    state.pp_curr_depth <- state.pp_curr_depth - 1;
  end

(* Initialize pretty-printer. *)
let pp_rinit state =
  pp_clear_queue state;
  initialize_scan_stack state.pp_scan_stack;
  Stack.clear state.pp_format_stack;
  state.pp_current_indent <- 0;
  state.pp_curr_depth <- 0;
  state.pp_space_left <- state.pp_margin;
  pp_open_sys_box state

(* Flushing pretty-printer queue. *)
let pp_flush_queue state b =
  while state.pp_curr_depth > 1 do
    pp_close_box state ()
  done;
  state.pp_right_total <- pp_infinity;
  advance_left state;
  if b then pp_output_newline state;
  pp_rinit state

(*

  Procedures to format values and use boxes.

*)

(* To format a string. *)
let pp_print_as_size state size s =
  if state.pp_curr_depth < state.pp_max_boxes
  then enqueue_string_as state size s


let pp_print_as state isize s =
  pp_print_as_size state (Size.of_int isize) s


let pp_print_string state s =
  pp_print_as state (String.length s) s


(* To format an integer. *)
let pp_print_int state i = pp_print_string state (Int.to_string i)

(* To format a float. *)
let pp_print_float state f = pp_print_string state (string_of_float f)

(* To format a boolean. *)
let pp_print_bool state b = pp_print_string state (string_of_bool b)

(* To format a char. *)
let pp_print_char state c =
  pp_print_as state 1 (String.make 1 c)


(* Opening boxes. *)
let pp_open_hbox state () = pp_open_box_gen state 0 Pp_hbox
and pp_open_vbox state indent = pp_open_box_gen state indent Pp_vbox

and pp_open_hvbox state indent = pp_open_box_gen state indent Pp_hvbox
and pp_open_hovbox state indent = pp_open_box_gen state indent Pp_hovbox
and pp_open_box state indent = pp_open_box_gen state indent Pp_box


(* Printing queued text.

   [pp_print_flush] prints all pending items in the pretty-printer queue and
   then flushes the low level output device of the formatter to actually
   display printing material.

   [pp_print_newline] behaves as [pp_print_flush] after printing an additional
   new line. *)
let pp_print_newline state () =
  pp_flush_queue state true; state.pp_out_flush ()
and pp_print_flush state () =
  pp_flush_queue state false; state.pp_out_flush ()


(* To get a newline when one does not want to close the current box. *)
let pp_force_newline state () =
  if state.pp_curr_depth < state.pp_max_boxes then
    enqueue_advance state { size = Size.zero; token = Pp_newline; length = 0 }


(* To format something, only in case the line has just been broken. *)
let pp_print_if_newline state () =
  if state.pp_curr_depth < state.pp_max_boxes then
    enqueue_advance state
      { size = Size.zero; token = Pp_if_newline; length = 0 }


(* Generalized break hint that allows to print strings before/after
   same-line offset (width) or new-line offset *)
let pp_print_custom_break state ~fits ~breaks =
  let before, width, after = fits in
  if state.pp_curr_depth < state.pp_max_boxes then
    let size = Size.of_int (- state.pp_right_total) in
    let token = Pp_break { fits; breaks } in
    let length = String.length before + width + String.length after in
    let elem = { size; token; length } in
    scan_push state true elem

(* To format a string, only in case the line has just been broken. *)
let pp_print_string_if_newline state s =
  if state.pp_curr_depth < state.pp_max_boxes then
    let length = String.length s in
    let size = Size.zero in
    let token = Pp_string_if_newline s in
    enqueue_advance state { size; token; length }


(* Printing break hints:
   A break hint indicates where a box may be broken.
   If line is broken then offset is added to the indentation of the current
   box else (the value of) width blanks are printed. *)
let pp_print_break state width offset =
  pp_print_custom_break state
    ~fits:("", width, "") ~breaks:("", offset, "")


(* To format a break and the first string, only in case the line has not just
   been broken, or the second string, in case the line has just been broken. *)
let pp_print_or_newline state width offset fits breaks =
  if state.pp_curr_depth < state.pp_max_boxes then
    let size = Size.of_int (- state.pp_right_total) in
    let token = Pp_or_newline (width, offset, fits, breaks) in
    let width = width + String.length fits in
    scan_push state true { size; token; length= width }


(* To format a string if the enclosing box fits, and otherwise to format a
   break and a string. *)
let pp_print_fits_or_breaks state ?(level = 0) fits nspaces offset breaks =
  if state.pp_curr_depth < state.pp_max_boxes then
    let size = Size.of_int (- state.pp_right_total) in
    let token = Pp_fits_or_breaks (level, fits, nspaces, offset, breaks) in
    let length = String.length fits in
    scan_push state true { size; token; length }


(* Print a space :
   a space is a break hint that prints a single space if the break does not
   split the line;
   a cut is a break hint that prints nothing if the break does not split the
   line. *)
let pp_print_space state () = pp_print_break state 1 0
and pp_print_cut state () = pp_print_break state 0 0

(*

  Procedures to control the pretty-printers

*)

(* Set_max_boxes. *)
let pp_set_max_boxes state n = if n > 1 then state.pp_max_boxes <- n

(* To know the current maximum number of boxes allowed. *)
let pp_get_max_boxes state () = state.pp_max_boxes

let pp_over_max_boxes state () = state.pp_curr_depth = state.pp_max_boxes

(* To set the margin of pretty-printer. *)
let pp_limit n =
  if n < pp_infinity then n else pred pp_infinity


(* Internal pretty-printer functions. *)
let pp_set_min_space_left state n =
  if n >= 1 then
    let n = pp_limit n in
    state.pp_min_space_left <- n;
    state.pp_max_indent <- state.pp_margin - state.pp_min_space_left;
    pp_rinit state


(* Initially, we have :
   pp_max_indent = pp_margin - pp_min_space_left, and
   pp_space_left = pp_margin. *)
let pp_set_max_indent state n =
  if n > 1 then
    pp_set_min_space_left state (state.pp_margin - n)


let pp_get_max_indent state () = state.pp_max_indent

let pp_set_max_newline_offset state n =
  state.pp_max_newline_offset <- n

let pp_set_margin state n =
  if n >= 1 then
    let n = pp_limit n in
    state.pp_margin <- n;
    let new_max_indent =
      (* Try to maintain max_indent to its actual value. *)
      if state.pp_max_indent <= state.pp_margin
      then state.pp_max_indent else
      (* If possible maintain pp_min_space_left to its actual value,
         if this leads to a too small max_indent, take half of the
         new margin, if it is greater than 1. *)
       max (max (state.pp_margin - state.pp_min_space_left)
                (state.pp_margin / 2)) 1 in
    (* Rebuild invariants. *)
    pp_set_max_indent state new_max_indent


(** Geometry functions and types *)
type geometry = { max_indent:int; margin: int}

let check_geometry geometry =
  geometry.max_indent > 1
  &&  geometry.margin > geometry.max_indent

let pp_get_margin state () = state.pp_margin

let pp_set_geometry state ~max_indent ~margin =
  if max_indent < 2 then
    raise (Invalid_argument "Format.pp_set_geometry: max_indent < 2")
  else if margin <= max_indent then
      raise (Invalid_argument "Format.pp_set_geometry: margin <= max_indent")
  else
    pp_set_margin state margin; pp_set_max_indent state max_indent

let pp_safe_set_geometry state ~max_indent ~margin =
  if check_geometry {max_indent;margin} then
    pp_set_geometry state ~max_indent ~margin
  else
    ()

let pp_get_geometry state () =
  { margin = pp_get_margin state (); max_indent = pp_get_max_indent state () }

(* Setting a formatter basic output functions. *)
let pp_set_formatter_out_functions state {
      out_string = f;
      out_flush = g;
      out_newline = h;
      out_spaces = i;
      out_indent = j;
    } =
  state.pp_out_string <- f;
  state.pp_out_flush <- g;
  state.pp_out_newline <- h;
  state.pp_out_spaces <- i;
  state.pp_out_indent <- j

let pp_get_formatter_out_functions state () = {
  out_string = state.pp_out_string;
  out_flush = state.pp_out_flush;
  out_newline = state.pp_out_newline;
  out_spaces = state.pp_out_spaces;
  out_indent = state.pp_out_indent;
}


(* Setting a formatter basic string output and flush functions. *)
let pp_set_formatter_output_functions state f g =
  state.pp_out_string <- f; state.pp_out_flush <- g

let pp_get_formatter_output_functions state () =
  (state.pp_out_string, state.pp_out_flush)


(* The default function to output new lines. *)
let display_newline state () = state.pp_out_string "\n" 0  1

(* The default function to output spaces. *)
let blank_line = String.make 80 ' '
let rec display_blanks state n =
  if n > 0 then
  if n <= 80 then state.pp_out_string blank_line 0 n else
  begin
    state.pp_out_string blank_line 0 80;
    display_blanks state (n - 80)
  end


(* The default function to output indentation of new lines. *)
let display_indent = display_blanks

(* Setting a formatter basic output functions as printing to a given
   [Pervasive.out_channel] value. *)
let pp_set_formatter_out_channel state oc =
  state.pp_out_string <- output_substring oc;
  state.pp_out_flush <- (fun () -> flush oc);
  state.pp_out_newline <- display_newline state;
  state.pp_out_spaces <- display_blanks state;
  state.pp_out_indent <- display_indent state

(*

  Defining specific formatters

*)

(* Building a formatter given its basic output functions.
   Other fields get reasonable default values. *)
let pp_make_formatter f g h i j =
  (* The initial state of the formatter contains a dummy box. *)
  let pp_queue = Queue.create () in
  let sys_tok =
    { size = Size.unknown; token = Pp_begin (0, Pp_hovbox); length = 0 } in
  Queue.add sys_tok pp_queue;
  let scan_stack = Stack.create () in
  initialize_scan_stack scan_stack;
  Stack.push { left_total = 1; queue_elem = sys_tok } scan_stack;
  let pp_margin = 78
  and pp_min_space_left = 10 in
  {
    pp_scan_stack = scan_stack;
    pp_format_stack = Stack.create ();
    pp_margin = pp_margin;
    pp_min_space_left = pp_min_space_left;
    pp_max_indent = pp_margin - pp_min_space_left;
    pp_max_newline_offset = pp_margin - pp_min_space_left;
    pp_space_left = pp_margin;
    pp_current_indent = 0;
    pp_is_new_line = true;
    pp_left_total = 1;
    pp_right_total = 1;
    pp_curr_depth = 1;
    pp_max_boxes = max_int;
    pp_ellipsis = ".";
    pp_out_string = f;
    pp_out_flush = g;
    pp_out_newline = h;
    pp_out_spaces = i;
    pp_out_indent = j;
    pp_queue = pp_queue;
  }


(* Build a formatter out of its out functions. *)
let formatter_of_out_functions out_funs =
  pp_make_formatter
    out_funs.out_string
    out_funs.out_flush
    out_funs.out_newline
    out_funs.out_spaces
    out_funs.out_indent


(* Make a formatter with default functions to output spaces,
  indentation, and new lines. *)
let make_formatter output flush =
  let ppf = pp_make_formatter output flush ignore ignore ignore in
  ppf.pp_out_newline <- display_newline ppf;
  ppf.pp_out_spaces <- display_blanks ppf;
  ppf.pp_out_indent <- display_indent ppf;
  ppf


(* Make a formatter writing to a given [Pervasive.out_channel] value. *)
let formatter_of_out_channel oc =
  make_formatter (output_substring oc) (fun () -> flush oc)


(* Make a formatter writing to a given [Buffer.t] value. *)
let formatter_of_buffer b =
  make_formatter (Buffer.add_substring b) ignore


(* Allocating buffer for pretty-printing purposes.
   Default buffer size is pp_buffer_size or 512.
*)
let pp_buffer_size = 512
let pp_make_buffer () = Buffer.create pp_buffer_size

(* The standard (shared) buffer. *)
let stdbuf = pp_make_buffer ()

(* Predefined formatters standard formatter to print
   to [Stdlib.stdout], [Stdlib.stderr], and {!stdbuf}. *)
let std_formatter = formatter_of_out_channel Stdlib.stdout
and err_formatter = formatter_of_out_channel Stdlib.stderr
and str_formatter = formatter_of_buffer stdbuf


(* [flush_buffer_formatter buf ppf] flushes formatter [ppf],
   then returns the contents of buffer [buf] that is reset.
   Formatter [ppf] is supposed to print to buffer [buf], otherwise this
   function is not really useful. *)
let flush_buffer_formatter buf ppf =
  pp_flush_queue ppf false;
  let s = Buffer.contents buf in
  Buffer.reset buf;
  s


(* Flush [str_formatter] and get the contents of [stdbuf]. *)
let flush_str_formatter () = flush_buffer_formatter stdbuf str_formatter


(* Convenience functions *)

(* To format a list *)
let rec pp_print_list ?(pp_sep = pp_print_cut) pp_v ppf = function
  | [] -> ()
  | [v] -> pp_v ppf v
  | v :: vs ->
    pp_v ppf v;
    pp_sep ppf ();
    pp_print_list ~pp_sep pp_v ppf vs

(* To format free-flowing text *)
let pp_print_text ppf s =
  let len = String.length s in
  let left = ref 0 in
  let right = ref 0 in
  let flush () =
    pp_print_string ppf (String.sub s !left (!right - !left));
    incr right; left := !right;
  in
  while (!right <> len) do
    match s.[!right] with
      | '\n' ->
        flush ();
        pp_force_newline ppf ()
      | ' ' ->
        flush (); pp_print_space ppf ()
      (* there is no specific support for '\t'
         as it is unclear what a right semantics would be *)
      | _ -> incr right
  done;
  if !left <> len then flush ()

let pp_print_option ?(none = fun _ () -> ()) pp_v ppf = function
| None -> none ppf ()
| Some v -> pp_v ppf v

let pp_print_result ~ok ~error ppf = function
| Ok v -> ok ppf v
| Error e -> error ppf e

 (**************************************************************)

let compute_tag output tag_acc =
  let buf = Buffer.create 16 in
  let ppf = formatter_of_buffer buf in
  output ppf tag_acc;
  pp_print_flush ppf ();
  let len = Buffer.length buf in
  if len < 2 then Buffer.contents buf
  else Buffer.sub buf 1 (len - 2)

 (**************************************************************

  Defining continuations to be passed as arguments of
  CamlinternalFormat.make_printf.

  **************************************************************)

open CamlinternalFormatBasics
open CamlinternalFormat
open Compat

(* Interpret a formatting entity on a formatter. *)
let output_formatting_lit ppf fmting_lit = match fmting_lit with
  | Close_box                 -> pp_close_box ppf ()
  | Close_tag                 -> failwith "no tag"
  | Break (_, width, offset)  -> pp_print_break ppf width offset
  | FFlush                    -> pp_print_flush ppf ()
  | Force_newline             -> pp_force_newline ppf ()
  | Flush_newline             -> pp_print_newline ppf ()
  | Magic_size (_, _)         -> ()
  | Escaped_at                -> pp_print_char ppf '@'
  | Escaped_percent           -> pp_print_char ppf '%'
  | Scan_indic c              -> pp_print_char ppf '@'; pp_print_char ppf c

(* Recursively output an "accumulator" containing a reversed list of
   printing entities (string, char, flus, ...) in an output_stream. *)
(* Differ from Printf.output_acc by the interpretation of formatting. *)
(* Used as a continuation of CamlinternalFormat.make_printf. *)
let rec output_acc ppf acc = match acc with
  | Acc_string_literal (Acc_formatting_lit (p, Magic_size (_, size)), s)
  | Acc_data_string (Acc_formatting_lit (p, Magic_size (_, size)), s) ->
    output_acc ppf p;
    pp_print_as_size ppf (Size.of_int size) s;
  | Acc_char_literal (Acc_formatting_lit (p, Magic_size (_, size)), c)
  | Acc_data_char (Acc_formatting_lit (p, Magic_size (_, size)), c) ->
    output_acc ppf p;
    pp_print_as_size ppf (Size.of_int size) (String.make 1 c);
  | Acc_formatting_lit (p, f) ->
    output_acc ppf p;
    output_formatting_lit ppf f;
  | Acc_formatting_gen (_, Acc_open_tag _) ->
    failwith "no tag"
  | Acc_formatting_gen (p, Acc_open_box acc') ->
    output_acc ppf p;
    let (indent, bty) = open_box_of_string (compute_tag output_acc acc') in
    pp_open_box_gen ppf indent bty
  | Acc_string_literal (p, s)
  | Acc_data_string (p, s)   -> output_acc ppf p; pp_print_string ppf s;
  | Acc_char_literal (p, c)
  | Acc_data_char (p, c)     -> output_acc ppf p; pp_print_char ppf c;
  | Acc_delay (p, f)         -> output_acc ppf p; f ppf;
  | Acc_flush p              -> output_acc ppf p; pp_print_flush ppf ();
  | Acc_invalid_arg (p, msg) -> output_acc ppf p; invalid_arg msg;
  | End_of_acc               -> ()

(* Recursively output an "accumulator" containing a reversed list of
   printing entities (string, char, flus, ...) in a buffer. *)
(* Differ from Printf.bufput_acc by the interpretation of formatting. *)
(* Used as a continuation of CamlinternalFormat.make_printf. *)
let rec strput_acc ppf acc = match acc with
  | Acc_string_literal (Acc_formatting_lit (p, Magic_size (_, size)), s)
  | Acc_data_string (Acc_formatting_lit (p, Magic_size (_, size)), s) ->
    strput_acc ppf p;
    pp_print_as_size ppf (Size.of_int size) s;
  | Acc_char_literal (Acc_formatting_lit (p, Magic_size (_, size)), c)
  | Acc_data_char (Acc_formatting_lit (p, Magic_size (_, size)), c) ->
    strput_acc ppf p;
    pp_print_as_size ppf (Size.of_int size) (String.make 1 c);
  | Acc_delay (Acc_formatting_lit (p, Magic_size (_, size)), f) ->
    strput_acc ppf p;
    pp_print_as_size ppf (Size.of_int size) (f ());
  | Acc_formatting_lit (p, f) ->
    strput_acc ppf p;
    output_formatting_lit ppf f;
  | Acc_formatting_gen (_, Acc_open_tag _) ->
    failwith "no tag"
  | Acc_formatting_gen (p, Acc_open_box acc') ->
    strput_acc ppf p;
    let (indent, bty) = open_box_of_string (compute_tag strput_acc acc') in
    pp_open_box_gen ppf indent bty
  | Acc_string_literal (p, s)
  | Acc_data_string (p, s)   -> strput_acc ppf p; pp_print_string ppf s;
  | Acc_char_literal (p, c)
  | Acc_data_char (p, c)     -> strput_acc ppf p; pp_print_char ppf c;
  | Acc_delay (p, f)         -> strput_acc ppf p; pp_print_string ppf (f ());
  | Acc_flush p              -> strput_acc ppf p; pp_print_flush ppf ();
  | Acc_invalid_arg (p, msg) -> strput_acc ppf p; invalid_arg msg;
  | End_of_acc               -> ()

(*

  Defining [fprintf] and various flavors of [fprintf].

*)

let kfprintf k ppf (Format (fmt, _)) =
  make_printf
    (fun ppf acc -> output_acc ppf acc; k ppf)
    ppf End_of_acc fmt

and ikfprintf k ppf (Format (fmt, _)) =
  make_iprintf k ppf fmt

let fprintf ppf = kfprintf ignore ppf
let printf fmt = fprintf std_formatter fmt
let eprintf fmt = fprintf err_formatter fmt

let ksprintf k (Format (fmt, _)) =
  let b = pp_make_buffer () in
  let ppf = formatter_of_buffer b in
  let k () acc =
    strput_acc ppf acc;
    k (flush_buffer_formatter b ppf) in
  make_printf k () End_of_acc fmt


let sprintf fmt = ksprintf id fmt

let kasprintf k (Format (fmt, _)) =
  let b = pp_make_buffer () in
  let ppf = formatter_of_buffer b in
  let k ppf acc =
    output_acc ppf acc;
    k (flush_buffer_formatter b ppf) in
  make_printf k ppf End_of_acc fmt


let asprintf fmt = kasprintf id fmt

(* Flushing standard formatters at end of execution. *)

let flush_standard_formatters () =
  pp_print_flush std_formatter ();
  pp_print_flush err_formatter ()

let () = at_exit flush_standard_formatters
