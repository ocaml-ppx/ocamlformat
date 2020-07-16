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

module Format = Format_

type s = (unit, Format.formatter, unit) format

type t = Format.formatter -> unit

type sp = Blank | Cut | Space | Break of int * int

let ( $ ) f g x = f x ; g x

let ( >$ ) f g x = f $ g x

let set_margin n fs = Format.pp_set_geometry fs ~max_indent:n ~margin:(n + 1)

let max_indent = ref None

let set_max_indent n (_ : Format.formatter) = max_indent := Some n

let eval fs t = t fs

let protect t ~on_error fs =
  try t fs
  with exn ->
    Format.pp_print_flush fs () ;
    on_error exn

(** Debug of formatting -------------------------------------------------*)

let pp_color_k color_code k fs =
  let c = Format.sprintf "\x1B[%dm" in
  Format.fprintf fs "@<0>%s%t@<0>%s" (c color_code) k (c 0)

(** Break hints and format strings --------------------------------------*)

let break n o fs = Format.pp_print_break fs n o

let cbreak ~fits ~breaks fs = Format.pp_print_custom_break fs ~fits ~breaks

let noop (_ : Format.formatter) = ()

let fmt f fs = Format.fprintf fs f

(** Primitive types -----------------------------------------------------*)

let char c fs = Format.pp_print_char fs c

let str s fs = if not (String.is_empty s) then Format.pp_print_string fs s

let sp = function
  | Blank -> char ' '
  | Cut -> fmt "@,"
  | Space -> fmt "@ "
  | Break (x, y) -> break x y

(** Primitive containers ------------------------------------------------*)

let opt o pp fs = Option.iter o ~f:(Fn.flip pp fs)

let list_pn x1N pp fs =
  match x1N with
  | [] -> ()
  | [x1] -> pp ~prev:None x1 ~next:None fs
  | x1 :: (x2 :: _ as x2N) ->
      pp ~prev:None x1 ~next:(Some x2) fs ;
      let rec list_pn_ fs prev = function
        | [] -> ()
        | [xI] -> pp ~prev:(Some prev) xI ~next:None fs
        | xI :: (xJ :: _ as xJN) ->
            pp ~prev:(Some prev) xI ~next:(Some xJ) fs ;
            list_pn_ fs xI xJN
      in
      list_pn_ fs x1 x2N

let list_fl xs pp fs =
  list_pn xs
    (fun ~prev x ~next fs ->
      pp ~first:(Option.is_none prev) ~last:(Option.is_none next) x fs)
    fs

let list xs sep pp fs =
  let pp_sep fs () = Format.fprintf fs sep in
  Format.pp_print_list ~pp_sep (fun fs x -> pp x fs) fs xs

let list_k xs pp_sep pp fs =
  let pp_sep fs () = pp_sep fs in
  Format.pp_print_list ~pp_sep (fun fs x -> pp x fs) fs xs

(** Conditional formatting ----------------------------------------------*)

let fmt_if_k cnd k fs = if cnd then k fs

let fmt_if cnd f fs = fmt_if_k cnd (fmt f) fs

let fmt_or_k cnd t_k f_k fs = if cnd then t_k fs else f_k fs

let fmt_or cnd t f fs = fmt_or_k cnd (fmt t) (fmt f) fs

let fmt_opt opt fs = match opt with Some k -> k fs | None -> ()

(** Conditional on immediately following a line break -------------------*)

let if_newline s fs = Format.pp_print_string_if_newline fs s

let break_unless_newline n o fs = Format.pp_print_or_newline fs n o "" ""

(** Conditional on breaking of enclosing box ----------------------------*)

type behavior = Fit | Break

let fits_breaks ?force ?(hint = (0, Int.min_value)) ?(level = 0) fits breaks
    fs =
  let nspaces, offset = hint in
  match force with
  | Some Fit -> Format.pp_print_string fs fits
  | Some Break ->
      if offset >= 0 then Format.pp_print_break fs nspaces offset ;
      Format.pp_print_string fs breaks
  | None ->
      Format.pp_print_fits_or_breaks fs ~level fits nspaces offset breaks

let fits_breaks_if ?force ?hint ?level cnd fits breaks fs =
  if cnd then fits_breaks ?force ?hint ?level fits breaks fs

(** Wrapping ------------------------------------------------------------*)

let wrap_if_k cnd pre suf k = fmt_if_k cnd pre $ k $ fmt_if_k cnd suf

let wrap_k x = wrap_if_k true x

let wrap_if cnd pre suf = wrap_if_k cnd (fmt pre) (fmt suf)

and wrap pre suf = wrap_k (fmt pre) (fmt suf)

let wrap_if_fits_and cnd pre suf k =
  fits_breaks_if cnd pre "" $ k $ fits_breaks_if cnd suf ""

let wrap_if_fits_or cnd pre suf k =
  if cnd then wrap_k (str pre) (str suf) k
  else fits_breaks pre "" $ k $ fits_breaks suf ""

let wrap_fits_breaks_if ?(space = true) c cnd pre suf k =
  match (c.Conf.indicate_multiline_delimiters, space) with
  | `No, false -> wrap_if_k cnd (str pre) (str suf) k
  | `Space, _ | _, true ->
      fits_breaks_if cnd pre (pre ^ " ")
      $ k
      $ fits_breaks_if cnd suf ~hint:(1, 0) suf
  | `Closing_on_separate_line, false ->
      fits_breaks_if cnd pre (pre ^ " ")
      $ k
      $ fits_breaks_if cnd suf ~hint:(1000, 0) suf

let wrap_fits_breaks ?(space = true) conf x =
  wrap_fits_breaks_if ~space conf true x

(** Boxes ---------------------------------------------------------------*)

let box_debug_enabled = ref false

let with_box_debug k fs =
  let g = !box_debug_enabled in
  box_debug_enabled := true ;
  k fs ;
  box_debug_enabled := g

let box_depth = ref 0

(* Numeric part of the ANSI escape sequence for colors *)
let box_depth_colors = [|32; 33; 94; 31; 35; 36|]

let box_depth_color () =
  box_depth_colors.(!box_depth % Array.length box_depth_colors)

let debug_box_open ?name box_kind n fs =
  if !box_debug_enabled then (
    let name =
      match name with
      | Some s -> Format.sprintf "%s:%s" box_kind s
      | None -> box_kind
    in
    let openning = if n = 0 then name else Format.sprintf "%s<%d" name n in
    pp_color_k (box_depth_color ())
      (fun fs -> Format.fprintf fs "@<0>[@<0>%s@<0>>" openning)
      fs ;
    Int.incr box_depth )

let debug_box_close fs =
  if !box_debug_enabled then
    if !box_depth = 0 then
      (* mismatched close, red background *)
      pp_color_k 41 (fun fs -> Format.fprintf fs "@<0>]") fs
    else (
      Int.decr box_depth ;
      pp_color_k (box_depth_color ())
        (fun fs -> Format.fprintf fs "@<0>]")
        fs )

let apply_max_indent n = Option.value_map !max_indent ~f:(min n) ~default:n

let open_box ?name n fs =
  let n = apply_max_indent n in
  debug_box_open ?name "b" n fs ;
  Format.pp_open_box fs n

and open_vbox ?name n fs =
  let n = apply_max_indent n in
  debug_box_open ?name "v" n fs ;
  Format.pp_open_vbox fs n

and open_hvbox ?name n fs =
  let n = apply_max_indent n in
  debug_box_open ?name "hv" n fs ;
  Format.pp_open_hvbox fs n

and open_hovbox ?name n fs =
  let n = apply_max_indent n in
  debug_box_open ?name "hov" n fs ;
  Format.pp_open_hovbox fs n

and close_box fs = debug_box_close fs ; Format.pp_close_box fs ()

(** Wrapping boxes ------------------------------------------------------*)

let cbox ?name n = wrap_k (open_box ?name n) close_box

and vbox ?name n = wrap_k (open_vbox ?name n) close_box

and hvbox ?name n = wrap_k (open_hvbox ?name n) close_box

and hovbox ?name n = wrap_k (open_hovbox ?name n) close_box

and cbox_if ?name cnd n = wrap_if_k cnd (open_box ?name n) close_box

and vbox_if ?name cnd n = wrap_if_k cnd (open_vbox ?name n) close_box

and hvbox_if ?name cnd n = wrap_if_k cnd (open_hvbox ?name n) close_box

and hovbox_if ?name cnd n = wrap_if_k cnd (open_hovbox ?name n) close_box

(** Text filling --------------------------------------------------------*)

let utf8_length s =
  Uuseg_string.fold_utf_8 `Grapheme_cluster (fun n _ -> n + 1) 0 s

let fill_text ?epi text =
  assert (not (String.is_empty text)) ;
  let fmt_line line =
    let words =
      List.filter ~f:(Fn.non String.is_empty)
        (String.split_on_chars line
           ~on:['\t'; '\n'; '\011'; '\012'; '\r'; ' '])
    in
    list words "@ " (fun s fs -> Format.pp_print_as fs (utf8_length s) s)
  in
  let lines =
    List.remove_consecutive_duplicates
      ~equal:(fun x y -> String.is_empty x && String.is_empty y)
      (String.split (String.rstrip text) ~on:'\n')
  in
  fmt_if (String.starts_with_whitespace text) " "
  $ hovbox 0
      ( hvbox 0
          (hovbox 0
             (list_pn lines (fun ~prev:_ curr ~next ->
                  fmt_line curr
                  $
                  match next with
                  | Some str when String.for_all str ~f:Char.is_whitespace ->
                      close_box $ fmt "\n@," $ open_hovbox 0
                  | Some _ when not (String.is_empty curr) -> fmt "@ "
                  | _ -> noop)))
      $ fmt_if
          (String.length text > 1 && String.ends_with_whitespace text)
          " "
      $ opt epi Fn.id )
