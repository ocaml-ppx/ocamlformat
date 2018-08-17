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

(** Formatting combinators *)

module Format = Format_

type s = (unit, Format.formatter, unit) format

type t = Format.formatter -> unit

let ( >$ ) f g x = f $ g x

let set_margin n fs =
  Format.pp_set_margin fs n ;
  Format.pp_set_max_indent fs (n - 1)

(** Break hints and format strings --------------------------------------*)

let break n o fs = Format.pp_print_break fs n o

let fmt f fs = Format.fprintf fs f

(** Primitive types -----------------------------------------------------*)

let char c fs = Format.pp_print_char fs c

let str s fs = Format.pp_print_string fs s

(** Primitive containers ------------------------------------------------*)

let opt o pp fs = Option.iter o ~f:(Fn.flip pp fs)

let list_pn x1N (pp : ?prev:_ -> _ -> ?next:_ -> _ -> unit) fs =
  match x1N with
  | [] -> ()
  | [x1] -> pp ?prev:None x1 ?next:None fs
  | x1 :: (x2 :: _ as x2N) ->
      pp ?prev:None x1 ~next:x2 fs ;
      let rec list_pn_ fs prev = function
        | [] -> ()
        | [xI] -> pp ~prev xI ?next:None fs
        | xI :: (xJ :: _ as xJN) ->
            pp ~prev xI ~next:xJ fs ; list_pn_ fs xI xJN
      in
      list_pn_ fs x1 x2N

let list_fl xs pp fs =
  list_pn xs
    (fun ?prev x ?next fs ->
      pp ~first:(Option.is_none prev) ~last:(Option.is_none next) x fs )
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

(** Conditional on immediately following a line break -------------------*)

let if_newline s fs = Format.pp_print_string_if_newline fs s

let break_unless_newline n o fs = Format.pp_print_or_newline fs n o "" ""

let or_newline fits breaks fs =
  Format.pp_print_or_newline fs 1 0 fits breaks

(** Conditional on immediately preceding a line break -------------------*)

let pre_break n s o fs = Format.pp_print_pre_break fs n s o

(** Conditional on breaking of enclosing box ----------------------------*)

let fits_breaks ?(force_fit_if = false) ?(force_break_if = false) fits
    breaks fs =
  let n, o, b =
    let len = String.length breaks in
    if len >= 2 && Char.equal breaks.[0] '@' then
      let b = String.sub breaks ~pos:2 ~len:(len - 2) in
      match breaks.[1] with
      | ';' -> (
        try Scanf.sscanf breaks "@;<%d %d>%s" (fun x y z -> (x, y, z)) with
        | Scanf.Scan_failure _ | End_of_file -> (1, 0, b) )
      | ',' -> (0, 0, b)
      | ' ' -> (1, 0, b)
      | _ -> (0, Int.min_value, breaks)
    else (0, Int.min_value, breaks)
  in
  if force_fit_if then Format.pp_print_string fs fits
  else if force_break_if then (
    if o >= 0 then Format.pp_print_break fs n o ;
    Format.pp_print_string fs b )
  else Format.pp_print_fits_or_breaks fs fits n o b

let fits_breaks_if ?force_fit_if ?force_break_if cnd fits breaks fs =
  if cnd then fits_breaks ?force_fit_if ?force_break_if fits breaks fs

(** Wrapping ------------------------------------------------------------*)

let wrap_if_k cnd pre suf k fs =
  if cnd then pre fs ;
  k fs ;
  if cnd then suf fs

let wrap_k x = wrap_if_k true x

let wrap_if cnd pre suf = wrap_if_k cnd (fmt pre) (fmt suf)
and wrap pre suf = wrap_k (fmt pre) (fmt suf)

let wrap_if_breaks pre suf k fs =
  fits_breaks "" pre fs ; k fs ; fits_breaks "" suf fs

let wrap_if_fits_and cnd pre suf k fs =
  fits_breaks_if cnd pre "" fs ;
  k fs ;
  fits_breaks_if cnd suf "" fs

let wrap_fits_breaks_if cnd pre suf k fs =
  fits_breaks_if cnd pre (pre ^ " ") fs ;
  k fs ;
  fits_breaks_if cnd suf ("@ " ^ suf) fs

let wrap_fits_breaks x = wrap_fits_breaks_if true x

(** Boxes ---------------------------------------------------------------*)

let open_box n fs = Format.pp_open_box fs n
and open_vbox n fs = Format.pp_open_vbox fs n
and open_hvbox n fs = Format.pp_open_hvbox fs n
and open_hovbox n fs = Format.pp_open_hovbox fs n
and close_box fs = Format.pp_close_box fs ()

(** Wrapping boxes ------------------------------------------------------*)

let cbox n = wrap_k (open_box n) close_box
and vbox n = wrap_k (open_vbox n) close_box
and hvbox n = wrap_k (open_hvbox n) close_box
and hovbox n = wrap_k (open_hovbox n) close_box
and cbox_if cnd n = wrap_if_k cnd (open_box n) close_box
and vbox_if cnd n = wrap_if_k cnd (open_vbox n) close_box
and hvbox_if cnd n = wrap_if_k cnd (open_hvbox n) close_box
and hovbox_if cnd n = wrap_if_k cnd (open_hovbox n) close_box

(** Text filling --------------------------------------------------------*)

let fill_text text =
  let fmt_line line =
    let words =
      List.filter ~f:(Fn.non String.is_empty)
        (String.split_on_chars line
           ~on:['\t'; '\n'; '\011'; '\012'; '\r'; ' '])
    in
    list words "@ " str
  in
  let lines =
    List.remove_consecutive_duplicates
      ~equal:(fun x y -> String.is_empty x && String.is_empty y)
      (String.split (String.rstrip text) ~on:'\n')
  in
  fmt_if (Char.is_whitespace text.[0]) " "
  $ vbox 0
      (hovbox 0
         (list_pn lines (fun ?prev:_ curr ?next ->
              fmt_line curr
              $
              match next with
              | Some str when String.for_all str ~f:Char.is_whitespace ->
                  close_box $ fmt "\n@," $ open_hovbox 0
              | Some _ when not (String.is_empty curr) -> fmt "@ "
              | _ -> fmt "" )))
  $ fmt_if (Char.is_whitespace text.[String.length text - 1]) " "
