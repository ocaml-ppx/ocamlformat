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

(** Define the core type and minimal combinators.

    Other higher level functions like [fmt_if] or [list_pn] are implemented
    on top of this. The goal is to be able to modify the underlying
    abstraction without modifying too many places in the [Fmt] module. *)
module T : sig
  type t

  val ( $ ) : t -> t -> t
  (** Sequence *)

  val with_pp : (Format_.formatter -> unit) -> t
  (** Use an arbitrary pretty-printing function *)

  val protect : t -> on_error:(exn -> unit) -> t
  (** Exception handler *)

  val lazy_ : (unit -> t) -> t
  (** Defer the evaluation of some side effects until formatting happens.

      This can matter if for example a list of [t] is built, and then only
      some of them end up being displayed. Using [lazy_] ensures that only
      side effects for the displayed elements have run.

      See [tests_lazy] in [Test_fmt]. *)

  val eval : Format_.formatter -> t -> unit
  (** Main function to evaluate a term using an actual formatter. *)
end = struct
  type t = (Format_.formatter -> unit) Staged.t

  let ( $ ) f g =
    let f = Staged.unstage f in
    let g = Staged.unstage g in
    Staged.stage (fun x -> f x ; g x)

  let with_pp f = Staged.stage f

  let eval fs f =
    let f = Staged.unstage f in
    f fs

  let protect t ~on_error =
    let t = Staged.unstage t in
    Staged.stage (fun fs ->
        try t fs
        with exn ->
          Format_.pp_print_flush fs () ;
          on_error exn )

  let lazy_ f =
    Staged.stage (fun fs ->
        let k = Staged.unstage (f ()) in
        k fs )
end

include T

type sp = Blank | Cut | Space | Break of int * int

let ( >$ ) f g x = f $ g x

let set_margin n =
  with_pp (fun fs ->
      Format_.pp_set_geometry fs ~max_indent:n ~margin:(n + 1) )

let max_indent = ref None

let set_max_indent x = with_pp (fun _ -> max_indent := x)

(** Debug of formatting -------------------------------------------------*)

let with_box_debug k = with_pp (Box_debug.with_box (fun fs -> eval fs k))

(** Break hints and format strings --------------------------------------*)

let break n o =
  let stack = Box_debug.get_stack () in
  with_pp (fun fs ->
      Box_debug.break fs n o ~stack ;
      Format_.pp_print_break fs n o )

let force_break = break 1000 0

let space_break =
  (* a stack is useless here, this would require adding a unit parameter *)
  with_pp (fun fs ->
      Box_debug.space_break fs ;
      Format_.pp_print_space fs () )

let cut_break =
  with_pp (fun fs ->
      Box_debug.cut_break fs ;
      Format_.pp_print_cut fs () )

let force_newline =
  let stack = Box_debug.get_stack () in
  with_pp (fun fs ->
      Box_debug.force_newline ~stack fs ;
      Format_.pp_force_newline fs () )

let cbreak ~fits ~breaks =
  let stack = Box_debug.get_stack () in
  with_pp (fun fs ->
      Box_debug.cbreak fs ~stack ~fits ~breaks ;
      Format_.pp_print_custom_break fs ~fits ~breaks )

let noop = with_pp (fun _ -> ())

let sequence l =
  let rec go l len =
    match l with
    | [] -> noop
    | [x] -> x
    | l ->
        let a_len = len / 2 in
        let b_len = len - a_len in
        let a, b = List.split_n l a_len in
        go a a_len $ go b b_len
  in
  go l (List.length l)

(** Primitive types -----------------------------------------------------*)

let char c = with_pp (fun fs -> Format_.pp_print_char fs c)

let str_length s =
  Uuseg_string.fold_utf_8 `Grapheme_cluster (fun n _ -> n + 1) 0 s

let str_as n s =
  let stack = Box_debug.get_stack () in
  with_pp (fun fs ->
      Box_debug.start_str fs s ;
      Format_.pp_print_as fs n s ;
      Box_debug.end_str ~stack fs )

let str s = if String.is_empty s then noop else str_as (str_length s) s

let sp = function
  | Blank -> char ' '
  | Cut -> cut_break
  | Space -> space_break
  | Break (x, y) -> break x y

(** Primitive containers ------------------------------------------------*)

let opt o f = Option.value_map ~default:noop ~f o

let list_pn x1N pp =
  match x1N with
  | [] -> noop
  | [x1] -> lazy_ (fun () -> pp ~prev:None x1 ~next:None)
  | x1 :: (x2 :: _ as x2N) ->
      let l =
        let rec aux (prev, acc) = function
          | [] -> acc
          | [xI] -> aux (xI, (Some prev, xI, None) :: acc) []
          | xI :: (xJ :: _ as xJN) ->
              aux (xI, (Some prev, xI, Some xJ) :: acc) xJN
        in
        aux (x1, [(None, x1, Some x2)]) x2N
      in
      List.rev_map l ~f:(fun (prev, x, next) ->
          lazy_ (fun () -> pp ~prev x ~next) )
      |> sequence

let list_fl xs pp =
  list_pn xs (fun ~prev x ~next ->
      pp ~first:(Option.is_none prev) ~last:(Option.is_none next) x )

let list l sep f =
  list_fl l (fun ~first:_ ~last x -> f x $ if last then noop else sep)

(** Conditional formatting ----------------------------------------------*)

let fmt_if cnd x = if cnd then x else noop

let fmt_or cnd t f = if cnd then t else f

let fmt_opt o = Option.value o ~default:noop

(** Conditional on immediately following a line break -------------------*)

let if_newline s =
  let stack = Box_debug.get_stack () in
  with_pp (fun fs ->
      Box_debug.if_newline fs ~stack s ;
      Format_.pp_print_string_if_newline fs s )

let break_unless_newline n o =
  let stack = Box_debug.get_stack () in
  with_pp (fun fs ->
      Box_debug.break_unless_newline fs ~stack n o ;
      Format_.pp_print_or_newline fs n o "" "" )

(** Conditional on breaking of enclosing box ----------------------------*)

type behavior = Fit | Break

let fits_or_breaks ~level fits nspaces offset breaks =
  let stack = Box_debug.get_stack () in
  with_pp (fun fs ->
      Box_debug.fits_or_breaks fs ~stack fits nspaces offset breaks ;
      Format_.pp_print_fits_or_breaks fs ~level fits nspaces offset breaks )

let fits_breaks ?force ?(hint = (0, Int.min_value)) ?(level = 0) fits breaks
    =
  let nspaces, offset = hint in
  match force with
  | Some Fit -> str fits
  | Some Break -> fmt_if (offset >= 0) (break nspaces offset) $ str breaks
  | None -> fits_or_breaks ~level fits nspaces offset breaks

let fits_breaks_if ?force ?hint ?level cnd fits breaks =
  fmt_if cnd (fits_breaks ?force ?hint ?level fits breaks)

(** Wrapping ------------------------------------------------------------*)

let wrap_if cnd pre suf k = fmt_if cnd pre $ k $ fmt_if cnd suf

let wrap x = wrap_if true x

let wrap_if_fits_or cnd pre suf k =
  if cnd then wrap (str pre) (str suf) k
  else fits_breaks pre "" $ k $ fits_breaks suf ""

let wrap_fits_breaks_if ?(space = true) (c : Conf.t) cnd pre suf k =
  match (c.fmt_opts.indicate_multiline_delimiters.v, space) with
  | `No, false -> wrap_if cnd (str pre) (str suf) k
  | `Space, _ | `No, true ->
      fits_breaks_if cnd pre (pre ^ " ")
      $ k
      $ fits_breaks_if cnd suf ~hint:(1, 0) suf
  | `Closing_on_separate_line, _ ->
      fits_breaks_if cnd pre (pre ^ " ")
      $ k
      $ fits_breaks_if cnd suf ~hint:(1000, 0) suf

let wrap_fits_breaks ?(space = true) conf x =
  wrap_fits_breaks_if ~space conf true x

(** Boxes ---------------------------------------------------------------*)

let apply_max_indent n = Option.value_map !max_indent ~f:(min n) ~default:n

let open_box ?name n =
  let stack = Box_debug.get_stack () in
  with_pp (fun fs ->
      let n = apply_max_indent n in
      Box_debug.box_open ~stack ?name "b" n fs ;
      Format_.pp_open_box fs n )

and open_vbox ?name n =
  let stack = Box_debug.get_stack () in
  with_pp (fun fs ->
      let n = apply_max_indent n in
      Box_debug.box_open ~stack ?name "v" n fs ;
      Format_.pp_open_vbox fs n )

and open_hvbox ?name n =
  let stack = Box_debug.get_stack () in
  with_pp (fun fs ->
      let n = apply_max_indent n in
      Box_debug.box_open ~stack ?name "hv" n fs ;
      Format_.pp_open_hvbox fs n )

and open_hovbox ?name n =
  let stack = Box_debug.get_stack () in
  with_pp (fun fs ->
      let n = apply_max_indent n in
      Box_debug.box_open ~stack ?name "hov" n fs ;
      Format_.pp_open_hovbox fs n )

and close_box =
  with_pp (fun fs ->
      Box_debug.box_close fs ;
      Format_.pp_close_box fs () )

(** Wrapping boxes ------------------------------------------------------*)

let cbox ?name n = wrap (open_box ?name n) close_box

and vbox ?name n = wrap (open_vbox ?name n) close_box

and hvbox ?name n = wrap (open_hvbox ?name n) close_box

and hovbox ?name n = wrap (open_hovbox ?name n) close_box

and cbox_if ?name cnd n = wrap_if cnd (open_box ?name n) close_box

and vbox_if ?name cnd n = wrap_if cnd (open_vbox ?name n) close_box

and hvbox_if ?name cnd n = wrap_if cnd (open_hvbox ?name n) close_box

and hovbox_if ?name cnd n = wrap_if cnd (open_hovbox ?name n) close_box
