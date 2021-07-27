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

module T = struct
  type t = {txt: string; loc: Location.t}

  let loc t = t.loc

  let txt t = t.txt

  let create txt loc = {txt; loc}

  let compare =
    Comparable.lexicographic
      [ Comparable.lift String.compare ~f:txt
      ; Comparable.lift Location.compare ~f:loc ]

  let sexp_of_t {txt; loc} =
    Sexp.Atom (Format.asprintf "%s %a" txt Migrate_ast.Location.fmt loc)
end

include T
include Comparator.Make (T)
open Fmt

type pos = Before | Within | After

module Asterisk_prefixed = struct
  let split {txt; loc= {Location.loc_start; _}} =
    let len = Position.column loc_start + 3 in
    let pat =
      String.Search_pattern.create
        (String.init len ~f:(function
          | 0 -> '\n'
          | n when n < len - 1 -> ' '
          | _ -> '*' ) )
    in
    let rec split_ pos =
      match String.Search_pattern.index pat ~pos ~in_:txt with
      | Some 0 -> "" :: split_ len
      | Some idx -> String.sub txt ~pos ~len:(idx - pos) :: split_ (idx + len)
      | _ ->
          let drop = function ' ' | '\t' -> true | _ -> false in
          let line = String.rstrip ~drop (String.drop_prefix txt pos) in
          if String.is_empty line then [" "]
          else if Char.equal line.[String.length line - 1] '\n' then
            [String.drop_suffix line 1; ""]
          else if Char.is_whitespace txt.[String.length txt - 1] then
            [line ^ " "]
          else [line]
    in
    split_ 0

  let fmt lines =
    vbox 1
      ( fmt "(*"
      $ list_fl lines (fun ~first:_ ~last line ->
            match line with
            | "" when last -> fmt ")"
            | _ -> str line $ fmt_or last "*)" "@,*" ) )
end

module Unwrapped = struct
  let unindent_lines ~opn_pos first_line tl_lines =
    let indent_of_line s =
      (* index of first non-whitespace is indentation, None means white
         line *)
      String.lfindi s ~f:(fun _ c -> not (Char.is_whitespace c))
    in
    (* The indentation of the first line must account for the location of the
       comment opening *)
    let fl_spaces = Option.value ~default:0 (indent_of_line first_line) in
    let fl_offset = opn_pos.Lexing.pos_cnum - opn_pos.pos_bol + 2 in
    let fl_indent = fl_spaces + fl_offset in
    let min_indent =
      List.fold_left ~init:fl_indent
        ~f:(fun acc s ->
          Option.value_map ~default:acc ~f:(min acc) (indent_of_line s) )
        tl_lines
    in
    (* Completely trim the first line *)
    String.drop_prefix first_line fl_spaces
    :: List.map ~f:(fun s -> String.drop_prefix s min_indent) tl_lines

  let fmt_multiline_cmt ?epi ~opn_pos ~starts_with_sp first_line tl_lines =
    let is_white_line s = String.for_all s ~f:Char.is_whitespace in
    let unindented = unindent_lines ~opn_pos first_line tl_lines in
    let fmt_line ~first ~last:_ s =
      let sep, sp =
        if is_white_line s then (str "\n", noop)
        else (fmt "@;<1000 0>", fmt_if starts_with_sp " ")
      in
      fmt_if_k (not first) sep $ sp $ str (String.rstrip s)
    in
    vbox 0 ~name:"multiline" (list_fl unindented fmt_line $ fmt_opt epi)

  let fmt ~ocp_indent_compat {txt= s; loc} pos =
    let is_sp = function ' ' | '\t' -> true | _ -> false in
    match String.split_lines (String.rstrip s) with
    | first_line :: (_ :: _ as tl) when not (String.is_empty first_line) ->
        if ocp_indent_compat then
          (* Not adding artificial breaks and keeping the comment contents
             verbatim will not interfere with ocp-indent. *)
          match pos with
          | Before -> wrap "(*" "*)" @@ str s
          | Within -> wrap "(*" "*)" @@ str s
          | After -> break_unless_newline 1000 0 $ wrap "(*" "*)" @@ str s
        else
          let epi =
            (* Preserve position of closing but strip empty lines at the
               end *)
            match String.rfindi s ~f:(fun _ c -> not (is_sp c)) with
            | Some i when Char.( = ) s.[i] '\n' ->
                break 1000 (-2) (* Break before closing *)
            | Some i when i < String.length s - 1 ->
                str " " (* Preserve a space at the end *)
            | _ -> noop
          in
          (* Preserve the first level of indentation *)
          let starts_with_sp = is_sp first_line.[0] in
          wrap "(*" "*)"
          @@ fmt_multiline_cmt ~opn_pos:loc.loc_start ~epi ~starts_with_sp
               first_line tl
    | _ -> wrap "(*" "*)" @@ str s
end

let fmt cmt ~wrap:wrap_comments ~ocp_indent_compat ~fmt_code pos =
  let mode =
    match cmt.txt with
    | "" -> impossible "not produced by parser"
    (* "(**)" is not parsed as a docstring but as a regular comment
       containing '*' and would be rewritten as "(***)" *)
    | "*" when Location.width cmt.loc = 4 -> `Verbatim "(**)"
    | "*" -> `Verbatim "(***)"
    | "$" -> `Verbatim "(*$*)"
    | str when Char.equal str.[0] '$' -> (
        let dollar_suf = Char.equal str.[String.length str - 1] '$' in
        let cls : Fmt.s = if dollar_suf then "$*)" else "*)" in
        let len = String.length str - if dollar_suf then 2 else 1 in
        let source = String.sub ~pos:1 ~len str in
        match fmt_code source with
        | Ok formatted -> `Code (formatted, cls)
        | Error () -> `Unwrapped cmt )
    | _ -> (
      match Asterisk_prefixed.split cmt with
      | [] | [""] -> impossible "not produced by split_asterisk_prefixed"
      | [""; ""] -> `Verbatim "(* *)"
      | [text] when wrap_comments -> `Wrapped (text, "*)")
      | [text; ""] when wrap_comments -> `Wrapped (text, " *)")
      | [_] | [_; ""] -> `Unwrapped cmt
      | lines -> `Asterisk_prefixed lines )
  in
  match mode with
  | `Verbatim x -> str x
  | `Code (x, cls) -> hvbox 2 @@ wrap "(*$@;" cls (x $ fmt "@;<1 -2>")
  | `Wrapped (x, epi) -> str "(*" $ fill_text x ~epi
  | `Unwrapped x -> Unwrapped.fmt ~ocp_indent_compat x pos
  | `Asterisk_prefixed x -> Asterisk_prefixed.fmt x
