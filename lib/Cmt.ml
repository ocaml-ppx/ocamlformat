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

type error =
  { kind: [`Added of t | `Modified of t * t | `Dropped of t]
  ; cmt_kind: [`Comment | `Doc_comment] }

let pp_error fs {kind; cmt_kind} =
  let pp_cmt fs x =
    match cmt_kind with
    | `Doc_comment -> Format.fprintf fs "(** %s *)" (txt x)
    | `Comment -> Format.fprintf fs "(* %s *)" (txt x)
  in
  let s_kind =
    match cmt_kind with
    | `Doc_comment -> "doc-comment"
    | `Comment -> "comment"
  in
  match kind with
  | `Added x ->
      Format.fprintf fs "%!@{<loc>%a@}:@,@{<error>Error@}: %s %a added.\n%!"
        Location.print_loc (loc x) s_kind pp_cmt x
  | `Dropped x ->
      Format.fprintf fs
        "%!@{<loc>%a@}:@,@{<error>Error@}: %s %a dropped.\n%!"
        Location.print_loc (loc x) s_kind pp_cmt x
  | `Modified (x, y) -> (
      Format.fprintf fs
        "%!@{<loc>%a@}:@,\
         @{<error>Error@}: formatting of %s is unstable.\n\
        \  before: %a\n\
        \   after: %a\n\
         %!"
        Location.print_loc (loc x) s_kind pp_cmt x pp_cmt y ;
      match cmt_kind with
      | `Comment -> ()
      | `Doc_comment ->
          Format.fprintf fs
            "Please tighten up this comment in the source or disable the \
             formatting using the option --no-parse-docstrings.\n\
             %!" )

module T_no_loc = struct
  include T

  let compare =
    Comparable.lexicographic [Comparable.lift String.compare ~f:txt]
end

type loc = t

module Comparator_no_loc = struct
  type t = loc

  include Comparator.Make (T_no_loc)
end

type pos = Before | Within | After

type decoded_kind =
  | Verbatim of string
  | Doc of string
  | Normal of string
  | Code of string list
  | Asterisk_prefixed of string list

type decoded = {prefix: string; suffix: string; kind: decoded_kind}

let unindent_lines ~opn_pos first_line tl_lines =
  let indent_of_line s =
    (* index of first non-whitespace is indentation, None means white line *)
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

let unindent_lines ~opn_pos txt =
  match String.split_lines txt with
  | [] -> []
  | hd :: tl -> unindent_lines ~opn_pos hd tl

let split_asterisk_prefixed lines =
  if List.for_all ~f:(String.is_prefix ~prefix:"*") lines then
    Some (List.map lines ~f:(fun s -> String.drop_prefix s 1))
  else None

let mk ?(prefix = "") ?(suffix = "") kind = {prefix; suffix; kind}

let decode {txt; loc} =
  let txt =
    (* Windows compatibility *)
    let f = function '\r' -> false | _ -> true in
    String.filter txt ~f
  in
  let opn_pos = loc.Location.loc_start in
  if String.length txt >= 2 then
    match txt.[0] with
    | '$' when not (Char.is_whitespace txt.[1]) -> mk (Verbatim txt)
    | '$' ->
        let dollar_suf = Char.equal txt.[String.length txt - 1] '$' in
        let suffix = if dollar_suf then "$" else "" in
        let len = String.length txt - if dollar_suf then 2 else 1 in
        let source = String.sub ~pos:1 ~len txt in
        let source =
          String.lstrip ~drop:(function '\n' -> true | _ -> false) source
        in
        let lines = unindent_lines ~opn_pos source in
        let lines = List.map ~f:String.rstrip lines in
        mk ~prefix:"$" ~suffix (Code lines)
    | '=' -> mk (Verbatim txt)
    | '*' -> mk ~prefix:"*" (Doc (String.drop_prefix txt 1))
    | _ -> (
        let prefix = if String.starts_with_whitespace txt then " " else ""
        and suffix = if String.ends_with_whitespace txt then " " else "" in
        let lines = unindent_lines ~opn_pos txt in
        match split_asterisk_prefixed lines with
        | Some deprefixed_lines -> mk (Asterisk_prefixed deprefixed_lines)
        | None ->
            (* Reconstruct the text with indentation removed and heading and
               trailing empty lines removed. *)
            let txt = String.strip (String.concat ~sep:"\n" lines) in
            let cmt =
              if String.is_empty txt then Verbatim "" else Normal txt
            in
            mk ~prefix ~suffix cmt )
  else
    match txt with
    (* "(**)" is not parsed as a docstring but as a regular comment
       containing '*' and would be rewritten as "(***)" *)
    | "*" when Location.width loc = 4 -> mk (Verbatim "")
    | ("*" | "$") as txt -> mk (Verbatim txt)
    | "\n" | " " -> mk (Verbatim " ")
    | _ -> mk (Normal txt)
