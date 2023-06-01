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

(** [opn_offset] indicates at which column the body of the comment starts. *)
let unindent_lines ~opn_offset first_line tl_lines =
  let indent_of_line s =
    (* index of first non-whitespace is indentation, None means white line *)
    String.lfindi s ~f:(fun _ c -> not (Char.is_whitespace c))
  in
  (* The indentation of the first line must account for the location of the
     comment opening. Don't account for the first line if it's empty. *)
  let fl_spaces, fl_indent =
    match indent_of_line first_line with
    | Some i -> (i, i + opn_offset)
    | None -> (0, Stdlib.max_int)
  in
  let min_indent =
    List.fold_left ~init:fl_indent
      ~f:(fun acc s ->
        match indent_of_line s with Some i -> min acc i | None -> acc )
      tl_lines
  in
  (* Completely trim the first line *)
  String.drop_prefix first_line fl_spaces
  :: List.map ~f:(fun s -> String.drop_prefix s min_indent) tl_lines

let unindent_lines ~opn_offset txt =
  match String.split_lines txt with
  | [] -> []
  | hd :: tl -> unindent_lines ~opn_offset hd tl

let split_asterisk_prefixed lines =
  if List.for_all ~f:(String.is_prefix ~prefix:"*") lines then
    Some (List.map lines ~f:(fun s -> String.drop_prefix s 1))
  else None

let mk ?(prefix = "") ?(suffix = "") kind = {prefix; suffix; kind}

let is_all_whitespace s =
  Option.is_none
  @@ String.lfindi s ~f:(fun _ c -> not (Char.is_whitespace c))

let decode ~parse_comments_as_doc {txt; loc} =
  let txt =
    (* Windows compatibility *)
    let f = function '\r' -> false | _ -> true in
    String.filter txt ~f
  in
  let opn_offset =
    let {Lexing.pos_cnum; pos_bol; _} = loc.Location.loc_start in
    pos_cnum - pos_bol + 2
  in
  if String.length txt >= 2 then
    match txt.[0] with
    | '$' when not (Char.is_whitespace txt.[1]) -> mk (Verbatim txt)
    | '$' ->
        let opn_offset = opn_offset + 1 in
        let dollar_suf = Char.equal txt.[String.length txt - 1] '$' in
        let suffix = if dollar_suf then "$" else "" in
        let len = String.length txt - if dollar_suf then 2 else 1 in
        (* Strip white lines at the end but not at the start until after
           [unindent_lines] is called. *)
        let source = String.rstrip (String.sub ~pos:1 ~len txt) in
        let lines = unindent_lines ~opn_offset source in
        let lines = List.map ~f:String.rstrip lines in
        let lines = List.drop_while ~f:String.is_empty lines in
        mk ~prefix:"$" ~suffix (Code lines)
    | '=' -> mk (Verbatim txt)
    | '*' -> mk ~prefix:"*" (Doc (String.drop_prefix txt 1))
    | _ when is_all_whitespace txt ->
        mk (Verbatim " ") (* Make sure not to format to [(**)]. *)
    | _ when parse_comments_as_doc -> mk (Doc txt)
    | _ -> (
        let prefix = if String.starts_with_whitespace txt then " " else ""
        and suffix = if String.ends_with_whitespace txt then " " else "" in
        let txt = String.rstrip txt in
        let lines = unindent_lines ~opn_offset txt in
        match split_asterisk_prefixed lines with
        | Some deprefixed_lines -> mk (Asterisk_prefixed deprefixed_lines)
        | None ->
            (* Reconstruct the text with indentation removed and heading and
               trailing empty lines removed. *)
            let txt = String.lstrip (String.concat ~sep:"\n" lines) in
            mk ~prefix ~suffix (Normal txt) )
  else
    match txt with
    (* "(**)" is not parsed as a docstring but as a regular comment
       containing '*' and would be rewritten as "(***)" *)
    | "*" when Location.width loc = 4 -> mk (Verbatim "")
    | ("*" | "$") as txt -> mk (Verbatim txt)
    | "\n" | " " -> mk (Verbatim " ")
    | _ -> mk (Normal txt)
