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
  type t =
    | Comment of {txt: string; loc: Location.t}
    | Docstring of {txt: string; loc: Location.t}

  let loc (Comment {loc; _} | Docstring {loc; _}) = loc

  let txt (Comment {txt; _} | Docstring {txt; _}) = txt

  let create_comment txt loc = Comment {txt; loc}

  let create_docstring txt loc = Docstring {txt; loc}

  let is_docstring = function Comment _ -> false | Docstring _ -> true

  let compare = Poly.compare

  let sexp_of_t cmt =
    let kind, txt, loc =
      match cmt with
      | Comment {txt; loc} -> ("comment", txt, loc)
      | Docstring {txt; loc} -> ("docstring", txt, loc)
    in
    Sexp.List
      [ Sexp.Atom kind
      ; Sexp.Atom txt
      ; Sexp.Atom (Format.asprintf "%a" Migrate_ast.Location.fmt loc) ]
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

type pos = Before | Within | After

type decoded_kind =
  | Verbatim of string
  | Doc of string
  | Normal of string
  | Code of string
  | Asterisk_prefixed of string list

type decoded = {prefix: string; suffix: string; kind: decoded_kind}

(** [~content_offset] indicates at which column the body of the comment
    starts (1-indexed). [~max_idnent] indicates the maximum amount of
    indentation to trim. *)
let unindent_lines ?(max_indent = Stdlib.max_int) ~content_offset first_line
    tl_lines =
  let tl_indent =
    List.fold_left ~init:max_indent
      ~f:(fun acc s ->
        Option.value_map ~default:acc ~f:(min acc) (String.indent_of_line s) )
      tl_lines
  in
  (* The indentation of the first line must account for the location of the
     comment opening. Don't account for the first line if it's empty.
     [fl_trim] is the number of characters to remove from the first line. *)
  let fl_trim, fl_indent =
    match String.indent_of_line first_line with
    | Some i ->
        (max 0 (min i (tl_indent - content_offset)), i + content_offset - 1)
    | None -> (String.length first_line, max_indent)
  in
  let min_indent = min tl_indent fl_indent in
  let first_line = String.drop_prefix first_line fl_trim in
  first_line
  :: List.map ~f:(fun s -> String.drop_prefix s min_indent) tl_lines

let unindent_lines ?max_indent ~content_offset txt =
  match String.split ~on:'\n' txt with
  | [] -> []
  | hd :: tl -> unindent_lines ?max_indent ~content_offset hd tl

let is_all_whitespace s = String.for_all s ~f:Char.is_whitespace

let split_asterisk_prefixed =
  let prefix = "*" in
  let drop_prefix s = String.drop_prefix s (String.length prefix) in
  let rec lines_are_asterisk_prefixed = function
    | [] -> true
    (* Allow the last line to be empty *)
    | [last] when is_all_whitespace last -> true
    | hd :: tl ->
        String.is_prefix hd ~prefix && lines_are_asterisk_prefixed tl
  in
  function
  (* Check whether the second line is not empty to avoid matching a comment
     with no asterisks. *)
  | fst_line :: (snd_line :: _ as tl)
    when lines_are_asterisk_prefixed tl && not (is_all_whitespace snd_line)
    ->
      Some (fst_line :: List.map tl ~f:drop_prefix)
  | _ -> None

let mk ?(prefix = "") ?(suffix = "") kind = {prefix; suffix; kind}

let decode_comment txt loc =
  let txt =
    (* Windows compatibility *)
    let f = function '\r' -> false | _ -> true in
    String.filter txt ~f
  in
  let opn_offset =
    let {Lexing.pos_cnum; pos_bol; _} = loc.Location.loc_start in
    pos_cnum - pos_bol + 1
  in
  if String.length txt >= 2 then
    match txt.[0] with
    | '$' when not (Char.is_whitespace txt.[1]) -> mk (Verbatim txt)
    | '$' ->
        let dollar_suf = Char.equal txt.[String.length txt - 1] '$' in
        let suffix = if dollar_suf then "$" else "" in
        let code =
          let len = String.length txt - if dollar_suf then 2 else 1 in
          String.sub ~pos:1 ~len txt
        in
        mk ~prefix:"$" ~suffix (Code code)
    | '=' -> mk (Verbatim txt)
    | _ when is_all_whitespace txt ->
        mk (Verbatim " ") (* Make sure not to format to [(**)]. *)
    | _ -> (
        let lines =
          let content_offset = opn_offset + 2 in
          unindent_lines ~content_offset txt
        in
        match split_asterisk_prefixed lines with
        | Some deprefixed_lines -> mk (Asterisk_prefixed deprefixed_lines)
        | None -> mk (Normal txt) )
  else
    match txt with
    (* "(**)" is not parsed as a docstring but as a regular comment
       containing '*' and would be rewritten as "(***)" *)
    | "*" when Location.width loc = 4 -> mk (Verbatim "")
    | ("*" | "$") as txt -> mk (Verbatim txt)
    | "\n" | " " -> mk (Verbatim " ")
    | _ -> mk (Normal txt)

let decode_docstring _loc = function
  | "" -> mk (Verbatim "")
  | ("*" | "$") as txt -> mk (Verbatim txt)
  | "\n" | " " -> mk (Verbatim " ")
  | txt -> mk ~prefix:"*" (Doc txt)

let decode = function
  | Comment {txt; loc} -> decode_comment txt loc
  | Docstring {txt; loc} -> decode_docstring loc txt
