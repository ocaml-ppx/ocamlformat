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

let is_all_whitespace s = String.for_all s ~f:Char.is_whitespace

let split_asterisk_prefixed =
  let prefix = "*" in
  let drop_prefix s =
    if is_all_whitespace s then ""
    else String.drop_prefix s (String.length prefix)
  in
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

let ambiguous_line line =
  String.contains line '"' || String.contains line '{'
  || String.contains line '}'

let mk ?(prefix = "") ?(suffix = "") kind = {prefix; suffix; kind}

let decode_comment ~parse_comments_as_doc ~preserve_ambiguous_line_comments
    txt loc =
  let txt =
    (* Windows compatibility *)
    let f = function '\r' -> false | _ -> true in
    String.filter txt ~f
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
    | c -> (
        let lines =
          txt |> String.split ~on:'\n'
          |> function
          | [] -> [] | hd :: tl -> hd :: List.map ~f:String.lstrip tl
        in
        match lines with
        | [line] when preserve_ambiguous_line_comments && ambiguous_line line
          ->
            mk (Verbatim txt)
        | _ -> (
          match split_asterisk_prefixed lines with
          | Some deprefixed_lines -> mk (Asterisk_prefixed deprefixed_lines)
          | None ->
              if parse_comments_as_doc then
                match c with
                | '_' -> mk ~prefix:"_" (Doc (String.subo ~pos:1 txt))
                | _ -> mk (Doc txt)
              else mk (Normal txt) ) )
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
  | txt when is_all_whitespace txt -> mk (Verbatim " ")
  | txt -> mk ~prefix:"*" (Doc txt)

let decode ~parse_comments_as_doc ~preserve_ambiguous_line_comments =
  function
  | Comment {txt; loc} ->
      decode_comment ~parse_comments_as_doc ~preserve_ambiguous_line_comments
        txt loc
  | Docstring {txt; loc} -> decode_docstring loc txt
