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

let unindent_lines ~offset first_line tl_lines =
  (* The indentation of the first line must account for the location of the
     comment opening *)
  let fl_spaces =
    Option.value ~default:0 (String.indent_of_line first_line)
  in
  let fl_indent = fl_spaces + offset in
  let min_indent =
    List.fold_left ~init:fl_indent
      ~f:(fun acc s ->
        Option.value_map ~default:acc ~f:(min acc) (String.indent_of_line s) )
      tl_lines
  in
  (* Completely trim the first line *)
  String.drop_prefix first_line fl_spaces
  :: List.map ~f:(fun s -> String.drop_prefix s min_indent) tl_lines

let unindent_lines ~offset = function
  | [] -> []
  | hd :: tl -> unindent_lines ~offset hd tl
