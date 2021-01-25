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
open Result.Monad_infix

let indent_aux ~lines ~range:(low, high) ~indent_line =
  let nlines = List.length lines in
  let rec aux ?prev acc i =
    if i > high then Ok (List.rev acc)
    else if i >= low then
      let line = Option.value (List.nth lines (i - 1)) ~default:"" in
      if String.(is_empty (strip line)) then aux ?prev (0 :: acc) (i + 1)
      else
        indent_line ?prev ~i ~line nlines
        >>= fun indent -> aux ~prev:(indent, line) (indent :: acc) (i + 1)
    else
      let line = Option.value (List.nth lines (i - 1)) ~default:"" in
      if String.(is_empty (strip line)) then aux ?prev acc (i + 1)
      else
        indent_line ?prev ~i ~line nlines
        >>= fun indent -> aux ~prev:(indent, line) acc (i + 1)
  in
  aux [] low

let last_token x =
  let lexbuf = Lexing.from_string x in
  let rec loop acc =
    match Lexer.token_with_comments lexbuf with
    | Parser.EOF -> acc
    | tok -> loop (Some tok)
  in
  loop None

module Valid_ast = struct
  let rec loc_of_line loctree locs line =
    match locs with
    | [] -> None
    | (h : Location.t) :: t ->
        if h.loc_start.pos_lnum = line then Some h
        else if h.loc_start.pos_lnum <= line && line <= h.loc_end.pos_lnum
        then
          match Loc_tree.children loctree h with
          | [] -> Some h
          | children -> (
            match loc_of_line loctree children line with
            | Some loc -> Some loc
            | None -> Some h )
        else loc_of_line loctree t line

  let matching_loc loc locs locs' =
    match List.zip locs locs' with
    | Ok assoc -> (
        let equal x y = Location.compare x y = 0 in
        match List.Assoc.find assoc ~equal loc with
        | Some loc -> Ok loc
        | None ->
            Error (`Msg "Cannot find matching location in formatted output.")
        )
    | Unequal_lengths ->
        Error (`Msg "Cannot match pre-post formatting locations.")

  let indent_range fragment ~unformatted:(ast, source)
      ~formatted:(formatted_ast, formatted_source) ~lines ~range =
    let loctree, locs = Loc_tree.of_ast fragment ast source in
    let _, locs' = Loc_tree.of_ast fragment formatted_ast formatted_source in
    let indent_line ?prev ~i ~line:_ nlines =
      if i = nlines + 1 then Ok 0
      else
        match loc_of_line loctree locs i with
        | Some loc -> (
            matching_loc loc locs locs'
            >>= fun (loc' : Location.t) ->
            let indent =
              match
                Source.find_first_token_on_line formatted_source
                  loc'.loc_start.pos_lnum
              with
              | Some (_, loc) -> Position.column loc.loc_start
              | None -> impossible "cannot happen"
            in
            match prev with
            | Some (prev_indent, prev_line)
              when indent = prev_indent || indent = 0 -> (
              match last_token prev_line with
              | Some tok -> (
                match Source.indent_after_token tok with
                | Some i -> Ok (prev_indent + i)
                | None -> Ok indent )
              | None -> Ok indent )
            | _ -> Ok indent )
        | None -> Ok 0
    in
    indent_aux ~lines ~range ~indent_line
end

module Partial_ast = struct
  let indent_line ?prev ~i ~line nlines =
    if i = nlines + 1 then Ok 0
    else
      let indent = String.(length line - length (lstrip line)) in
      match prev with
      | Some (prev_indent, prev_line) -> (
        match last_token prev_line with
        | Some tok -> (
          match Source.indent_after_token tok with
          | Some i -> Ok (prev_indent + i)
          | None -> Ok indent )
        | None -> Ok indent )
      | _ -> Ok indent

  let indent_range ~lines ~range = indent_aux ~lines ~range ~indent_line
end
