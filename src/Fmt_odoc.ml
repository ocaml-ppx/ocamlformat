(**********************************************************************
 *                                                                    *
 *                            OCamlFormat                             *
 *                                                                    *
 *  Copyright (c) 2018-present, Facebook, Inc.  All rights reserved.  *
 *                                                                    *
 *  This source code is licensed under the MIT license found in the   *
 *  LICENSE file in the root directory of this source tree.           *
 *                                                                    *
 **********************************************************************)

open Fmt
open Odoc_parser.Ast
module Location_ = Odoc_model.Location_

type conf = {conf: Conf.t; fmt_code: Conf.t -> string -> Fmt.t}

(** Escape characters if they are not already escaped. [escapeworthy] should
    be [true] if the character should be escaped, [false] otherwise. *)
let ensure_escape ?(escape_char = '\\') ~escapeworthy s =
  let dst = Buffer.create (String.length s + 8) in
  let prev_off = ref 0 in
  let stash until =
    Buffer.add_substring dst s ~pos:!prev_off ~len:(until - !prev_off)
  in
  let len = String.length s in
  for i = 0 to len - 1 do
    if
      escapeworthy s.[i]
      && not (String.Escaping.is_char_escaped s ~escape_char i)
    then (
      stash i ;
      prev_off := i ;
      Buffer.add_char dst escape_char )
  done ;
  stash len ;
  Buffer.contents dst

let escape_brackets s =
  let escapeworthy = function '[' | ']' -> true | _ -> false in
  ensure_escape ~escapeworthy s

let escape_all s =
  let escapeworthy = function
    | '@' | '{' | '}' | '[' | ']' -> true
    | _ -> false
  in
  ensure_escape ~escapeworthy s

let split_on_whitespaces =
  String.split_on_chars ~on:['\t'; '\n'; '\011'; '\012'; '\r'; ' ']

(** Escape special characters and normalize whitespaces *)
let str_normalized ?(escape = escape_all) s =
  split_on_whitespaces s
  |> List.filter ~f:(Fn.non String.is_empty)
  |> fun s -> list s "@ " (fun s -> escape s |> str)

let fmt_if_not_empty lst fmt = fmt_if (not (List.is_empty lst)) fmt

let ign_loc ~f with_loc = f with_loc.Location_.value

let fmt_verbatim_block s =
  let force_break = String.contains s '\n' in
  let content =
    (* Literal newline to avoid indentation *)
    if force_break then wrap "\n" "@\n" (str s)
    else fits_breaks " " "\n" $ str s $ fits_breaks " " ~hint:(0, 0) ""
  in
  hvbox 0 (wrap "{v" "v}" content)

let fmt_code_block conf s =
  try hvbox 0 (wrap "{[@;<1 2>" "@ ]}" (conf.fmt_code conf.conf s))
  with _ ->
    let fmt_line ~first ~last:_ l =
      let l = String.rstrip l in
      if first then str l
      else if String.length l = 0 then str "\n"
      else fmt "@," $ str l
    in
    let lines = String.split_lines s in
    let box = match lines with _ :: _ :: _ -> vbox 0 | _ -> hvbox 0 in
    box (wrap "{[@;<1 2>" "@ ]}" (vbox 0 (list_fl lines fmt_line)))

let fmt_reference = ign_loc ~f:str_normalized

(* Decide between using light and heavy syntax for lists *)
let list_should_use_heavy_syntax items =
  let heavy_nestable_block_elements = function
    (* More than one element or contains a list *)
    | [{Location_.value= `List _; _}] | _ :: _ :: _ -> true
    | [] | [_] -> false
  in
  List.exists items ~f:heavy_nestable_block_elements

(* Decide if should break between two elements *)
let block_element_should_break elem next =
  match (elem, next) with
  (* Mandatory breaks *)
  | `List (_, _, _), _ | `Paragraph _, `Paragraph _ -> true
  (* Arbitrary breaks *)
  | (`Paragraph _ | `Heading _), _ | _, (`Paragraph _ | `Heading _) -> true
  | _, _ -> false

(* Format a list of block_elements separated by newlines Inserts blank line
   depending on [block_element_should_break] *)
let list_block_elem elems f =
  list_pn elems (fun ?prev:_ elem ?next ->
      let elem = elem.Location_.value in
      let break =
        match next with
        | Some {Location_.value= n; _}
          when block_element_should_break
                 (elem :> block_element)
                 (n :> block_element) ->
            fmt "\n@\n"
        | Some _ -> fmt "@\n"
        | None -> fmt ""
      in
      f elem $ break)

(* Format each element with [fmt_elem] *)
let fmt_styled style fmt_elem elems =
  let s =
    match style with
    | `Bold -> "b"
    | `Italic -> "i"
    | `Emphasis -> "e"
    | `Superscript -> "^"
    | `Subscript -> "_"
  in
  hovbox 0
    (wrap "{" "}"
       ( str_normalized s
       $ fmt_if_not_empty elems "@ "
       $ list elems "" fmt_elem ))

let rec fmt_inline_element : inline_element -> Fmt.t = function
  | `Space _ -> fmt "@ "
  | `Word w ->
      (* Escape lines starting with '+' or '-' *)
      let escape =
        fmt_if_k
          (String.length w > 0 && Char.(w.[0] = '+' || w.[0] = '-'))
          (if_newline "\\")
      in
      escape $ str_normalized w
  | `Code_span s ->
      let s = escape_brackets s in
      hovbox 0 (wrap "[" "]" (str_normalized ~escape:escape_brackets s))
  | `Raw_markup (lang, s) ->
      let lang =
        match lang with
        | Some l -> str_normalized l $ str ":"
        | None -> noop
      in
      wrap "{%%" "%%}" (lang $ str s)
  | `Styled (style, elems) ->
      fmt_styled style (ign_loc ~f:fmt_inline_element) elems
  | `Reference (_kind, ref, txt) ->
      let ref = fmt "{!" $ fmt_reference ref $ fmt "}" in
      if List.is_empty txt then ref
      else
        hovbox 0 (wrap "{" "}" (ref $ fmt "@ " $ fmt_inline_elements txt))
  | `Link (url, txt) -> (
      let url = wrap "{:" "}" (str_normalized url) in
      match txt with
      | [] -> url
      | txt -> wrap "{" "}" (url $ fmt "@ " $ fmt_inline_elements txt) )

and fmt_inline_elements txt = list txt "" (ign_loc ~f:fmt_inline_element)

and fmt_nestable_block_element c = function
  | `Paragraph elems -> hovbox 0 (fmt_inline_elements elems)
  | `Code_block s -> fmt_code_block c s
  | `Verbatim s -> fmt_verbatim_block s
  | `Modules mods ->
      hovbox 0
        (wrap "{!modules:@," "@,}"
           (list mods "@ " (fun ref -> fmt_reference ref)))
  | `List (k, _syntax, items) when list_should_use_heavy_syntax items ->
      fmt_list_heavy c k items
  | `List (k, _syntax, items) -> fmt_list_light c k items

and fmt_list_heavy c kind items =
  let fmt_item elems =
    let box = match elems with [_] -> hvbox 3 | _ -> vbox 3 in
    box (wrap "{- " "@;<1 -3>}" (fmt_nestable_block_elements c elems))
  and start : s =
    match kind with `Unordered -> "{ul@," | `Ordered -> "{ol@,"
  in
  vbox 1 (wrap start "@;<1 -1>}" (list items "@," fmt_item))

and fmt_list_light c kind items =
  let line_start =
    match kind with `Unordered -> fmt "- " | `Ordered -> fmt "+ "
  in
  let fmt_item elems =
    line_start $ vbox 0 (fmt_nestable_block_elements c elems)
  in
  vbox 0 (list items "@," fmt_item)

and fmt_nestable_block_elements c ?(prefix = noop) = function
  | [] -> noop
  | elems -> prefix $ list_block_elem elems (fmt_nestable_block_element c)

let at = char '@'

let space = fmt "@ "

let fmt_tag_see c wrap sr txt =
  at $ fmt "see@ "
  $ wrap (str_normalized sr)
  $ fmt_nestable_block_elements c ~prefix:space txt

let fmt_tag c = function
  | `Author s -> at $ fmt "author@ " $ str_normalized s
  | `Version s -> at $ fmt "version@ " $ str_normalized s
  | `See (`Url, sr, txt) -> fmt_tag_see c (wrap "<" ">") sr txt
  | `See (`File, sr, txt) -> fmt_tag_see c (wrap "'" "'") sr txt
  | `See (`Document, sr, txt) -> fmt_tag_see c (wrap "\"" "\"") sr txt
  | `Since s -> at $ fmt "since@ " $ str_normalized s
  | `Before (s, txt) ->
      at $ fmt "before@ " $ str_normalized s
      $ fmt_nestable_block_elements c ~prefix:space txt
  | `Deprecated txt ->
      at $ fmt "deprecated"
      $ fmt_nestable_block_elements c ~prefix:space txt
  | `Param (s, txt) ->
      at $ fmt "param@ " $ str_normalized s
      $ fmt_nestable_block_elements c ~prefix:space txt
  | `Raise (s, txt) ->
      at $ fmt "raise@ " $ str_normalized s
      $ fmt_nestable_block_elements c ~prefix:space txt
  | `Return txt ->
      at $ fmt "return" $ fmt_nestable_block_elements c ~prefix:space txt
  | `Inline -> at $ str "inline"
  | `Open -> at $ str "open"
  | `Closed -> at $ str "closed"
  | `Canonical ref -> at $ fmt "canonical@ " $ fmt_reference ref

let fmt_block_element c = function
  | `Tag tag -> hovbox 0 (fmt_tag c tag)
  | `Heading (lvl, lbl, elems) ->
      let lvl = Int.to_string lvl in
      let lbl =
        match lbl with
        | Some lbl -> str ":" $ str_normalized lbl
        | None -> fmt ""
      in
      hovbox 0
        (wrap "{" "}" (str lvl $ lbl $ fmt "@ " $ fmt_inline_elements elems))
  | #nestable_block_element as elm ->
      hovbox 0 (fmt_nestable_block_element c elm)

let fmt conf ~fmt_code (docs : docs) =
  let c = {conf; fmt_code} in
  vbox 0 (list_block_elem docs (fmt_block_element c))

let diff c x y =
  let norm z =
    let f Cmt.{txt; _} = Normalize.docstring c txt in
    Set.of_list (module String) (List.map ~f z)
  in
  Set.symmetric_diff (norm x) (norm y)

let is_tag_only =
  List.for_all ~f:(function
    | {Location_.value= `Tag _; _} -> true
    | _ -> false)
