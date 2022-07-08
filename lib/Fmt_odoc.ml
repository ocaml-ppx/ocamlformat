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

open Fmt
open Odoc_parser.Ast
module Loc = Odoc_parser.Loc

type conf = {fmt_code: Fmt.code_formatter}

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

let ign_loc ~f with_loc = f with_loc.Loc.value

let fmt_verbatim_block s =
  let force_break = String.contains s '\n' in
  let content =
    (* Literal newline to avoid indentation *)
    if force_break then wrap "\n" "@\n" (str s)
    else fits_breaks " " "\n" $ str s $ fits_breaks " " ~hint:(0, 0) ""
  in
  hvbox 0 (wrap "{v" "v}" content)

let fmt_metadata (lang, meta) =
  let fmt_meta meta = str " " $ str meta in
  str "@" $ ign_loc ~f:str lang $ opt meta (ign_loc ~f:fmt_meta)

let fmt_code_block conf s1 s2 =
  let wrap_code x =
    str "{" $ opt s1 fmt_metadata $ fmt "[@;<1000 2>" $ x $ fmt "@ ]}"
  in
  let fmt_line ~first ~last:_ l =
    let l = String.rstrip l in
    if first then str l
    else if String.length l = 0 then str "\n"
    else fmt "@," $ str l
  in
  let fmt_no_code s =
    let lines = String.split_lines s in
    let box = match lines with _ :: _ :: _ -> vbox 0 | _ -> hvbox 0 in
    box (wrap_code (vbox 0 (list_fl lines fmt_line)))
  in
  let Odoc_parser.Loc.{location; value} = s2 in
  match s1 with
  | Some ({value= "ocaml"; _}, _) | None -> (
    match conf.fmt_code value with
    | Ok formatted -> hvbox 0 (wrap_code formatted)
    | Error (`Msg message) ->
        ( match message with
        | "" -> ()
        | _ ->
            Docstring.warn Caml.Format.err_formatter
              { location
              ; message= Format.sprintf "invalid code block: %s" message } ) ;
        fmt_no_code value )
  | Some _ -> fmt_no_code value

let fmt_code_span s = hovbox 0 (wrap "[" "]" (str (escape_brackets s)))

let fmt_math_span s = hovbox 2 (wrap "{m " "}" (str s))

let fmt_math_block s =
  let lines =
    List.drop_while ~f:String.is_empty
    @@ List.rev
    @@ List.drop_while ~f:String.is_empty
    @@ List.rev_map ~f:String.strip
    @@ String.split_lines s
  in
  let fmt ~first ~last:_ line =
    if first then str line
    else if String.is_empty line then str "\n"
    else fmt "@;<1000 0>" $ str line
  in
  hvbox 2 (wrap "{math@;" "@;<0 -2>}" (list_fl lines fmt))

let fmt_reference = ign_loc ~f:str_normalized

(* Decide between using light and heavy syntax for lists *)
let list_should_use_heavy_syntax items =
  let heavy_nestable_block_elements = function
    (* More than one element or contains a list *)
    | [{Loc.value= `List _; _}] | _ :: _ :: _ -> true
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
  list_pn elems (fun ~prev:_ elem ~next ->
      let elem = elem.Loc.value in
      let break =
        match next with
        | Some {Loc.value= n; _}
          when block_element_should_break
                 (elem :> block_element)
                 (n :> block_element) ->
            fmt "\n@\n"
        | Some _ -> fmt "@\n"
        | None -> noop
      in
      f elem $ break )

let space_elt : inline_element with_location = Loc.(at (span []) (`Space ""))

let rec fmt_inline_elements elements =
  let wrap_elements opn cls ~always_wrap hd = function
    | [] -> wrap_if always_wrap opn cls hd
    | tl -> wrap opn cls (hd $ fmt_inline_elements (space_elt :: tl))
  in
  let rec aux = function
    | [] -> noop
    | `Space _ :: `Word w :: t ->
        (* Escape lines starting with '+' or '-' *)
        let escape =
          if String.length w > 0 && Char.(w.[0] = '+' || w.[0] = '-') then
            "\\"
          else ""
        in
        cbreak ~fits:("", 1, "") ~breaks:("", 0, escape)
        $ str_normalized w $ aux t
    | `Space _ :: t -> fmt "@ " $ aux t
    | `Word w :: t -> str_normalized w $ aux t
    | `Code_span s :: t -> fmt_code_span s $ aux t
    | `Math_span s :: t -> fmt_math_span s $ aux t
    | `Raw_markup (lang, s) :: t ->
        let lang =
          match lang with
          | Some l -> str_normalized l $ str ":"
          | None -> noop
        in
        wrap "{%%" "%%}" (lang $ str s) $ aux t
    | `Styled (style, elems) :: t ->
        let s =
          match style with
          | `Bold -> "b"
          | `Italic -> "i"
          | `Emphasis -> "e"
          | `Superscript -> "^"
          | `Subscript -> "_"
        in
        hovbox
          (1 + String.length s + 1)
          (wrap_elements "{" "}" ~always_wrap:true (str_normalized s) elems)
        $ aux t
    | `Reference (_kind, rf, txt) :: t ->
        let rf = wrap "{!" "}" (fmt_reference rf) in
        wrap_elements "{" "}" ~always_wrap:false rf txt $ aux t
    | `Link (url, txt) :: t ->
        let url = wrap "{:" "}" (str_normalized url) in
        wrap_elements "{" "}" ~always_wrap:false url txt $ aux t
  in
  aux (List.map elements ~f:(ign_loc ~f:Fn.id))

and fmt_nestable_block_element c = function
  | `Paragraph elems -> fmt_inline_elements elems
  | `Code_block (s1, s2) -> fmt_code_block c s1 s2
  | `Math_block s -> fmt_math_block s
  | `Verbatim s -> fmt_verbatim_block s
  | `Modules mods ->
      hovbox 0
        (wrap "{!modules:@," "@,}"
           (list mods "@ " (fun ref -> fmt_reference ref)) )
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
    line_start $ hovbox 0 (fmt_nestable_block_elements c elems)
  in
  vbox 0 (list items "@," fmt_item)

and fmt_nestable_block_elements c elems =
  list_block_elem elems (fmt_nestable_block_element c)

let at = char '@'

let space = fmt "@ "

let fmt_tag_args ?arg ?txt c tag =
  at $ str tag
  $ opt arg (fun x -> char ' ' $ x)
  $ opt txt (function
      | [] -> noop
      | x -> space $ hovbox 0 (fmt_nestable_block_elements c x) )

let wrap_see = function
  | `Url -> wrap "<" ">"
  | `File -> wrap "'" "'"
  | `Document -> wrap "\"" "\""

let fmt_tag c = function
  | `Author s -> fmt_tag_args c "author" ~arg:(str s)
  | `Version s -> fmt_tag_args c "version" ~arg:(str s)
  | `See (k, sr, txt) -> fmt_tag_args c "see" ~arg:(wrap_see k (str sr)) ~txt
  | `Since s -> fmt_tag_args c "since" ~arg:(str s)
  | `Before (s, txt) -> fmt_tag_args c "before" ~arg:(str s) ~txt
  | `Deprecated txt -> fmt_tag_args c "deprecated" ~txt
  | `Param (s, txt) -> fmt_tag_args c "param" ~arg:(str s) ~txt
  | `Raise (s, txt) -> fmt_tag_args c "raise" ~arg:(str s) ~txt
  | `Return txt -> fmt_tag_args c "return" ~txt
  | `Inline -> fmt_tag_args c "inline"
  | `Open -> fmt_tag_args c "open"
  | `Closed -> fmt_tag_args c "closed"
  | `Canonical ref -> fmt_tag_args c "canonical" ~arg:(fmt_reference ref)

let fmt_block_element c = function
  | `Tag tag -> hovbox 2 (fmt_tag c tag)
  | `Heading (lvl, lbl, elems) ->
      let lvl = Int.to_string lvl in
      let lbl =
        match lbl with
        | Some lbl -> str ":" $ str_normalized lbl
        | None -> noop
      in
      let elems =
        if List.is_empty elems then elems else space_elt :: elems
      in
      hovbox 0 (wrap "{" "}" (str lvl $ lbl $ fmt_inline_elements elems))
  | #nestable_block_element as elm ->
      hovbox 0 (fmt_nestable_block_element c elm)

let fmt ~fmt_code (docs : t) =
  vbox 0 (list_block_elem docs (fmt_block_element {fmt_code}))
