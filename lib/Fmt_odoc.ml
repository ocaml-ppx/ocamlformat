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

type fmt_code = Conf.t -> offset:int -> string -> string or_error

type c = {fmt_code: fmt_code; conf: Conf.t}

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

(** Insert [ins] into [s] at every indexes in [ats]. *)
let insert_ats s ins ats =
  let len = String.length s in
  let b = Buffer.create (len + (String.length ins * List.length ats)) in
  let stash pos until = Buffer.add_substring b s ~pos ~len:(until - pos) in
  let rec loop last_ins = function
    | [] -> stash last_ins len
    | i :: tl -> stash last_ins i ; Buffer.add_string b ins ; loop i tl
  in
  loop 0 (List.sort ~compare:Int.compare ats) ;
  Buffer.contents b

let escape_balanced_brackets s =
  (* Do not escape paired brackets. Opening and closing that couldn't be
     paired will be escaped. *)
  let rec brackets_to_escape opens closes i =
    if i >= String.length s then opens @ closes
    else
      let opens, closes =
        match s.[i] with
        | '[' -> (i :: opens, closes)
        | ']' -> (
          match opens with
          | [] -> (opens, i :: closes)
          | _ :: tl -> (tl, closes) )
        | _ -> (opens, closes)
      in
      brackets_to_escape opens closes (i + 1)
  in
  insert_ats s "\\" (brackets_to_escape [] [] 0)

let escape_all s =
  let escapeworthy = function '{' | '}' | '[' | ']' -> true | _ -> false in
  ensure_escape ~escapeworthy s

let split_on_whitespaces =
  String.split_on_chars ~on:['\t'; '\n'; '\011'; '\012'; '\r'; ' ']

(** Escape special characters and normalize whitespaces *)
let str_normalized ?(escape = escape_all) c s =
  if c.conf.fmt_opts.wrap_docstrings.v then
    split_on_whitespaces s
    |> List.filter ~f:(Fn.non String.is_empty)
    |> fun s -> list s "@ " (fun s -> escape s |> str)
  else str (escape s)

let ign_loc ~f with_loc = f with_loc.Loc.value

let fmt_verbatim_block ~loc s =
  let force_break = loc.Loc.start.line < loc.end_.line in
  let content =
    (* Literal newline to avoid indentation *)
    if force_break then wrap "\n" "@\n" (str s)
    else fits_breaks " " "\n" $ str s $ fits_breaks " " ~hint:(0, 0) ""
  in
  hvbox 0 (wrap "{v" "v}" content)

let fmt_metadata (lang, meta) =
  let fmt_meta meta = str " " $ str meta in
  str "@" $ ign_loc ~f:str lang $ opt meta (ign_loc ~f:fmt_meta)

let fmt_code_block c s1 s2 =
  let wrap_code x =
    str "{" $ opt s1 fmt_metadata $ fmt "[@;<1000 2>" $ x $ fmt "@ ]}"
  in
  let fmt_line ~first ~last:_ l =
    let l = String.rstrip l in
    if first then str l
    else if String.length l = 0 then str "\n"
    else fmt "@," $ str l
  in
  let fmt_code s =
    let lines = String.split_lines s in
    let box = match lines with _ :: _ :: _ -> vbox 0 | _ -> hvbox 0 in
    box (wrap_code (vbox 0 (list_fl lines fmt_line)))
  in
  let Odoc_parser.Loc.{location; value= original} = s2 in
  match s1 with
  | Some ({value= "ocaml"; _}, _) | None -> (
    (* [offset] doesn't take into account code blocks nested into lists. *)
    match c.fmt_code c.conf ~offset:2 original with
    | Ok formatted -> fmt_code formatted
    | Error (`Msg message) ->
        ( match message with
        | "" -> ()
        | _ when Option.is_none s1 -> ()
        | _ ->
            if not c.conf.opr_opts.quiet.v then
              Docstring.warn Stdlib.Format.err_formatter
                { location
                ; message= Format.sprintf "invalid code block: %s" message }
        ) ;
        fmt_code original )
  | Some _ -> fmt_code original

let fmt_code_span s = wrap "[" "]" (str (escape_balanced_brackets s))

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

let fmt_reference = ign_loc ~f:str

(* Decide between using light and heavy syntax for lists *)
let list_should_use_heavy_syntax items =
  let heavy_nestable_block_elements = function
    (* More than one element or contains a list *)
    | [{Loc.value= `List _; _}] | _ :: _ :: _ -> true
    | [] | [_] -> false
  in
  List.exists items ~f:heavy_nestable_block_elements

(* Decide if a blank line should be added between two elements *)
let block_element_should_blank elem next =
  match (elem, next) with
  | `Tag _, `Tag _ -> false
  (* Mandatory blanks lines. *)
  | (`List _ | `Tag _), _ | `Paragraph _, `Paragraph _ -> true
  | _, _ -> false

let should_preserve_blank _c (a : Loc.span) (b : Loc.span) =
  (* Whether there were already an empty line *)
  b.start.line - a.end_.line > 1

(* Format a list of block_elements separated by newlines Inserts blank line
   depending on [block_element_should_blank] *)
let list_block_elem c elems f =
  list_pn elems (fun ~prev:_ elem ~next ->
      let break =
        match next with
        | Some n ->
            if
              block_element_should_blank
                (elem.Loc.value :> block_element)
                (n.value :> block_element)
              || should_preserve_blank c elem.location n.location
            then fmt "\n@\n"
            else fmt "@\n"
        | None -> noop
      in
      f elem $ break )

let space_elt c : inline_element with_location =
  let sp = if c.conf.fmt_opts.wrap_docstrings.v then "" else " " in
  Loc.(at (span []) (`Space sp))

let non_wrap_space sp = if String.contains sp '\n' then fmt "@\n" else str sp

let rec fmt_inline_elements c elements =
  let wrap_elements opn cls ~always_wrap hd = function
    | [] -> wrap_if always_wrap opn cls hd
    | tl -> wrap opn cls (hd $ fmt_inline_elements c (space_elt c :: tl))
  in
  let rec aux = function
    | [] -> noop
    | `Space sp :: `Word (("-" | "+") as w) :: t ->
        (* Escape lines starting with '+' or '-'. *)
        fmt_or_k c.conf.fmt_opts.wrap_docstrings.v
          (cbreak ~fits:("", 1, "") ~breaks:("", 0, "\\"))
          (non_wrap_space sp)
        $ str w $ aux t
    | `Space sp :: t ->
        fmt_or_k c.conf.fmt_opts.wrap_docstrings.v (fmt "@ ")
          (non_wrap_space sp)
        $ aux t
    | `Word w :: t ->
        fmt_if (String.is_prefix ~prefix:"@" w) "\\"
        $ str_normalized c w $ aux t
    | `Code_span s :: t -> fmt_code_span s $ aux t
    | `Math_span s :: t -> fmt_math_span s $ aux t
    | `Raw_markup (lang, s) :: t ->
        let lang =
          match lang with
          | Some l -> str_normalized c l $ str ":"
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
        hovbox_if c.conf.fmt_opts.wrap_docstrings.v
          (1 + String.length s + 1)
          (wrap_elements "{" "}" ~always_wrap:true (str_normalized c s) elems)
        $ aux t
    | `Reference (_kind, rf, txt) :: t ->
        let rf = wrap "{!" "}" (fmt_reference rf) in
        wrap_elements "{" "}" ~always_wrap:false rf txt $ aux t
    | `Link (url, txt) :: t ->
        let url = wrap "{:" "}" (str_normalized c url) in
        hovbox_if c.conf.fmt_opts.wrap_docstrings.v 2
        @@ wrap_elements "{" "}" ~always_wrap:false url txt
        $ aux t
  in
  aux (List.map elements ~f:(ign_loc ~f:Fn.id))

and fmt_nestable_block_element c elm =
  match elm.Loc.value with
  | `Paragraph elems -> fmt_inline_elements c elems
  | `Code_block (s1, s2) -> fmt_code_block c s1 s2
  | `Math_block s -> fmt_math_block s
  | `Verbatim s -> fmt_verbatim_block ~loc:elm.location s
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
  list_block_elem c elems (fmt_nestable_block_element c)

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

let fmt_block_element c elm =
  match elm.Loc.value with
  | `Tag tag -> hovbox 2 (fmt_tag c tag)
  | `Heading (lvl, lbl, elems) ->
      let lvl = Int.to_string lvl in
      let lbl =
        match lbl with
        | Some lbl -> str ":" $ str_normalized c lbl
        | None -> noop
      in
      let elems =
        if List.is_empty elems then elems else space_elt c :: elems
      in
      hovbox 0 (wrap "{" "}" (str lvl $ lbl $ fmt_inline_elements c elems))
  | #nestable_block_element as value ->
      hovbox 0 (fmt_nestable_block_element c {elm with value})

let fmt_ast conf ~fmt_code (docs : t) =
  let c = {fmt_code; conf} in
  vbox 0 (list_block_elem c docs (fmt_block_element c))

let beginning_offset (conf : Conf.t) input =
  let whitespace_count =
    match String.indent_of_line input with Some c -> c | None -> 1
  in
  if conf.fmt_opts.ocp_indent_compat.v && not conf.fmt_opts.wrap_docstrings.v
  then
    (* Preserve offset of the first line and indent the whole comment based
       on that. *)
    whitespace_count
  else min whitespace_count 1

let fmt_parsed (conf : Conf.t) ~fmt_code ~input ~offset parsed =
  let open Fmt in
  let begin_offset = beginning_offset conf input in
  (* The offset is used to adjust the margin when formatting code blocks. *)
  let offset = offset + begin_offset in
  let fmt_code conf ~offset:offset' input =
    fmt_code conf ~offset:(offset + offset') input
  in
  let fmt_parsed parsed =
    str (String.make begin_offset ' ')
    $ fmt_ast conf ~fmt_code parsed
    $ fmt_if
        (String.length input > 1 && String.ends_with_whitespace input)
        " "
  in
  match parsed with
  | _ when not conf.fmt_opts.parse_docstrings.v -> str input
  | Ok parsed -> fmt_parsed parsed
  | Error msgs ->
      if not conf.opr_opts.quiet.v then
        List.iter msgs ~f:(Docstring.warn Format.err_formatter) ;
      str input
