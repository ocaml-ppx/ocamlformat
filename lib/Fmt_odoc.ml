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
open Ocamlformat_odoc_parser.Ast
module Loc = Ocamlformat_odoc_parser.Loc

type fmt_code =
     Conf.t
  -> offset:int
  -> set_margin:bool
  -> string
  -> (Fmt.t, [`Msg of string]) Result.t

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
let str_normalized ?(escape = escape_all) ~wrap s =
  if wrap then
    split_on_whitespaces s
    |> List.filter ~f:(Fn.non String.is_empty)
    |> fun s -> list s space_break (fun s -> escape s |> str)
  else str (escape s)

let rec drop_leading_spaces = function
  | {Loc.value= `Space _; _} :: tl -> drop_leading_spaces tl
  | elems -> elems

let ign_loc ~f with_loc = f with_loc.Loc.value

let fmt_verbatim_block ~loc s =
  let force_break = loc.Loc.start.line < loc.end_.line in
  let content =
    (* Literal newline to avoid indentation *)
    if force_break then wrap (str "\n") force_newline (str s)
    else fits_breaks " " "\n" $ str s $ fits_breaks " " ~hint:(0, 0) ""
  in
  hvbox 0 (wrap (str "{v") (str "v}") content)

let fmt_code_span ~wrap s =
  let s = escape_balanced_brackets s in
  let s =
    if wrap then
      let words = String.split_on_chars ~on:[' '] s in
      list words space_break str
    else str s
  in
  hovbox_if wrap 1 (str "[" $ s $ str "]")

let fmt_math_span s = hovbox 2 (wrap (str "{m ") (str "}") (str s))

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
    else break 1000 0 $ str line
  in
  hvbox 2
    (wrap
       (str "{math" $ space_break)
       (break 0 (-2) $ str "}")
       (list_fl lines fmt) )

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
            then str "\n" $ force_break
            else force_break
        | None -> noop
      in
      f elem $ break )

module Light_table = struct
  (** A table type that can safely be formatted using the light syntax. *)

  type cell = inline_element with_location list

  type row = cell list

  (** [header_rows, alignments, data_rows] *)
  type t = row list * alignment option list option * row list

  module Ast = Ocamlformat_odoc_parser.Ast

  (** Returns [None] if the given table cannot be safely formatted using the
      light syntax. This might return [None] for tables that were using the
      light syntax in the original source. *)
  let of_table : Ast.table -> t option =
    let exception Table_not_safe in
    let extract_cell ((elems, _header) : _ Ast.cell) =
      match elems with
      | [] -> []
      | [{value= `Paragraph inline_elems; _}] -> inline_elems
      | _ -> raise Table_not_safe
    in
    let extract_row (row : _ Ast.row) = List.map ~f:extract_cell row in
    let is_header_cell (_, h) =
      match h with `Header -> true | `Data -> false
    in
    let rec extract header : _ Ast.grid -> _ = function
      | hd :: tl when List.exists hd ~f:is_header_cell ->
          extract (extract_row hd :: header) tl
      | data_rows -> (List.rev header, List.map ~f:extract_row data_rows)
    in
    function
    | _, `Heavy -> None
    | (grid, alignments), `Light -> (
      try
        let header, data = extract [] grid in
        Some (header, alignments, data)
      with Table_not_safe -> None )

  let alignment_chars = function
    | Some `Left -> (":", "-")
    | Some `Center -> (":", ":")
    | Some `Right -> ("-", ":")
    | None -> ("-", "-")
end

let non_wrap_space sp =
  if String.contains sp '\n' then force_newline else str sp

let fmt_block_markup ?force_break:(fb = false) tag content =
  let initial_break = if fb then force_break else space_break in
  hvbox 2
    (str "{" $ str tag $ initial_break $ content $ break 1 ~-2 $ str "}")

let rec fmt_inline_elements c ~wrap elements =
  let rec aux = function
    | [] -> noop
    | `Space sp :: `Word (("-" | "+") as w) :: t ->
        (* Escape lines starting with '+' or '-'. *)
        fmt_or wrap
          (cbreak ~fits:("", 1, "") ~breaks:("", 0, "\\"))
          (non_wrap_space sp)
        $ str w $ aux t
    | `Space sp :: t -> fmt_or wrap space_break (non_wrap_space sp) $ aux t
    | `Word w :: t ->
        fmt_if (String.is_prefix ~prefix:"@" w) (str "\\")
        $ str_normalized ~wrap w $ aux t
    | `Code_span s :: t -> fmt_code_span ~wrap s $ aux t
    | `Math_span s :: t -> fmt_math_span s $ aux t
    | `Raw_markup (lang, s) :: t ->
        let lang =
          match lang with
          | Some l -> str_normalized ~wrap l $ str ":"
          | None -> noop
        in
        (* todo check this was an escape sequence *)
        str "{%" $ (lang $ str s) $ str "%}" $ aux t
    | `Styled (style, elems) :: t ->
        let s =
          match style with
          | `Bold -> "b"
          | `Italic -> "i"
          | `Emphasis -> "e"
          | `Superscript -> "^"
          | `Subscript -> "_"
        in
        hovbox_if wrap
          (1 + String.length s + 1)
          (fmt_markup_with_inline_elements c ~wrap ~force_space:true
             (str_normalized ~wrap s) elems )
        $ aux t
    | `Reference (_kind, rf, txt) :: t ->
        let rf = str "{!" $ fmt_reference rf $ str "}" in
        fmt_link_or_reference c ~wrap rf txt $ aux t
    | `Link (url, txt) :: t ->
        let url = str "{:" $ str_normalized ~wrap url $ str "}" in
        fmt_link_or_reference c ~wrap url txt $ aux t
  in
  aux (List.map elements ~f:(ign_loc ~f:Fn.id))

and fmt_link_or_reference c ~wrap tag txt =
  match txt with
  | [] -> tag
  | _ :: _ ->
      hovbox_if wrap 1 (fmt_markup_with_inline_elements c ~wrap tag txt)

(** Format a markup of the form [{tag elems}]. If [force_space] is [true], a
    space will be added after the tag, even if it's not present in the source.
*)
and fmt_markup_with_inline_elements c ~wrap ?(force_space = false) tag elems
    =
  let leading_space, elems =
    if force_space then (str " ", drop_leading_spaces elems)
    else (noop, elems)
  in
  str "{" $ tag $ leading_space $ fmt_inline_elements c ~wrap elems $ str "}"

and fmt_nestable_block_element c (elm : nestable_block_element with_location)
    =
  match elm.Loc.value with
  | `Paragraph elems ->
      hovbox 0
        (fmt_inline_elements c ~wrap:c.conf.fmt_opts.wrap_docstrings.v elems)
  | `Code_block code_block -> fmt_code_block c code_block
  | `Math_block s -> fmt_math_block s
  | `Verbatim s -> fmt_verbatim_block ~loc:elm.location s
  | `Modules mods ->
      hovbox 0
        (wrap
           (str "{!modules:" $ cut_break)
           (cut_break $ str "}")
           (list mods space_break (fun ref -> fmt_reference ref)) )
  | `List (k, _syntax, items) when list_should_use_heavy_syntax items ->
      fmt_list_heavy c k items
  | `List (k, _syntax, items) -> fmt_list_light c k items
  | `Table table -> fmt_table c table
  | `Media (_kind, href, text, media) -> (
      let prefix =
        match media with
        | `Image -> "image"
        | `Video -> "video"
        | `Audio -> "audio"
      in
      let href =
        match href.value with
        | `Reference s -> str "!" $ str s
        | `Link s -> str ":" $ str s
      in
      let ref = str "{" $ str prefix $ href $ str "}" in
      match text with "" -> ref | _ -> str "{" $ ref $ str text $ str "}" )

and fmt_list_heavy c kind items =
  let fmt_item elems =
    let box = match elems with [_] -> hvbox 3 | _ -> vbox 3 in
    box
      (wrap (str "{- ")
         (break 1 (-3) $ str "}")
         (fmt_nestable_block_elements c elems) )
  and start =
    match kind with
    | `Unordered -> str "{ul" $ cut_break
    | `Ordered -> str "{ol" $ cut_break
  in
  vbox 1
    (wrap start (break 1 (-1) $ str "}") (list items cut_break fmt_item))

and fmt_list_light c kind items =
  let line_start =
    match kind with `Unordered -> str "- " | `Ordered -> str "+ "
  in
  let fmt_item elems =
    line_start $ hovbox 0 (fmt_nestable_block_elements c elems)
  in
  vbox 0 (list items force_break fmt_item)

and fmt_table_heavy c (((grid, alignments), _) : table) =
  let fmt_cell (elems, header) =
    let cell_tag = match header with `Header -> "th" | `Data -> "td" in
    fmt_block_markup cell_tag (fmt_nestable_block_elements c elems)
  in
  let fmt_row row = fmt_block_markup "tr" (list row space_break fmt_cell) in
  ignore alignments ;
  fmt_block_markup "table" (list grid force_break fmt_row)

and fmt_table_light c (header, alignments, data) =
  (* Format every cells into strings to then compute the width of columns. *)
  let format_rows_to_strings =
    let format_cell_to_string elems =
      Format_.asprintf " %a " Fmt.eval
      @@ fmt_inline_elements c ~wrap:false elems
    in
    List.map ~f:(List.map ~f:format_cell_to_string)
  in
  let header = format_rows_to_strings header
  and data = format_rows_to_strings data in
  let column_width =
    let column_count =
      let f acc row = max acc (List.length row) in
      let compute init rows = List.fold_left rows ~init ~f in
      let aligns_count =
        Option.value_map alignments ~default:0 ~f:List.length
      in
      compute (compute aligns_count header) data
    in
    let column_min_width = if Option.is_some alignments then 3 else 1 in
    let widths = Array.init column_count ~f:(fun _ -> column_min_width) in
    let compute_column_widths row =
      List.iteri row ~f:(fun i cell ->
          widths.(i) <- max widths.(i) (Fmt.str_length cell) )
    in
    List.iter ~f:compute_column_widths header ;
    List.iter ~f:compute_column_widths data ;
    Array.get widths
  in
  let align_row, align_of_column =
    let align_column i align =
      let l, r = Light_table.alignment_chars align in
      l ^ String.make (column_width i - 2) '-' ^ r
    in
    match alignments with
    | Some aligns ->
        let aligns_ar = Array.of_list aligns in
        let aligns_get i =
          if i >= Array.length aligns_ar then `Left
          else Option.value ~default:`Left aligns_ar.(i)
        in
        ([List.mapi ~f:align_column aligns], aligns_get)
    | None -> ([], fun _ -> `Left)
  in
  let padding n = str (String.make n ' ') in
  let fmt_cell i s =
    let pad = column_width i - Fmt.str_length s in
    let l, r =
      if pad <= 0 then (noop, noop)
      else
        match align_of_column i with
        | `Left -> (noop, padding pad)
        | `Center -> (padding (pad / 2), padding ((pad + 1) / 2))
        | `Right -> (padding pad, noop)
    in
    l $ str s $ r
  in
  let fmt_row row =
    let row = List.mapi row ~f:fmt_cell in
    str "|" $ list row (str "|") Fn.id $ str "|"
  in
  fmt_block_markup ~force_break:true "t"
    (vbox 0 (list (header @ align_row @ data) force_break fmt_row))

and fmt_table c table =
  match Light_table.of_table table with
  | Some light -> fmt_table_light c light
  | None -> fmt_table_heavy c table

and fmt_code_block c (b : code_block) =
  let content =
    let content = b.content.value in
    match b.meta with
    | Some {language= {value= "ocaml"; _}; _} | None -> (
      (* [offset] doesn't take into account code blocks nested into lists. *)
      match c.fmt_code c.conf ~offset:2 ~set_margin:true content with
      | Ok formatted -> formatted |> Format_.asprintf "%a" Fmt.eval
      | Error (`Msg message) ->
          if
            (not (String.is_empty message))
            && Option.is_some b.meta
            && not c.conf.opr_opts.quiet.v
          then
            Docstring.warn Stdlib.Format.err_formatter
              { location= b.content.location
              ; message= Format.sprintf "invalid code block: %s" message } ;
          content )
    | Some _ -> content
  in
  let fmt_line ~first ~last:_ l =
    let l = String.rstrip l in
    if first then str l
    else if String.length l = 0 then str_as 0 "\n"
    else force_break $ str l
  in
  let fmt_code s =
    let lines = String.split_lines s in
    vbox 0 (list_fl lines fmt_line)
  in
  let delim = opt b.delimiter str in
  let opening =
    let meta =
      opt b.meta (fun meta ->
          str "@"
          $ ign_loc ~f:str meta.language
          $ opt meta.tags (fun tags -> str " " $ ign_loc ~f:str tags) )
    in
    str "{" $ delim $ meta $ str "["
  in
  let output_or_closing =
    match b.output with
    | Some elems ->
        hvbox 2
          ( str "]" $ delim $ str "[" $ force_break
          $ fmt_nestable_block_elements c elems
          $ fmt_if (not (List.is_empty elems)) (break 1000 ~-2)
          $ str "]}" )
    | None -> str "]" $ delim $ str "}"
  in
  hvbox 2
    ( opening $ force_break $ fmt_code content $ break 1 ~-2
    $ output_or_closing )

and fmt_nestable_block_elements c elems =
  list_block_elem c elems (fmt_nestable_block_element c)

let at = char '@'

let fmt_tag_args ?arg ?txt c tag =
  at $ str tag
  $ opt arg (fun x -> char ' ' $ x)
  $ opt txt (function
      | [] -> noop
      | x -> space_break $ hovbox 0 (fmt_nestable_block_elements c x) )

let wrap_see = function
  | `Url -> wrap (str "<") (str ">")
  | `File -> wrap (str "'") (str "'")
  | `Document -> wrap (str "\"") (str "\"")

let fmt_tag c : tag -> _ = function
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
  | `Hidden -> fmt_tag_args c "hidden"
  | `Canonical ref -> fmt_tag_args c "canonical" ~arg:(fmt_reference ref)
  | `Children_order txt -> fmt_tag_args c "children_order" ~txt
  | `Short_title txt -> fmt_tag_args c "short_title" ~txt

let fmt_block_element c elm =
  match elm.Loc.value with
  | `Tag tag -> hovbox 2 (fmt_tag c tag)
  | `Heading (lvl, lbl, elems) ->
      let lvl = Int.to_string lvl in
      let lbl =
        match lbl with
        | Some lbl -> str ":" $ str_normalized ~wrap:false lbl
        | None -> noop
      in
      let tag = str lvl $ lbl in
      hovbox 0
        (fmt_markup_with_inline_elements c
           ~wrap:c.conf.fmt_opts.wrap_docstrings.v ~force_space:true tag
           elems )
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

let fmt_parsed (conf : Conf.t) ?trailing_space ~fmt_code ~input ~offset
    parsed =
  let open Fmt in
  let begin_offset = beginning_offset conf input in
  (* The offset is used to adjust the margin when formatting code blocks. *)
  let offset = offset + begin_offset in
  let fmt_code conf ~offset:offset' ~set_margin input =
    fmt_code conf ~offset:(offset + offset') ~set_margin input
  in
  let trailing_space =
    match trailing_space with
    | Some sp -> sp
    | None ->
        fmt_if
          (String.length input > 1 && String.ends_with_whitespace input)
          space_break
  in
  let fmt_parsed parsed =
    str (String.make begin_offset ' ')
    $ fmt_ast conf ~fmt_code parsed
    $ trailing_space
  in
  match parsed with
  | _ when not conf.fmt_opts.parse_docstrings.v -> str input
  | Ok parsed -> fmt_parsed parsed
  | Error msgs ->
      if not conf.opr_opts.quiet.v then
        List.iter msgs ~f:(Docstring.warn Format.err_formatter) ;
      str input
