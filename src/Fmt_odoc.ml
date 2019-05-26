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

open Octavius.Types
open Fmt

let escape_brackets =
  String.Escaping.escape ~escapeworthy:['['; ']'] ~escape_char:'\\'

let escape_all =
  String.Escaping.escape ~escapeworthy:['@'; '{'; '}'; '['; ']']
    ~escape_char:'\\'

let str ?(escape = true) s =
  let escape = if escape then Staged.unstage escape_all else Fn.id in
  s
  |> String.split_on_chars ~on:['\t'; '\n'; '\011'; '\012'; '\r'; ' ']
  |> List.filter ~f:(Fn.non String.is_empty)
  |> fun s -> list s "@ " (fun s -> escape s |> str)

let verbatim s = Fmt.str s

let fmt_ref_kind = function
  | RK_element -> str "!"
  | RK_module -> str "!module:"
  | RK_module_type -> str "!modtype:"
  | RK_class -> str "!class:"
  | RK_class_type -> str "!classtype:"
  | RK_value -> str "!val:"
  | RK_type -> str "!type:"
  | RK_exception -> str "!exception:"
  | RK_attribute -> str "!attribute:"
  | RK_method -> str "!method:"
  | RK_section -> str "!section:"
  | RK_recfield -> str "!recfield:"
  | RK_const -> str "!const:"
  | RK_link -> str ":"
  | RK_custom s -> str "!" $ str s $ str ":"

let rec fmt_style style txt =
  let s =
    match style with
    | SK_bold -> "b"
    | SK_italic -> "i"
    | SK_emphasize -> "e"
    | SK_center -> "C"
    | SK_left -> "L"
    | SK_right -> "R"
    | SK_superscript -> "^"
    | SK_subscript -> "_"
    | SK_custom s -> s
  in
  hovbox 0
    (wrap "{" "}"
       (Fmt.str s $ fmt_if (not (List.is_empty txt)) "@ " $ fmt_text txt))

and fmt_text_elt = function
  | Raw s -> str s
  | Code s ->
      hovbox 0
        (wrap "[" "]"
           (str ~escape:false ((Staged.unstage escape_brackets) s)))
  | PreCode s -> hovbox 0 (wrap "{[\n" "@\n]}" (hovbox 0 (verbatim s)))
  | Verbatim s -> hovbox 0 (wrap "{v\n" "@\nv}" (hovbox 0 (verbatim s)))
  | Style (st, txt) -> fmt_style st txt
  | List l -> fmt_list `List l
  | Enum l -> fmt_list `Enum l
  | Newline -> fmt "\n@\n"
  | Title (i, None, txt) ->
      hovbox 0
        (wrap "{" "}" (str (Int.to_string i) $ fmt "@ " $ fmt_text txt))
  | Title (i, Some s, txt) ->
      hovbox 0
        (wrap "{" "}"
           ( str (Int.to_string i)
           $ str ":" $ str s $ fmt "@ " $ fmt_text txt ))
  | Ref (rk, s, None) -> hovbox 0 (wrap "{" "}" (fmt_ref_kind rk $ str s))
  | Ref (rk, s, Some txt) ->
      hovbox 0
        (wrap "{" "}"
           ( hovbox 0 (wrap "{" "}" (fmt_ref_kind rk $ str s))
           $ fmt "@ " $ fmt_text txt ))
  | Special_ref (SRK_module_list l) ->
      hvbox 0 (wrap "{!modules:" "}" (list l "@," str))
  | Special_ref SRK_index_list -> str "{!indexlist}"
  | Target (s, l) ->
      let target = Option.value_map s ~default:"" ~f:(fun s -> s ^ ":") in
      hovbox 0
        (wrap "{" "}"
           (char '%' $ str target $ str ~escape:false l $ char '%'))

and fmt_list kind l =
  let light_syntax =
    let line_start =
      fmt (match kind with `List -> "- " | `Enum -> "+ ")
    in
    let fmt_item txt = hovbox 2 (line_start $ fmt_text txt) in
    vbox 0 (list l "@," fmt_item)
  in
  let heavy_syntax =
    let fmt_item txt = hovbox 0 (wrap "{- " "}" (fmt_text txt)) in
    let start : _ format =
      match kind with `List -> "{ul@," | `Enum -> "{ol@,"
    in
    vbox 1 (wrap start "}" (list l "@," fmt_item))
  in
  let print_and_parse fmt =
    let str = Format.asprintf "\n%t" fmt in
    Octavius.parse (Lexing.from_string str)
  in
  match (print_and_parse light_syntax, print_and_parse heavy_syntax) with
  | Ok x, Ok y when Poly.(x = y) -> light_syntax
  | Ok _, Ok _ -> heavy_syntax
  | Error _, _ | _, Error _ -> heavy_syntax

and fmt_newline = close_box $ fmt "\n@\n" $ open_hovbox 0

and fmt_text txt =
  let no_space_before = ['.'; ':'; ';'; ','; '-'; ')'; '\''] in
  let no_space_before c = List.mem no_space_before c ~equal:Char.equal in
  let is_space c = List.mem [' '; '\r'; '\t'; '\n'] c ~equal:Char.equal in
  let f ?prev:_ curr ?next =
    match next with
    | Some (Raw " ") -> fmt_text_elt curr
    | Some (Raw x) when no_space_before x.[0] -> fmt_text_elt curr
    | Some Newline -> fmt_text_elt curr
    | Some next -> (
      match curr with
      | Newline -> fmt_newline
      | List _ | Enum _ -> fmt_text_elt curr $ fmt_newline
      | Raw x when not (is_space x.[String.length x - 1]) -> (
          fmt_text_elt curr
          $ match next with List _ | Enum _ -> fmt "@\n" | _ -> noop )
      | Code _ -> (
          fmt_text_elt curr
          $
          match next with
          | List _ | Enum _ -> fmt "@\n"
          | Raw x -> fmt_if (is_space x.[0]) "@ "
          | _ -> fmt "@ " )
      | _ -> (
          fmt_text_elt curr
          $ match next with List _ | Enum _ -> fmt "@\n" | _ -> fmt "@ " ) )
    | None -> fmt_text_elt curr
  in
  hovbox 0 (list_pn txt f)

let at () = char '@'

let fmt_see_ref = function
  | See_url s | See_file s | See_doc s -> char '<' $ Fmt.str s $ char '>'

let fmt_tag t =
  hovbox 0
    ( match t with
    | Author s -> at () $ fmt "author@ " $ str s
    | Version s -> at () $ fmt "version@ " $ str s
    | See (sr, txt) ->
        at () $ fmt "see@ " $ fmt_see_ref sr $ fmt "@ " $ fmt_text txt
    | Since s -> at () $ fmt "since@ " $ str s
    | Before (s, txt) ->
        at () $ fmt "before@ " $ str s $ fmt "@ " $ fmt_text txt
    | Deprecated txt -> at () $ fmt "deprecated@ " $ fmt_text txt
    | Param (s, txt) ->
        at () $ fmt "param@ " $ str s $ fmt "@ " $ fmt_text txt
    | Raised_exception (s, txt) ->
        at () $ fmt "raise@ " $ str s $ fmt "@ " $ fmt_text txt
    | Return_value txt -> at () $ fmt "return@ " $ fmt_text txt
    | Inline -> at () $ str "inline"
    | Custom (s, []) -> at () $ str s
    | Custom (s, txt) -> at () $ str s $ fmt "@ " $ fmt_text txt
    | Canonical s -> at () $ fmt "canonical@ " $ str s )

let fmt (txt, tags) =
  if List.is_empty tags then hovbox 0 (fmt_text txt)
  else if List.is_empty txt then vbox 0 (list tags "@;" fmt_tag)
  else
    vbox 0
      (hovbox 0 (fmt_text txt) $ fmt "@;" $ vbox 0 (list tags "@;" fmt_tag))

let diff c x y =
  let norm z =
    let f (txt, _) = Normalize.docstring c txt in
    Set.of_list (module String) (List.map ~f z)
  in
  Set.symmetric_diff (norm x) (norm y)
