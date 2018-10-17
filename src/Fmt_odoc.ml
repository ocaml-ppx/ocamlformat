(**********************************************************************
 *                                                                    *
 *                            OCamlFormat                             *
 *                                                                    *
 *  Copyright (c) 2017-present, Facebook, Inc.  All rights reserved.  *
 *                                                                    *
 *  This source code is licensed under the MIT license found in the   *
 *  LICENSE file in the root directory of this source tree.           *
 *                                                                    *
 **********************************************************************)

open Octavius.Types
open Fmt

exception Unsupported

let str s =
  s
  |> String.split_on_chars ~on:['\t'; '\n'; '\011'; '\012'; '\r'; ' ']
  |> List.filter ~f:(fun x -> not (String.is_empty x))
  |> fun s -> list s "@ " str

let fmt_ref_kind = function
  | RK_element -> str ""
  | RK_module -> str "module:"
  | RK_module_type -> str "modtype:"
  | RK_class -> str "class:"
  | RK_class_type -> str "classtype:"
  | RK_value -> str "val:"
  | RK_type -> str "type:"
  | RK_exception -> str "exception:"
  | RK_attribute -> str "attribute:"
  | RK_method -> str "method:"
  | RK_section -> str "section:"
  | RK_recfield -> str "recfield:"
  | RK_const -> str "const:"
  | RK_link -> str ":"
  | RK_custom s -> str s $ str ":"

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
  hovbox 0 (wrap "{" "}" (str s $ fmt "@ " $ fmt_text txt))

and fmt_text_elt = function
  | Raw s -> str s
  | Code s -> hovbox 0 (wrap "[" "]" (str s))
  | PreCode s -> hovbox 0 (wrap "{[" "]}" (str s))
  | Verbatim s -> hovbox 0 (wrap "{v " " v}" (str s))
  | Style (st, txt) -> fmt_style st txt
  | List l -> vbox 0 (wrap "{ul " "}" (hovbox 2 (list l "@;" fmt_item)))
  | Enum l -> vbox 0 (wrap "{ol " "}" (hovbox 2 (list l "@;" fmt_item)))
  | Newline -> fmt "\n@\n"
  | Title (i, None, txt) ->
      hovbox 0
        (wrap "{" "}" (str (Int.to_string i) $ fmt "@ " $ fmt_text txt))
  | Title (i, Some s, txt) ->
      hovbox 0
        (wrap "{" "}"
           ( str (Int.to_string i)
           $ str ":" $ str s $ fmt "@ " $ fmt_text txt ))
  | Ref (rk, s, None) -> hovbox 0 (wrap "{!" "}" (fmt_ref_kind rk $ str s))
  | Ref _ -> raise Unsupported
  | Special_ref (SRK_module_list l) ->
      hvbox 0 (wrap "{!modules:" "}" (list l "@," str))
  | Special_ref SRK_index_list -> str "{!indexlist}"
  | Target (s, l) ->
      hovbox 0
        (wrap "{" "}"
           ( str "%"
           $ str (Option.value s ~default:"latex")
           $ str ":" $ str l $ str "%" ))

and fmt_item txt = hovbox 0 (wrap "{- " "}" (fmt_text txt))

and fmt_text txt =
  let ops = ['.'; ':'; ';'; ','] in
  let is_op c = List.mem ops c ~equal:Char.equal in
  let f ?prev:_ curr ?next =
    match next with
    | Some (Raw x) when is_op x.[0] -> fmt_text_elt curr
    | Some Newline -> fmt_text_elt curr
    | Some _ -> (
      match curr with
      | Newline -> fmt_text_elt curr
      | _ -> fmt_text_elt curr $ fmt "@ " )
    | None -> fmt_text_elt curr
  in
  list_pn txt f

let at () = str "@"

let fmt_tag = function
  | Author s -> at () $ fmt "author@ " $ str s
  | Version s -> at () $ fmt "version@ " $ str s
  | See (_see_ref, txt) -> at () $ fmt "see@ " $ fmt_text txt
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
  | Custom (s, txt) -> at () $ str s $ fmt "@ " $ fmt_text txt
  | Canonical s -> at () $ fmt "canonical@ " $ str s

let fmt (txt, tags) =
  try
    if List.is_empty tags then Ok (hovbox 0 (fmt_text txt))
    else Ok (hovbox 0 (fmt_text txt $ fmt "@;" $ list tags "@;" fmt_tag))
  with err -> Error err
