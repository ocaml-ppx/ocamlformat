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
open Odoc__parser.Ast
module Names = Odoc__model.Names
module Location_ = Odoc__model.Location_

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

let str ?(escape = true) s =
  let escape = if escape then escape_all else Fn.id in
  s
  |> String.split_on_chars ~on:['\t'; '\n'; '\011'; '\012'; '\r'; ' ']
  |> List.filter ~f:(Fn.non String.is_empty)
  |> fun s -> list s "@ " (fun s -> escape s |> str)

let str_verbatim = Fmt.str

let fmt_if_not_empty lst fmt = fmt_if (not (List.is_empty lst)) fmt

let ign_loc ~f with_loc = f with_loc.Location_.value

(* Decide between using light and heavy syntax for lists *)
let list_should_use_heavy_syntax items =
  let heavy_nestable_block_elements = function
    (* More than one element or contains a list *)
    | [{Location_.value= `List _}] | _ :: _ :: _ -> true
    | [] | [_] -> false
  in
  List.exists items ~f:heavy_nestable_block_elements

(* Decide if should break between two elements *)
let block_element_should_break elem next =
  match (elem, next) with
  (* Mandatory breaks *)
  | `List (_, _), _ | `Paragraph _, `Paragraph _ -> true
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
        | Some {Location_.value= n}
          when block_element_should_break
                 (elem :> block_element)
                 (n :> block_element) ->
            fmt "\n@\n"
        | Some _ -> fmt "@\n"
        | None -> fmt ""
      in
      f elem $ break )

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
       (str s $ fmt_if_not_empty elems "@ " $ list elems "" fmt_elem))

let rec fmt_reference_resolved =
  let open Names in
  let open Reference.Resolved in
  let dot r s =
    let _, k = fmt_reference_resolved r in
    k $ str "." $ str s
  in
  function
  | `Identifier id -> ("", str (Identifier.name id))
  | `SubstAlias (_, r) -> fmt_reference_resolved (r :> t)
  | `Module (r, s) -> ("module:", dot (r :> t) (ModuleName.to_string s))
  | `Canonical (_, `Resolved r) -> fmt_reference_resolved (r :> t)
  | `Canonical (p, _) -> fmt_reference_resolved (p :> t)
  | `ModuleType (r, s) ->
      ("module-type:", dot (r :> t) (ModuleTypeName.to_string s))
  | `Type (r, s) -> ("type:", dot (r :> t) (TypeName.to_string s))
  | `Constructor (r, s) ->
      ("constructor:", dot (r :> t) (ConstructorName.to_string s))
  | `Field (r, s) -> ("field:", dot (r :> t) (FieldName.to_string s))
  | `Extension (r, s) ->
      ("extension:", dot (r :> t) (ExtensionName.to_string s))
  | `Exception (r, s) ->
      ("exception:", dot (r :> t) (ExceptionName.to_string s))
  | `Value (r, s) -> ("val:", dot (r :> t) (ValueName.to_string s))
  | `Class (r, s) -> ("class:", dot (r :> t) (ClassName.to_string s))
  | `ClassType (r, s) ->
      ("class-type:", dot (r :> t) (ClassTypeName.to_string s))
  | `Method (r, s) -> ("method:", dot (r :> t) (MethodName.to_string s))
  | `InstanceVariable (r, s) ->
      ("instance-variable:", dot (r :> t) (InstanceVariableName.to_string s))
  | `Label (r, s) ->
      let kind, k = fmt_reference_resolved (r :> t) in
      (kind, k $ str ":" $ str (LabelName.to_string s))

let fmt_reference_kind = function
  | `TModule -> "module:"
  | `TModuleType -> "module-type:"
  | `TType -> "type:"
  | `TConstructor -> "constructor:"
  | `TField -> "field:"
  | `TExtension -> "extension:"
  | `TException -> "exception:"
  | `TValue -> "value:"
  | `TClass -> "class:"
  | `TClassType -> "class-type:"
  | `TMethod -> "method:"
  | `TInstanceVariable -> "instance-variable:"
  | `TLabel -> "label:"
  | `TPage -> "page:"
  | `TUnknown -> ""

let rec fmt_reference : Reference.t -> _ =
  let open Names in
  let open Reference in
  let dot p rhs =
    let _, k = fmt_reference p in
    k $ str "." $ str rhs
  in
  function
  | `Root (s, kind) -> (fmt_reference_kind kind, str (UnitName.to_string s))
  | `Dot (p, s) -> ("", dot (p :> t) s)
  | `Module (p, s) -> ("module:", dot (p :> t) (ModuleName.to_string s))
  | `ModuleType (p, s) ->
      ("module-type:", dot (p :> t) (ModuleTypeName.to_string s))
  | `Type (p, s) -> ("type:", dot (p :> t) (TypeName.to_string s))
  | `Constructor (p, s) ->
      ("constructor:", dot (p :> t) (ConstructorName.to_string s))
  | `Field (p, s) -> ("field:", dot (p :> t) (FieldName.to_string s))
  | `Extension (p, s) ->
      ("extension:", dot (p :> t) (ExtensionName.to_string s))
  | `Exception (p, s) ->
      ("exception:", dot (p :> t) (ExceptionName.to_string s))
  | `Value (p, s) -> ("val:", dot (p :> t) (ValueName.to_string s))
  | `Class (p, s) -> ("class:", dot (p :> t) (ClassName.to_string s))
  | `ClassType (p, s) ->
      ("class-type:", dot (p :> t) (ClassTypeName.to_string s))
  | `Method (p, s) -> ("method:", dot (p :> t) (MethodName.to_string s))
  | `InstanceVariable (p, s) ->
      ("instance-variable:", dot (p :> t) (InstanceVariableName.to_string s))
  | `Label (p, s) -> ("label:", dot (p :> t) (LabelName.to_string s))
  | `Resolved r -> fmt_reference_resolved r

let rec fmt_inline_element : inline_element -> Fmt.t = function
  | `Space -> fmt "@ "
  | `Word w ->
      (* Escape lines starting with '+' or '-' *)
      let escape =
        fmt_if_k
          (String.length w > 0 && Char.(w.[0] = '+' || w.[0] = '-'))
          (if_newline "\\")
      in
      escape $ str w
  | `Code_span s ->
      let s = escape_brackets s in
      hovbox 0 (wrap "[" "]" (str_verbatim s))
  | `Raw_markup (`Html, s) -> str s
  | `Styled (style, elems) ->
      fmt_styled style (ign_loc ~f:fmt_inline_element) elems
  | `Reference (_kind, ref, txt) ->
      let ref_kind, ref = fmt_reference (ref :> Reference.t) in
      let ref = fmt "{!" $ str ref_kind $ ref $ fmt "}" in
      if List.is_empty txt then ref
      else
        hovbox 0 (wrap "{" "}" (ref $ fmt "@ " $ fmt_inline_elements txt))
  | `Link (url, txt) -> (
      let url = wrap "{:" "}" (str url) in
      match txt with
      | [] -> url
      | txt -> wrap "{" "}" (url $ fmt "@ " $ fmt_inline_elements txt) )

and fmt_inline_elements txt = list txt "" (ign_loc ~f:fmt_inline_element)

(** TODO:

    {[
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
    ]} *)

and fmt_nestable_block_element : nestable_block_element -> t = function
  | `Paragraph elems -> hovbox 0 (fmt_inline_elements elems)
  | `Code_block s ->
      vbox 0 (fmt "{[@;<1 -999>" $ str_verbatim s $ fmt "@ ]}")
  | `Verbatim s -> vbox 0 (fmt "{v@;<1 -999>" $ str_verbatim s $ fmt "@ v}")
  | `Modules mods ->
      let mods = (mods :> Reference.t list) in
      hovbox 0
        (wrap "{!modules:@," "@,}"
           (list mods "@ " (fun ref -> snd (fmt_reference ref))))
  | `List (k, items) when list_should_use_heavy_syntax items ->
      fmt_list_heavy k items
  | `List (k, items) -> fmt_list_light k items

and fmt_list_heavy kind items =
  let fmt_item elems =
    let box = match elems with [_] -> hvbox 3 | _ -> vbox 3 in
    box (wrap "{- " "@;<1 -3>}" (fmt_nestable_block_elements elems))
  and start : s =
    match kind with `Unordered -> "{ul@," | `Ordered -> "{ol@,"
  in
  vbox 1 (wrap start "@;<1 -1>}" (list items "@," fmt_item))

and fmt_list_light kind items =
  let line_start =
    match kind with `Unordered -> fmt "- " | `Ordered -> fmt "+ "
  in
  let fmt_item elems =
    line_start $ vbox 0 (fmt_nestable_block_elements elems)
  in
  vbox 0 (list items "@," fmt_item)

and fmt_nestable_block_elements elems =
  list_block_elem elems fmt_nestable_block_element

let at = char '@'

let fmt_tag : tag -> Fmt.t = function
  | `Author s -> at $ fmt "author@ " $ str s
  | `Version s -> at $ fmt "version@ " $ str s
  | `See (_, sr, txt) ->
      at $ fmt "see@ <" $ str sr $ fmt ">@ "
      $ fmt_nestable_block_elements txt
  | `Since s -> at $ fmt "since@ " $ str s
  | `Before (s, txt) ->
      at $ fmt "before@ " $ str s $ fmt "@ "
      $ fmt_nestable_block_elements txt
  | `Deprecated txt ->
      at $ fmt "deprecated@ " $ fmt_nestable_block_elements txt
  | `Param (s, txt) ->
      at $ fmt "param@ " $ str s $ fmt "@ "
      $ fmt_nestable_block_elements txt
  | `Raise (s, txt) ->
      at $ fmt "raise@ " $ str s $ fmt "@ "
      $ fmt_nestable_block_elements txt
  | `Return txt -> at $ fmt "return@ " $ fmt_nestable_block_elements txt
  | `Inline -> at $ str "inline"
  | `Open -> at $ str "open"
  | `Closed -> at $ str "closed"
  | `Canonical (_, ref) ->
      (* TODO: print the path ? *)
      let _, ref = fmt_reference (ref :> Reference.t) in
      at $ fmt "canonical@ " $ ref

let fmt_block_element = function
  | `Tag tag -> hovbox 0 (fmt_tag tag)
  | `Heading (lvl, lbl, elems) ->
      let lvl = Int.to_string lvl in
      let lbl =
        match lbl with Some lbl -> str ":" $ str lbl | None -> fmt ""
      in
      hovbox 0
        (wrap "{" "}" (str lvl $ lbl $ fmt "@ " $ fmt_inline_elements elems))
  | #nestable_block_element as elm ->
      hovbox 0 (fmt_nestable_block_element elm)

let fmt (docs : docs) = vbox 0 (list_block_elem docs fmt_block_element)

let diff c x y =
  let norm z =
    let f (txt, _) = Normalize.docstring c txt in
    Set.of_list (module String) (List.map ~f z)
  in
  Set.symmetric_diff (norm x) (norm y)
