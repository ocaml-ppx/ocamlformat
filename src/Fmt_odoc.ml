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
    return [true] if the character should be escaped, [false] otherwise. *)
let ensure_escape ?(escape_char = '\\') ~escapeworthy s =
  let module E = String.Escaping in
  let dst = Buffer.create (String.length s + 8) in
  let prev_off = ref 0 in
  let stash until =
    Buffer.add_substring dst s ~pos:!prev_off ~len:(until - !prev_off)
  in
  let len = String.length s in
  for i = 0 to len - 1 do
    if escapeworthy s.[i] && not (E.is_char_escaped s ~escape_char i) then (
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

let ign_loc f with_loc = f with_loc.Location_.value

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

(* Format references From odoc's `html/comment.ml` *)
let rec fmt_reference_resolved : Reference.Resolved.t -> Fmt.t =
  let open Names in
  let open Reference.Resolved in
  let dot r s = fmt_reference_resolved r $ str "." $ str s in
  function
  | `Identifier id -> str (Identifier.name id)
  | `SubstAlias (_, r) -> fmt_reference_resolved (r :> t)
  | `Module (r, s) -> dot (r :> t) (ModuleName.to_string s)
  | `Canonical (_, `Resolved r) -> fmt_reference_resolved (r :> t)
  | `Canonical (p, _) -> fmt_reference_resolved (p :> t)
  | `ModuleType (r, s) -> dot (r :> t) (ModuleTypeName.to_string s)
  | `Type (r, s) -> dot (r :> t) (TypeName.to_string s)
  | `Constructor (r, s) -> dot (r :> t) (ConstructorName.to_string s)
  | `Field (r, s) -> dot (r :> t) (FieldName.to_string s)
  | `Extension (r, s) -> dot (r :> t) (ExtensionName.to_string s)
  | `Exception (r, s) -> dot (r :> t) (ExceptionName.to_string s)
  | `Value (r, s) -> dot (r :> t) (ValueName.to_string s)
  | `Class (r, s) -> dot (r :> t) (ClassName.to_string s)
  | `ClassType (r, s) -> dot (r :> t) (ClassTypeName.to_string s)
  | `Method (r, s) -> dot (r :> t) (MethodName.to_string s)
  | `InstanceVariable (r, s) ->
      dot (r :> t) (InstanceVariableName.to_string s)
  | `Label (r, s) ->
      fmt_reference_resolved (r :> t)
      $ str ":"
      $ str (LabelName.to_string s)

let rec fmt_reference : Reference.t -> Fmt.t =
  let open Names in
  let open Reference in
  let dot p rhs = fmt_reference p $ str "." $ str rhs in
  function
  | `Root (s, _) -> str (UnitName.to_string s)
  | `Dot (p, s) -> dot (p :> t) s
  | `Module (p, s) -> dot (p :> t) (ModuleName.to_string s)
  | `ModuleType (p, s) -> dot (p :> t) (ModuleTypeName.to_string s)
  | `Type (p, s) -> dot (p :> t) (TypeName.to_string s)
  | `Constructor (p, s) -> dot (p :> t) (ConstructorName.to_string s)
  | `Field (p, s) -> dot (p :> t) (FieldName.to_string s)
  | `Extension (p, s) -> dot (p :> t) (ExtensionName.to_string s)
  | `Exception (p, s) -> dot (p :> t) (ExceptionName.to_string s)
  | `Value (p, s) -> dot (p :> t) (ValueName.to_string s)
  | `Class (p, s) -> dot (p :> t) (ClassName.to_string s)
  | `ClassType (p, s) -> dot (p :> t) (ClassTypeName.to_string s)
  | `Method (p, s) -> dot (p :> t) (MethodName.to_string s)
  | `InstanceVariable (p, s) ->
      dot (p :> t) (InstanceVariableName.to_string s)
  | `Label (p, s) -> dot (p :> t) (LabelName.to_string s)
  | `Resolved r -> fmt_reference_resolved r

let fmt_reference_kind : Reference.t -> Fmt.t = function
  | `Module _ -> str "!module:"
  | `ModuleType _ -> str "!modtype:"
  | `Class _ -> str "!class:"
  | `ClassType _ -> str "!classtype:"
  | `Value _ -> str "!val:"
  | `Type _ -> str "!type:"
  | `Exception _ -> str "!exception:"
  | `Method _ -> str "!method:"
  | `Field _ -> str "!field:"
  | `Label _ -> str "!label:"
  | `Constructor _ -> str "!constructor:"
  | `Extension _ -> str "!extension:"
  | `InstanceVariable _ -> str "!instance-variable:"
  | `Resolved _ | `Root _ | `Dot _ -> str "!"

let rec fmt_inline_element : inline_element -> Fmt.t = function
  | `Space -> fmt "@ "
  | `Word w ->
      (* Escape lines starting with '+' or '-' *)
      let escape =
        if String.length w > 0 && Char.(w.[0] = '+' || w.[0] = '-') then
          if_newline "\\"
        else fmt ""
      in
      escape $ str w
  | `Code_span s ->
      let s = escape_brackets s in
      hovbox 0 (wrap "[" "]" (str_verbatim s))
  | `Raw_markup (`Html, s) -> str s
  | `Styled (style, elems) ->
      fmt_styled style (ign_loc fmt_inline_element) elems
  | `Reference (_kind, ref, txt) ->
      let ref = wrap "{" "}" (fmt_reference_kind ref $ fmt_reference ref) in
      if List.is_empty txt then ref
      else hovbox 0 (wrap "{" "}" (ref $ fmt "@ " $ fmt_inline_elements txt))
  | `Link (url, txt) -> (
      let url = wrap "{:" "}" (str url) in
      match txt with
      | [] -> url
      | txt -> wrap "{" "}" (url $ fmt "@ " $ fmt_inline_elements txt) )

and fmt_inline_elements txt = list txt "" (ign_loc fmt_inline_element)

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
  | `Paragraph elems ->
      hovbox 0 (fmt_inline_elements elems)
  | `Code_block s ->
      vbox 0 (fmt "{[@;<1 -999>" $ str_verbatim s $ fmt "@ ]}")
  | `Verbatim s -> vbox 0 (fmt "{v@;<1 -999>" $ str_verbatim s $ fmt "@ v}")
  | `Modules mods ->
      let mods = (mods :> Reference.t list) in
      hovbox 0 (wrap "{!modules:@," "@,}" (list mods "@ " fmt_reference))
  | `List (k, items) when list_should_use_heavy_syntax items ->
      fmt_list_heavy k items
  | `List (k, items) -> fmt_list_light k items

and fmt_list_heavy kind items =
  let fmt_item elems =
    wrap "{- " "@,}" @@ vbox 0 (fmt_nestable_block_elements elems)
  and start : s =
    match kind with `Unordered -> "{ul@," | `Ordered -> "{ol@,"
  in
  vbox 1 (wrap start "}" (list items "@," fmt_item))

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
      let ref = fmt_reference (ref :> Reference.t) in
      at $ fmt "canonical@ " $ ref

let fmt_block_element = function
  | `Tag tag -> hovbox 0 (fmt_tag tag)
  | `Heading (lvl, lbl, elems) ->
      let lvl = Int.to_string lvl in
      let lbl = match lbl with Some lbl -> str ":" $ str lbl | None -> fmt "" in
      hovbox 0
        (wrap "{" "}"
           (str lvl $ lbl $ fmt "@ " $ fmt_inline_elements elems))
  | #nestable_block_element as elm ->
      hovbox 0 (fmt_nestable_block_element elm)

let fmt (docs : docs) = vbox 0 (list_block_elem docs fmt_block_element)

let diff c x y =
  let norm z =
    let f (txt, _) = Normalize.docstring c txt in
    Set.of_list (module String) (List.map ~f z)
  in
  Set.symmetric_diff (norm x) (norm y)
