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

let parse ~loc text =
  let location = loc.Location.loc_start in
  let location =
    { location with
      pos_cnum= location.pos_cnum + 3 (* Length of comment opening *) }
  in
  let v = Odoc_parser.parse_comment ~location ~text in
  match Odoc_parser.warnings v with
  | [] -> Ok (Odoc_parser.ast v)
  | warnings -> Error warnings

let parse_file location text =
  Odoc_parser.ast (Odoc_parser.parse_comment ~location ~text)

let warn fmt warning =
  Format.fprintf fmt "Warning: Invalid documentation comment:@,%s\n%!"
    (Odoc_parser.Warning.to_string warning)

let is_tag_only =
  List.for_all ~f:(function
    | {Odoc_parser.Loc.value= `Tag _; _} -> true
    | _ -> false )

type norm_conf = {normalize_code: string -> string}

let normalize_text s =
  (* normalize consecutive whitespace chars to a single space *)
  String.concat ~sep:" "
    (List.filter ~f:(Fn.non String.is_empty)
       (String.split_on_chars s ~on:['\t'; '\n'; '\011'; '\012'; '\r'; ' ']) )

let list f fmt l =
  let pp_sep fmt () = Format.fprintf fmt "" in
  Format.pp_print_list ~pp_sep f fmt l

let str fmt s = Format.fprintf fmt "%s" (normalize_text s)

let ign_loc f fmt with_loc = f fmt with_loc.Odoc_parser.Loc.value

let fpf = Format.fprintf

let odoc_reference = ign_loc str

let option f fmt = function Some v -> f fmt v | None -> ()

let pair fmt_a fmt_b fmt (a, b) = fpf fmt "(%a,%a)" fmt_a a fmt_b b

let odoc_style fmt = function
  | `Bold -> fpf fmt "Bold"
  | `Italic -> fpf fmt "Italic"
  | `Emphasis -> fpf fmt "Emphasis"
  | `Superscript -> fpf fmt "Superscript"
  | `Subscript -> fpf fmt "Subscript"

let rec odoc_inline_element fmt = function
  | `Space _ -> ()
  | `Word txt ->
      (* Ignore backspace changes *)
      let txt =
        String.filter txt ~f:(function '\\' -> false | _ -> true)
      in
      fpf fmt "Word(%a)" str txt
  | `Code_span txt -> fpf fmt "Code_span(%a)" str txt
  | `Math_span txt -> fpf fmt "Math_span(%a)" str txt
  | `Raw_markup (Some lang, txt) -> fpf fmt "Raw_markup(%s,%a)" lang str txt
  | `Raw_markup (None, txt) -> fpf fmt "Raw_markup(%a)" str txt
  | `Styled (style, elems) ->
      fpf fmt "Styled(%a,%a)" odoc_style style odoc_inline_elements elems
  | `Reference (_kind, ref, content) ->
      fpf fmt "Reference(%a,%a)" odoc_reference ref odoc_inline_elements
        content
  | `Link (txt, content) ->
      fpf fmt "Link(%a,%a)" str txt odoc_inline_elements content

and odoc_inline_elements fmt elems =
  list (ign_loc odoc_inline_element) fmt elems

let rec odoc_nestable_block_element c fmt = function
  | `Paragraph elms -> fpf fmt "Paragraph(%a)" odoc_inline_elements elms
  | `Code_block (metadata, txt) ->
      let txt = Odoc_parser.Loc.value txt in
      let txt = c.normalize_code txt in
      let fmt_metadata =
        option (pair (ign_loc str) (option (ign_loc str)))
      in
      fpf fmt "Code_block(%a, %a)" fmt_metadata metadata str txt
  | `Math_block txt -> fpf fmt "Math_block(%a)" str txt
  | `Verbatim txt -> fpf fmt "Verbatim(%a)" str txt
  | `Modules mods -> fpf fmt "Modules(%a)" (list odoc_reference) mods
  | `List (ord, _syntax, items) ->
      let ord = match ord with `Unordered -> "U" | `Ordered -> "O" in
      let list_item fmt elems =
        fpf fmt "Item(%a)" (odoc_nestable_block_elements c) elems
      in
      fpf fmt "List(%s,%a)" ord (list list_item) items

and odoc_nestable_block_elements c fmt elems =
  list (ign_loc (odoc_nestable_block_element c)) fmt elems

let odoc_tag c fmt = function
  | `Author txt -> fpf fmt "Author(%a)" str txt
  | `Deprecated elems ->
      fpf fmt "Deprecated(%a)" (odoc_nestable_block_elements c) elems
  | `Param (p, elems) ->
      fpf fmt "Param(%a,%a)" str p (odoc_nestable_block_elements c) elems
  | `Raise (p, elems) ->
      fpf fmt "Raise(%a,%a)" str p (odoc_nestable_block_elements c) elems
  | `Return elems ->
      fpf fmt "Return(%a)" (odoc_nestable_block_elements c) elems
  | `See (kind, txt, elems) ->
      let kind =
        match kind with `Url -> "U" | `File -> "F" | `Document -> "D"
      in
      fpf fmt "See(%s,%a,%a)" kind str txt
        (odoc_nestable_block_elements c)
        elems
  | `Since txt -> fpf fmt "Since(%a)" str txt
  | `Before (p, elems) ->
      fpf fmt "Before(%a,%a)" str p (odoc_nestable_block_elements c) elems
  | `Version txt -> fpf fmt "Version(%a)" str txt
  | `Canonical ref -> fpf fmt "Canonical(%a)" odoc_reference ref
  | `Inline -> fpf fmt "Inline"
  | `Open -> fpf fmt "Open"
  | `Closed -> fpf fmt "Closed"

let odoc_block_element c fmt = function
  | `Heading (lvl, lbl, content) ->
      let lvl = Int.to_string lvl in
      let lbl = match lbl with Some lbl -> lbl | None -> "" in
      fpf fmt "Heading(%s,%a,%a)" lvl str lbl odoc_inline_elements content
  | `Tag tag -> fpf fmt "Tag(%a)" (odoc_tag c) tag
  | #Odoc_parser.Ast.nestable_block_element as elm ->
      odoc_nestable_block_element c fmt elm

let odoc_docs c fmt elems = list (ign_loc (odoc_block_element c)) fmt elems

let normalize ~parse_docstrings ~normalize_code text =
  if not parse_docstrings then normalize_text text
  else
    let location = Lexing.dummy_pos in
    let parsed = Odoc_parser.parse_comment ~location ~text in
    let c = {normalize_code} in
    Format.asprintf "Docstring(%a)%!" (odoc_docs c) (Odoc_parser.ast parsed)

let dump fmt x =
  let c = {normalize_code= Fn.id} in
  Format.fprintf fmt "Docstring(%a)%!" (odoc_docs c) x
