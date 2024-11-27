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

module Ast = Ocamlformat_odoc_parser.Ast
module Odoc_parser = Ocamlformat_odoc_parser.Odoc_parser

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

let light_heavy_to_string = function `Light -> "Light" | `Heavy -> "Heavy"

let alignment_to_string = function
  | `Left -> "Left"
  | `Right -> "Right"
  | `Center -> "Center"

let header_data_to_string = function `Header -> "Header" | `Data -> "Data"

let media_to_string = function
  | `Audio -> "Audio"
  | `Video -> "Video"
  | `Image -> "Image"

let fmt_media_href fmt = function
  | `Reference s -> fpf fmt "Reference(%s)" s
  | `Link s -> fpf fmt "Link(%s)" s

let rec odoc_nestable_block_element c fmt : Ast.nestable_block_element -> _ =
  function
  | `Paragraph elms -> fpf fmt "Paragraph(%a)" odoc_inline_elements elms
  | `Code_block (b : Ast.code_block) ->
      let fmt_metadata fmt (m : Ast.code_block_meta) =
        fpf fmt "(%a, %a)" (ign_loc str) m.language
          (option (ign_loc str))
          m.tags
      in
      let fmt_content =
        ign_loc (fun fmt s -> str fmt (c.normalize_code s))
      in
      let fmt_output =
        option (list (ign_loc (odoc_nestable_block_element c)))
      in
      fpf fmt "Code_block(%a, %a, %a, %a)" (option fmt_metadata) b.meta
        (option str) b.delimiter fmt_content b.content fmt_output b.output
  | `Math_block txt -> fpf fmt "Math_block(%a)" str txt
  | `Verbatim txt -> fpf fmt "Verbatim(%a)" str txt
  | `Modules mods -> fpf fmt "Modules(%a)" (list odoc_reference) mods
  | `List (ord, _syntax, items) ->
      let ord = match ord with `Unordered -> "U" | `Ordered -> "O" in
      let list_item fmt elems =
        fpf fmt "Item(%a)" (odoc_nestable_block_elements c) elems
      in
      fpf fmt "List(%s,%a)" ord (list list_item) items
  | `Table ((grid, alignment), syntax) ->
      let pp_align fmt aln = fpf fmt "%s" (alignment_to_string aln) in
      let pp_cell fmt (elems, header) =
        fpf fmt "(%a,%s)"
          (odoc_nestable_block_elements c)
          elems
          (header_data_to_string header)
      in
      let pp_grid = list (list pp_cell) in
      let pp_alignment = option (list (option pp_align)) in
      fpf fmt "Table((%a,%a),%s)" pp_grid grid pp_alignment alignment
        (light_heavy_to_string syntax)
  | `Media (_kind, href, text, media) ->
      fpf fmt "Media(%a,%S,%s)" (ign_loc fmt_media_href) href text
        (media_to_string media)

and odoc_nestable_block_elements c fmt elems =
  list (ign_loc (odoc_nestable_block_element c)) fmt elems

let odoc_implicitly_ended_tag c fmt tag elems =
  fpf fmt "%s(%a)" tag (odoc_nestable_block_elements c) elems

let odoc_tag c fmt : Ast.tag -> unit = function
  | `Author txt -> fpf fmt "Author(%a)" str txt
  | `Deprecated elems -> odoc_implicitly_ended_tag c fmt "Deprecated" elems
  | `Param (p, elems) ->
      fpf fmt "Param(%a,%a)" str p (odoc_nestable_block_elements c) elems
  | `Raise (p, elems) ->
      fpf fmt "Raise(%a,%a)" str p (odoc_nestable_block_elements c) elems
  | `Return elems -> odoc_implicitly_ended_tag c fmt "Return" elems
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
  | `Hidden -> fpf fmt "Hidden"
  | `Children_order elems ->
      odoc_implicitly_ended_tag c fmt "Children_order" elems
  | `Short_title elems -> odoc_implicitly_ended_tag c fmt "Short_title" elems

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
