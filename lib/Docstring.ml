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

(** Like [String.fields] but the separator can be of variable length.
    [f start_index str] returns [0] if [str.[start_index]] is not the first
    char of a separator, or returns the length of the separator.
    [start_index] is always less than [String.length s]. *)
let fields_pattern ?(empty = true) ~is_sep s =
  let module Sub = Astring.String.Sub in
  let len = String.length s in
  (* Add a sub at the front of [acc] while respecting [empty]. *)
  let sub' ~start ~stop acc =
    if start = stop && not empty then acc else Sub.v ~start ~stop s :: acc
  in
  let rec loop acc field_start i =
    if i = len then List.rev (sub' ~start:field_start ~stop:i acc)
    else
      match is_sep i s with
      | 0 -> loop acc field_start (i + 1)
      | sep_len ->
          let next = i + sep_len in
          loop (sub' ~start:field_start ~stop:i acc) next next
  in
  loop [] 0 0

let normalize_text_subs s =
  (* normalize consecutive whitespace chars to a single space *)
  let is_sep i s =
    match s.[i] with
    | '\t' | '\n' | '\011' | '\012' | '\r' | ' ' -> 1
    | '\\' when i + 1 < String.length s -> (
      match s.[i + 1] with 'n' -> 2 | _ -> 0 )
    | _ -> 0
  in
  fields_pattern ~empty:false ~is_sep s

let normalize_text s =
  let module Sub = Astring.String.Sub in
  let sep = Sub.v " " in
  Sub.concat ~sep (normalize_text_subs s) |> Sub.to_string

let list f fmt l =
  let pp_sep _ () = () in
  Format.pp_print_list ~pp_sep f fmt l

let str_with_sep ~pp_sep fmt s =
  Format.pp_print_list ~pp_sep Astring.String.Sub.pp fmt
    (normalize_text_subs s)

let str fmt s =
  let pp_sep fmt () = Format.pp_print_string fmt " " in
  str_with_sep ~pp_sep fmt s

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

let fmt_code_block_tag fmt = function
  | `Tag s -> fpf fmt "Tag(%a)" (ign_loc str) s
  | `Binding (a, b) ->
      fpf fmt "Binding(%a, %a)" (ign_loc str) a (ign_loc str) b

let rec odoc_nestable_block_element c fmt : Ast.nestable_block_element -> _ =
  function
  | `Paragraph elms -> fpf fmt "Paragraph(%a)" odoc_inline_elements elms
  | `Code_block (b : Ast.code_block) ->
      let fmt_metadata fmt (m : Ast.code_block_meta) =
        fpf fmt "(%a, %a)" (ign_loc str) m.language (list fmt_code_block_tag)
          m.tags
      in
      let fmt_content =
        let pp_sep _ () = () in
        ign_loc (fun fmt s -> str_with_sep ~pp_sep fmt (c.normalize_code s))
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
  | `Toc_status txt ->
      fpf fmt "Toc_status(%a)" (odoc_nestable_block_elements c) txt
  | `Order_category txt ->
      fpf fmt "Order_category(%a)" (odoc_nestable_block_elements c) txt
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
