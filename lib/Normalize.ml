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

(** Normalize abstract syntax trees *)

open Migrate_ast
open Asttypes
open Ast_passes
open Ast_helper

type conf =
  {conf: Conf.t; normalize_code: Ast_final.structure -> Ast_final.structure}

(** Remove comments that duplicate docstrings (or other comments). *)
let dedup_cmts fragment ast comments =
  let of_ast ast =
    let iter =
      object
        inherit [Set.M(Cmt).t] Ppxlib.Ast_traverse.fold as super

        method! attribute atr docs =
          match atr with
          | { attr_payload=
                PStr
                  [ { pstr_desc=
                        Pstr_eval
                          ( { pexp_desc=
                                Pexp_constant (Pconst_string (doc, _, None))
                            ; pexp_loc
                            ; _ }
                          , [] )
                    ; _ } ]
            ; _ }
            when Ast.Attr.is_doc atr ->
              Set.add docs (Cmt.create ("*" ^ doc) pexp_loc)
          | _ -> super#attribute atr docs
      end
    in
    Ast_final.fold fragment iter ast (Set.empty (module Cmt))
  in
  Set.(to_list (diff (of_list (module Cmt) comments) (of_ast ast)))

let comment s =
  (* normalize consecutive whitespace chars to a single space *)
  String.concat ~sep:" "
    (List.filter ~f:(Fn.non String.is_empty)
       (String.split_on_chars s ~on:['\t'; '\n'; '\011'; '\012'; '\r'; ' ']) )

let list f fmt l =
  let pp_sep fmt () = Format.fprintf fmt "" in
  Format.pp_print_list ~pp_sep f fmt l

let str fmt s = Format.fprintf fmt "%s" (comment s)

let ign_loc f fmt with_loc = f fmt with_loc.Odoc_model.Location_.value

let fpf = Format.fprintf

open Odoc_parser.Ast

let odoc_reference = ign_loc str

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
      fpf fmt "Word,%a" str txt
  | `Code_span txt -> fpf fmt "Code_span,%a" str txt
  | `Raw_markup (Some lang, txt) -> fpf fmt "Raw_html:%s,%a" lang str txt
  | `Raw_markup (None, txt) -> fpf fmt "Raw_html,%a" str txt
  | `Styled (style, elems) ->
      fpf fmt "Styled,%a,%a" odoc_style style odoc_inline_elements elems
  | `Reference (_kind, ref, content) ->
      fpf fmt "Reference,%a,%a" odoc_reference ref odoc_inline_elements
        content
  | `Link (txt, content) ->
      fpf fmt "Link,%a,%a" str txt odoc_inline_elements content

and odoc_inline_elements fmt elems =
  list (ign_loc odoc_inline_element) fmt elems

let rec odoc_nestable_block_element c fmt = function
  | `Paragraph elms -> fpf fmt "Paragraph,%a" odoc_inline_elements elms
  | `Code_block txt ->
      let txt =
        try
          let ({ast; comments; _} : _ Parse_with_comments.with_comments) =
            Parse_with_comments.parse Ast_passes.Ast0.Parse.ast Structure
              c.conf ~source:txt
          in
          let ast = Ast_passes.run Structure Structure ast in
          let comments = dedup_cmts Structure ast comments in
          let print_comments fmt (l : Cmt.t list) =
            List.sort l ~compare:(fun {Cmt.loc= a; _} {Cmt.loc= b; _} ->
                Location.compare a b )
            |> List.iter ~f:(fun {Cmt.txt; _} ->
                   Caml.Format.fprintf fmt "%s," txt )
          in
          let ast = c.normalize_code ast in
          Caml.Format.asprintf "AST,%a,COMMENTS,[%a]"
            Ast_passes.Ast_final.Pprintast.structure ast print_comments
            comments
        with _ -> txt
      in
      fpf fmt "Code_block,%a" str txt
  | `Verbatim txt -> fpf fmt "Verbatim,%a" str txt
  | `Modules mods -> fpf fmt "Modules,%a" (list odoc_reference) mods
  | `List (ord, _syntax, items) ->
      let ord = match ord with `Unordered -> "U" | `Ordered -> "O" in
      let list_item fmt elems =
        fpf fmt "Item(%a)" (odoc_nestable_block_elements c) elems
      in
      fpf fmt "List,%s,%a" ord (list list_item) items

and odoc_nestable_block_elements c fmt elems =
  list (ign_loc (odoc_nestable_block_element c)) fmt elems

let odoc_tag c fmt = function
  | `Author txt -> fpf fmt "Author,%a" str txt
  | `Deprecated elems ->
      fpf fmt "Deprecated,%a" (odoc_nestable_block_elements c) elems
  | `Param (p, elems) ->
      fpf fmt "Param,%a,%a" str p (odoc_nestable_block_elements c) elems
  | `Raise (p, elems) ->
      fpf fmt "Raise,%a,%a" str p (odoc_nestable_block_elements c) elems
  | `Return elems ->
      fpf fmt "Return,%a" (odoc_nestable_block_elements c) elems
  | `See (kind, txt, elems) ->
      let kind =
        match kind with `Url -> "U" | `File -> "F" | `Document -> "D"
      in
      fpf fmt "See,%s,%a,%a" kind str txt
        (odoc_nestable_block_elements c)
        elems
  | `Since txt -> fpf fmt "Since,%a" str txt
  | `Before (p, elems) ->
      fpf fmt "Before,%a,%a" str p (odoc_nestable_block_elements c) elems
  | `Version txt -> fpf fmt "Version,%a" str txt
  | `Canonical ref -> fpf fmt "Canonical,%a" odoc_reference ref
  | `Inline -> fpf fmt "Inline"
  | `Open -> fpf fmt "Open"
  | `Closed -> fpf fmt "Closed"

let odoc_block_element c fmt = function
  | `Heading (lvl, lbl, content) ->
      let lvl = Int.to_string lvl in
      let lbl = match lbl with Some lbl -> lbl | None -> "" in
      fpf fmt "Heading,%s,%a,%a" lvl str lbl odoc_inline_elements content
  | `Tag tag -> fpf fmt "Tag,%a" (odoc_tag c) tag
  | #nestable_block_element as elm -> odoc_nestable_block_element c fmt elm

let odoc_docs c fmt elems = list (ign_loc (odoc_block_element c)) fmt elems

let docstring c text =
  if not c.conf.parse_docstrings then comment text
  else
    let location = Lexing.dummy_pos in
    let parsed = Odoc_parser.parse_comment_raw ~location ~text in
    Format.asprintf "Docstring(%a)%!" (odoc_docs c)
      parsed.Odoc_model.Error.value

let sort_attributes : Ast_final.attributes -> Ast_final.attributes =
  List.sort ~compare:Poly.compare

let make_mapper conf ~ignore_doc_comments =
  let open Ast_final in
  object (self)
    inherit map as super

    (** Remove locations *)
    method! location _ = Location.none

    method! attribute attr =
      match attr.attr_payload with
      | PStr
          [ ( { pstr_desc=
                  Pstr_eval
                    ( ( { pexp_desc=
                            Pexp_constant (Pconst_string (doc, str_loc, None))
                        ; _ } as exp )
                    , [] )
              ; _ } as pstr ) ]
        when Ast.Attr.is_doc attr ->
          let doc' = docstring {conf; normalize_code= self#structure} doc in
          super#attribute
            { attr with
              attr_payload=
                PStr
                  [ { pstr with
                      pstr_desc=
                        Pstr_eval
                          ( { exp with
                              pexp_desc=
                                Pexp_constant
                                  (Pconst_string (doc', str_loc, None)) }
                          , [] ) } ] }
      | _ -> super#attribute attr

    (** Sort attributes *)
    method! attributes atrs =
      let atrs =
        if ignore_doc_comments then
          List.filter atrs ~f:(fun a -> not (Ast.Attr.is_doc a))
        else atrs
      in
      super#attributes (sort_attributes atrs)

    method! expression exp =
      match exp.pexp_desc with
      | Pexp_poly ({pexp_desc= Pexp_constraint (e, t); _}, None) ->
          self#expression {exp with pexp_desc= Pexp_poly (e, Some t)}
      | _ -> super#expression exp

    method! location_stack _ = []

    method! pattern pat =
      let {ppat_desc; ppat_loc= loc1; ppat_attributes= attrs1; _} = pat in
      (* normalize nested or patterns *)
      match ppat_desc with
      | Ppat_or
          ( pat1
          , { ppat_desc= Ppat_or (pat2, pat3)
            ; ppat_loc= loc2
            ; ppat_attributes= attrs2
            ; _ } ) ->
          self#pattern
            (Pat.or_ ~loc:loc1 ~attrs:attrs1
               (Pat.or_ ~loc:loc2 ~attrs:attrs2 pat1 pat2)
               pat3 )
      | _ -> super#pattern pat
  end

let normalize fragment ~ignore_doc_comments c =
  Ast_final.map fragment (make_mapper c ~ignore_doc_comments)

let equal fragment ~ignore_doc_comments c ast1 ast2 =
  let map = normalize fragment c ~ignore_doc_comments in
  Ast_final.equal fragment (map ast1) (map ast2)

let normalize = normalize ~ignore_doc_comments:false

let fold_docstrings =
  object
    inherit [(Location.t * string) list] Ppxlib.Ast_traverse.fold as super

    method! attribute attr docstrings =
      match (attr.attr_name, attr.attr_payload) with
      | ( {txt= "ocaml.doc" | "ocaml.text"; loc}
        , PStr
            [ { pstr_desc=
                  Pstr_eval
                    ( { pexp_desc=
                          Pexp_constant (Pconst_string (doc, _, None))
                      ; _ }
                    , [] )
              ; _ } ] ) ->
          (loc, doc) :: docstrings
      | _ -> super#attribute attr docstrings

    method! attributes atrs =
      let atrs = List.filter atrs ~f:Ast.Attr.is_doc in
      super#attributes (sort_attributes atrs)
  end

let docstrings (type a) (fragment : a Ast_final.t) s =
  Ast_final.fold fragment fold_docstrings s []

type docstring_error =
  | Moved of Location.t * Location.t * string
  | Unstable of Location.t * string * string
  | Added of Location.t * string
  | Removed of Location.t * string

let moved_docstrings fragment c s1 s2 =
  let c = {conf= c; normalize_code= normalize Structure c} in
  let d1 = docstrings fragment s1 in
  let d2 = docstrings fragment s2 in
  let equal (_, x) (_, y) = String.equal (docstring c x) (docstring c y) in
  match List.zip d1 d2 with
  | Unequal_lengths ->
      (* We only return the ones that are not in both lists. *)
      let l1 = List.filter d1 ~f:(fun x -> not (List.mem ~equal d2 x)) in
      let l1 = List.map ~f:(fun (loc, x) -> Removed (loc, x)) l1 in
      let l2 = List.filter d2 ~f:(fun x -> not (List.mem ~equal d1 x)) in
      let l2 = List.map ~f:(fun (loc, x) -> Added (loc, x)) l2 in
      List.rev_append l1 l2
  | Ok l ->
      let l = List.filter l ~f:(fun (x, y) -> not (equal x y)) in
      List.map ~f:(fun ((loc, x), (_, y)) -> Unstable (loc, x, y)) l

let docstring conf =
  let normalize_code = normalize Structure conf in
  docstring {conf; normalize_code}
