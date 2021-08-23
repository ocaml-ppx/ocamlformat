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

type conf =
  {conf: Conf.t; normalize_code: Std_ast.structure -> Std_ast.structure}

let is_doc = function
  | Std_ast.{attr_name= {Location.txt= "ocaml.doc" | "ocaml.text"; _}; _} ->
      true
  | _ -> false

(** Remove comments that duplicate docstrings (or other comments). *)
let dedup_cmts fragment ast comments =
  let open Extended_ast in
  let of_ast ast =
    let docs = ref (Set.empty (module Cmt)) in
    let attribute m atr =
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
          docs := Set.add !docs (Cmt.create ("*" ^ doc) pexp_loc) ;
          atr
      | _ -> Ast_mapper.default_mapper.attribute m atr
    in
    map fragment {Ast_mapper.default_mapper with attribute} ast |> ignore ;
    !docs
  in
  Set.(to_list (diff (of_list (module Cmt) comments) (of_ast ast)))

let dedup_cmts_std fragment ast comments =
  let open Std_ast in
  let open Ocaml_413 in
  let of_ast ast =
    let docs = ref (Set.empty (module Cmt)) in
    let attribute m atr =
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
        when is_doc atr ->
          docs := Set.add !docs (Cmt.create ("*" ^ doc) pexp_loc) ;
          atr
      | _ -> Ast_mapper.default_mapper.attribute m atr
    in
    map fragment {Ast_mapper.default_mapper with attribute} ast |> ignore ;
    !docs
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
      let txt =
        try
          let ({ast; comments; _} : _ Parse_with_comments.with_comments) =
            Parse_with_comments.parse Std_ast.Parse.ast Structure c.conf
              ~source:txt
          in
          let comments = dedup_cmts_std Structure ast comments in
          let print_comments fmt (l : Cmt.t list) =
            List.sort l ~compare:(fun {Cmt.loc= a; _} {Cmt.loc= b; _} ->
                Migrate_ast.Location.compare a b )
            |> List.iter ~f:(fun {Cmt.txt; _} ->
                   Caml.Format.fprintf fmt "%s," txt )
          in
          let ast = c.normalize_code ast in
          Caml.Format.asprintf "AST,%a,COMMENTS,[%a]"
            Std_ast.Pprintast.structure ast print_comments comments
        with _ -> txt
      in
      fpf fmt "Code_block(%a, %a)" (option (ign_loc str)) metadata str txt
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

let docstring c text =
  if not c.conf.parse_docstrings then comment text
  else
    let location = Lexing.dummy_pos in
    let parsed = Odoc_parser.parse_comment ~location ~text in
    Format.asprintf "Docstring(%a)%!" (odoc_docs c) (Odoc_parser.ast parsed)

let sort_attributes : Std_ast.attributes -> Std_ast.attributes =
  List.sort ~compare:Poly.compare

let make_mapper conf ~ignore_doc_comments =
  let open Std_ast in
  let open Ocaml_413 in
  let open Ast_helper in
  (* remove locations *)
  let location _ _ = Location.none in
  let attribute (m : Ast_mapper.mapper) (attr : attribute) =
    match attr.attr_payload with
    | PStr
        [ ( { pstr_desc=
                Pstr_eval
                  ( ( { pexp_desc=
                          Pexp_constant (Pconst_string (doc, str_loc, None))
                      ; _ } as exp )
                  , [] )
            ; _ } as pstr ) ]
      when is_doc attr ->
        let doc' = docstring {conf; normalize_code= m.structure m} doc in
        Ast_mapper.default_mapper.attribute m
          { attr with
            attr_payload=
              PStr
                [ { pstr with
                    pstr_desc=
                      Pstr_eval
                        ( { exp with
                            pexp_desc=
                              Pexp_constant
                                (Pconst_string (doc', str_loc, None))
                          ; pexp_loc_stack= [] }
                        , [] ) } ] }
    | _ -> Ast_mapper.default_mapper.attribute m attr
  in
  (* sort attributes *)
  let attributes (m : Ast_mapper.mapper) (atrs : attribute list) =
    let atrs =
      if ignore_doc_comments then
        List.filter atrs ~f:(fun a -> not (is_doc a))
      else atrs
    in
    Ast_mapper.default_mapper.attributes m (sort_attributes atrs)
  in
  let expr (m : Ast_mapper.mapper) exp =
    let exp = {exp with pexp_loc_stack= []} in
    let {pexp_desc; pexp_loc= loc1; pexp_attributes= attrs1; _} = exp in
    match pexp_desc with
    | Pexp_poly ({pexp_desc= Pexp_constraint (e, t); _}, None) ->
        m.expr m {exp with pexp_desc= Pexp_poly (e, Some t)}
    | Pexp_constraint (e, {ptyp_desc= Ptyp_poly ([], _t); _}) -> m.expr m e
    | Pexp_sequence
        ( exp1
        , { pexp_desc= Pexp_sequence (exp2, exp3)
          ; pexp_loc= loc2
          ; pexp_attributes= attrs2
          ; _ } ) ->
        m.expr m
          (Exp.sequence ~loc:loc1 ~attrs:attrs1
             (Exp.sequence ~loc:loc2 ~attrs:attrs2 exp1 exp2)
             exp3 )
    | _ -> Ast_mapper.default_mapper.expr m exp
  in
  let pat (m : Ast_mapper.mapper) pat =
    let pat = {pat with ppat_loc_stack= []} in
    let {ppat_desc; ppat_loc= loc1; ppat_attributes= attrs1; _} = pat in
    (* normalize nested or patterns *)
    match ppat_desc with
    | Ppat_or
        ( pat1
        , { ppat_desc= Ppat_or (pat2, pat3)
          ; ppat_loc= loc2
          ; ppat_attributes= attrs2
          ; _ } ) ->
        m.pat m
          (Pat.or_ ~loc:loc1 ~attrs:attrs1
             (Pat.or_ ~loc:loc2 ~attrs:attrs2 pat1 pat2)
             pat3 )
    | _ -> Ast_mapper.default_mapper.pat m pat
  in
  let typ (m : Ast_mapper.mapper) typ =
    let typ = {typ with ptyp_loc_stack= []} in
    Ast_mapper.default_mapper.typ m typ
  in
  { Ast_mapper.default_mapper with
    location
  ; attribute
  ; attributes
  ; expr
  ; pat
  ; typ }

let normalize fragment ~ignore_doc_comments c =
  Std_ast.map fragment (make_mapper c ~ignore_doc_comments)

let equal fragment ~ignore_doc_comments c ast1 ast2 =
  let map = normalize fragment c ~ignore_doc_comments in
  Std_ast.equal fragment (map ast1) (map ast2)

let normalize = normalize ~ignore_doc_comments:false

let make_docstring_mapper docstrings =
  let open Std_ast in
  let open Ocaml_413 in
  let attribute (m : Ast_mapper.mapper) attr =
    match (attr.attr_name, attr.attr_payload) with
    | ( {txt= "ocaml.doc" | "ocaml.text"; loc}
      , PStr
          [ { pstr_desc=
                Pstr_eval
                  ( { pexp_desc= Pexp_constant (Pconst_string (doc, _, None))
                    ; _ }
                  , [] )
            ; _ } ] ) ->
        docstrings := (loc, doc) :: !docstrings ;
        attr
    | _ -> Ast_mapper.default_mapper.attribute m attr
  in
  (* sort attributes *)
  let attributes (m : Ast_mapper.mapper) atrs =
    let atrs = List.filter atrs ~f:is_doc in
    Ast_mapper.default_mapper.attributes m (sort_attributes atrs)
  in
  {Ast_mapper.default_mapper with attribute; attributes}

let docstrings (type a) (fragment : a Std_ast.t) s =
  let docstrings = ref [] in
  let (_ : a) = Std_ast.map fragment (make_docstring_mapper docstrings) s in
  !docstrings

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
