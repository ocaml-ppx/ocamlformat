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

(** Normalize abstract syntax trees *)

open Migrate_ast
open Asttypes
open Parsetree
open Ast_helper

let comment s =
  (* normalize consecutive whitespace chars to a single space *)
  String.concat ~sep:" "
    (List.filter ~f:(Fn.non String.is_empty)
       (String.split_on_chars s ~on:['\t'; '\n'; '\011'; '\012'; '\r'; ' ']))

let list f fmt l =
  let pp_sep fmt () = Format.fprintf fmt "" in
  Format.pp_print_list ~pp_sep f fmt l

let str fmt s = Format.fprintf fmt "%s" (comment s)

let opt f fmt = function
  | Some x -> Format.fprintf fmt "Some(%a)" f x
  | None -> Format.fprintf fmt "None"

open Octavius.Types

let ref_kind fmt rk =
  let ref_kind fmt = function
    | RK_element -> Format.fprintf fmt "RK_element"
    | RK_module -> Format.fprintf fmt "RK_module"
    | RK_module_type -> Format.fprintf fmt "RK_module_type"
    | RK_class -> Format.fprintf fmt "RK_class"
    | RK_class_type -> Format.fprintf fmt "RK_class_type"
    | RK_value -> Format.fprintf fmt "RK_value"
    | RK_type -> Format.fprintf fmt "RK_type"
    | RK_exception -> Format.fprintf fmt "RK_exception"
    | RK_attribute -> Format.fprintf fmt "RK_attribute"
    | RK_method -> Format.fprintf fmt "RK_method"
    | RK_section -> Format.fprintf fmt "RK_section"
    | RK_recfield -> Format.fprintf fmt "RK_recfield"
    | RK_const -> Format.fprintf fmt "RK_const"
    | RK_link -> Format.fprintf fmt "RK_link"
    | RK_custom s -> Format.fprintf fmt "RK_custom,%a" str s
  in
  Format.fprintf fmt "RK(%a)" ref_kind rk

let style fmt s =
  let style fmt = function
    | SK_bold -> Format.fprintf fmt "b"
    | SK_italic -> Format.fprintf fmt "i"
    | SK_emphasize -> Format.fprintf fmt "e"
    | SK_center -> Format.fprintf fmt "C"
    | SK_left -> Format.fprintf fmt "L"
    | SK_right -> Format.fprintf fmt "R"
    | SK_superscript -> Format.fprintf fmt "^"
    | SK_subscript -> Format.fprintf fmt "_"
    | SK_custom s -> Format.fprintf fmt "Custom,%a" str s
  in
  Format.fprintf fmt "Style(%a)" style s

let rec odoc_text_elt fmt = function
  | Raw s ->
      let s = Format.asprintf "%a" str s in
      if String.equal s "" then Format.fprintf fmt ""
      else Format.fprintf fmt "Raw,%s" s
  | Code s -> Format.fprintf fmt "Code,%a" str s
  | PreCode s -> Format.fprintf fmt "PreCode,%a" str s
  | Verbatim s -> Format.fprintf fmt "Verbatim,%a" str s
  | Style (st, txt) ->
      Format.fprintf fmt "Style,%a,%a" style st odoc_text txt
  | List l -> Format.fprintf fmt "List,%a" (list odoc_text) l
  | Enum l -> Format.fprintf fmt "Enum,%a" (list odoc_text) l
  | Newline -> Format.fprintf fmt ""
  | Title (i, s, txt) ->
      Format.fprintf fmt "Title,%i,%a,%a" i (opt str) s odoc_text txt
  | Ref (rk, s, txt) ->
      Format.fprintf fmt "Ref,%a,%a,%a" ref_kind rk str s (opt odoc_text)
        txt
  | Special_ref (SRK_module_list l) ->
      Format.fprintf fmt "Special_ref,SRK_module_list(%a)" (list str) l
  | Special_ref SRK_index_list ->
      Format.fprintf fmt "Special_ref,SRK_index_list"
  | Target (s, l) -> Format.fprintf fmt "Target,%a,%a" (opt str) s str l

and odoc_text fmt t = Format.fprintf fmt "%a" (list odoc_text_elt) t

let see_ref fmt sr =
  let see_ref fmt = function
    | See_url s -> Format.fprintf fmt "See_url,%a" str s
    | See_file s -> Format.fprintf fmt "See_file,%a" str s
    | See_doc s -> Format.fprintf fmt "See_doc,%a" str s
  in
  Format.fprintf fmt "See_ref(%a)" see_ref sr

let odoc_tag fmt t =
  let tag fmt = function
    | Author s -> Format.fprintf fmt "Author,%a" str s
    | Version s -> Format.fprintf fmt "Version,%a" str s
    | See (sr, txt) ->
        Format.fprintf fmt "See,%a,%a" see_ref sr odoc_text txt
    | Since s -> Format.fprintf fmt "Since,%a" str s
    | Before (s, txt) ->
        Format.fprintf fmt "Before,%a,%a" str s odoc_text txt
    | Deprecated txt -> Format.fprintf fmt "Deprecated,%a" odoc_text txt
    | Param (s, txt) -> Format.fprintf fmt "Param,%a,%a" str s odoc_text txt
    | Raised_exception (s, txt) ->
        Format.fprintf fmt "Raised_exception,%a,%a" str s odoc_text txt
    | Return_value txt -> Format.fprintf fmt "Return_value,%a" odoc_text txt
    | Inline -> Format.fprintf fmt "Inline"
    | Custom (s, txt) ->
        Format.fprintf fmt "Custom,%a,%a" str s odoc_text txt
    | Canonical s -> Format.fprintf fmt "Canonical,%a" str s
  in
  Format.fprintf fmt "Tag(%a)" tag t

let docstring c s =
  if not c.Conf.parse_docstrings then comment s
  else
    match Octavius.parse (Lexing.from_string s) with
    | Ok (text, tags) ->
        Format.asprintf "Docstring(%a,%a)%!" odoc_text text (list odoc_tag)
          tags
    | Error _ -> comment s

let sort_attributes : attributes -> attributes =
  List.sort ~compare:Poly.compare

let make_mapper c ~ignore_doc_comment =
  (* remove locations *)
  let location _ _ = Location.none in
  let doc_attribute = function
    | {attr_name= {txt= "ocaml.doc" | "ocaml.text"; _}; _} -> true
    | _ -> false
  in
  let attribute (m : Ast_mapper.mapper) (attr : attribute) =
    match (attr.attr_name, attr.attr_payload) with
    | ( {txt= ("ocaml.doc" | "ocaml.text") as txt; loc}
      , PStr
          [ { pstr_desc=
                Pstr_eval
                  ( { pexp_desc= Pexp_constant (Pconst_string (doc, None))
                    ; pexp_loc
                    ; pexp_attributes
                    ; _ }
                  , [] )
            ; pstr_loc } ] ) ->
        let doc' =
          if ignore_doc_comment then "IGNORED" else docstring c doc
        in
        { attr_name= {txt; loc= m.location m loc}
        ; attr_payload=
            m.payload m
              (PStr
                 [ { pstr_desc=
                       Pstr_eval
                         ( { pexp_desc=
                               Pexp_constant (Pconst_string (doc', None))
                           ; pexp_loc= m.location m pexp_loc
                           ; pexp_attributes= m.attributes m pexp_attributes
                           ; pexp_loc_stack= [] }
                         , [] )
                   ; pstr_loc= m.location m pstr_loc } ])
        ; attr_loc= attr.attr_loc }
    | _ -> Ast_mapper.default_mapper.attribute m attr
  in
  (* sort attributes *)
  let attributes (m : Ast_mapper.mapper) (atrs : attribute list) =
    let atrs =
      if ignore_doc_comment then
        List.filter atrs ~f:(fun a -> not (doc_attribute a))
      else atrs
    in
    Ast_mapper.default_mapper.attributes m (sort_attributes atrs)
  in
  let expr (m : Ast_mapper.mapper) exp =
    let {pexp_desc; pexp_attributes; _} = exp in
    match pexp_desc with
    (* convert [(c1; c2); c3] to [c1; (c2; c3)] *)
    | Pexp_sequence
        ({pexp_desc= Pexp_sequence (e1, e2); pexp_attributes= []; _}, e3) ->
        m.expr m
          (Exp.sequence e1 (Exp.sequence ~attrs:pexp_attributes e2 e3))
    | Pexp_poly ({pexp_desc= Pexp_constraint (e, t); _}, None) ->
        m.expr m {exp with pexp_desc= Pexp_poly (e, Some t)}
    | Pexp_constraint (e, {ptyp_desc= Ptyp_poly ([], _t); _}) -> m.expr m e
    | _ -> Ast_mapper.default_mapper.expr m exp
  in
  let pat (m : Ast_mapper.mapper) pat =
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
             pat3)
    | _ -> Ast_mapper.default_mapper.pat m pat
  in
  let value_binding (m : Ast_mapper.mapper) vb =
    let { pvb_pat= {ppat_desc; ppat_loc; ppat_attributes; _}
        ; pvb_expr
        ; pvb_loc
        ; pvb_attributes } =
      vb
    in
    match (ppat_desc, pvb_expr.pexp_desc) with
    (* recognize and undo the pattern of code introduced by
       ocaml/ocaml@fd0dc6a0fbf73323c37a73ea7e8ffc150059d6ff to fix
       https://caml.inria.fr/mantis/view.php?id=7344 *)
    | ( Ppat_constraint
          ( ({ppat_desc= Ppat_var _; _} as p0)
          , {ptyp_desc= Ptyp_poly ([], t0); _} )
      , Pexp_constraint (e0, t1) )
      when Poly.(t0 = t1) ->
        m.value_binding m
          (Vb.mk ~loc:pvb_loc ~attrs:pvb_attributes p0
             (Exp.constraint_ ~loc:ppat_loc ~attrs:ppat_attributes e0 t0))
    (* convert [let (x : t) = e] to [let x = (e : t)] *)
    | Ppat_constraint (p0, t0), _ ->
        m.value_binding m
          (Vb.mk ~loc:pvb_loc ~attrs:pvb_attributes p0
             (Exp.constraint_ ~loc:ppat_loc ~attrs:ppat_attributes pvb_expr
                t0))
    | _ -> Ast_mapper.default_mapper.value_binding m vb
  in
  let structure_item (m : Ast_mapper.mapper) (si : structure_item) =
    match si.pstr_desc with
    | Pstr_eval ({pexp_desc= Pexp_extension e; _}, []) ->
        let e = m.extension m e in
        let pstr_loc = m.location m si.pstr_loc in
        {pstr_desc= Pstr_extension (e, []); pstr_loc}
    | _ -> Ast_mapper.default_mapper.structure_item m si
  in
  let structure (m : Ast_mapper.mapper) (si : structure) =
    let si =
      if ignore_doc_comment then
        List.filter si ~f:(fun si ->
            match si.pstr_desc with
            | Pstr_attribute a -> not (doc_attribute a)
            | _ -> true)
      else si
    in
    Ast_mapper.default_mapper.structure m si
  in
  let signature (m : Ast_mapper.mapper) (si : signature) =
    let si =
      if ignore_doc_comment then
        List.filter si ~f:(fun si ->
            match si.psig_desc with
            | Psig_attribute a -> not (doc_attribute a)
            | _ -> true)
      else si
    in
    Ast_mapper.default_mapper.signature m si
  in
  let class_signature (m : Ast_mapper.mapper) (si : class_signature) =
    let si =
      if ignore_doc_comment then
        let pcsig_fields =
          List.filter si.pcsig_fields ~f:(fun si ->
              match si.pctf_desc with
              | Pctf_attribute a -> not (doc_attribute a)
              | _ -> true)
        in
        {si with pcsig_fields}
      else si
    in
    Ast_mapper.default_mapper.class_signature m si
  in
  let class_structure (m : Ast_mapper.mapper) (si : class_structure) =
    let si =
      if ignore_doc_comment then
        let pcstr_fields =
          List.filter si.pcstr_fields ~f:(fun si ->
              match si.pcf_desc with
              | Pcf_attribute a -> not (doc_attribute a)
              | _ -> true)
        in
        {si with pcstr_fields}
      else si
    in
    Ast_mapper.default_mapper.class_structure m si
  in
  { Ast_mapper.default_mapper with
    location
  ; attribute
  ; attributes
  ; expr
  ; pat
  ; value_binding
  ; structure_item
  ; signature
  ; structure
  ; class_signature
  ; class_structure }

let mapper_ignore_doc_comment = make_mapper ~ignore_doc_comment:true

let mapper = make_mapper ~ignore_doc_comment:false

let impl c = map_structure (mapper c)

let intf c = map_signature (mapper c)

let map_use_file mapper ast =
  let ast = map_use_file mapper ast in
  List.map ast ~f:(function
    | Ptop_def _ as x -> x
    | Ptop_dir {pdir_name; pdir_arg; _} ->
        let pdir_arg =
          match pdir_arg with
          | None -> None
          | Some a -> Some {a with pdira_loc= Location.none}
        in
        Ptop_dir
          { pdir_name= {pdir_name with loc= Location.none}
          ; pdir_arg
          ; pdir_loc= Location.none })

let use_file c ast = map_use_file (mapper c) ast

let equal_impl ~ignore_doc_comments c ast1 ast2 =
  let map =
    if ignore_doc_comments then map_structure (mapper_ignore_doc_comment c)
    else map_structure (mapper c)
  in
  Poly.(map ast1 = map ast2)

let equal_intf ~ignore_doc_comments c ast1 ast2 =
  let map =
    if ignore_doc_comments then map_signature (mapper_ignore_doc_comment c)
    else map_signature (mapper c)
  in
  Poly.(map ast1 = map ast2)

let equal_use_file ~ignore_doc_comments c ast1 ast2 =
  let map =
    if ignore_doc_comments then map_use_file (mapper_ignore_doc_comment c)
    else map_use_file (mapper c)
  in
  Poly.(map ast1 = map ast2)

let make_docstring_mapper c docstrings =
  let doc_attribute = function
    | {attr_name= {txt= "ocaml.doc" | "ocaml.text"; _}; _} -> true
    | _ -> false
  in
  let attribute (m : Ast_mapper.mapper) attr =
    match (attr.attr_name, attr.attr_payload) with
    | ( {txt= ("ocaml.doc" | "ocaml.text") as txt; loc}
      , PStr
          [ { pstr_desc=
                Pstr_eval
                  ( { pexp_desc= Pexp_constant (Pconst_string (doc, None))
                    ; pexp_loc
                    ; pexp_attributes
                    ; _ }
                  , [] )
            ; pstr_loc } ] ) ->
        let doc' = docstring c doc in
        docstrings := (loc, doc) :: !docstrings ;
        { attr_name= {txt; loc}
        ; attr_payload=
            m.payload m
              (PStr
                 [ { pstr_desc=
                       Pstr_eval
                         ( { pexp_desc=
                               Pexp_constant (Pconst_string (doc', None))
                           ; pexp_loc
                           ; pexp_attributes
                           ; pexp_loc_stack= [] }
                         , [] )
                   ; pstr_loc } ])
        ; attr_loc= attr.attr_loc }
    | _ -> Ast_mapper.default_mapper.attribute m attr
  in
  (* sort attributes *)
  let attributes (m : Ast_mapper.mapper) atrs =
    let atrs = List.filter atrs ~f:doc_attribute in
    Ast_mapper.default_mapper.attributes m (sort_attributes atrs)
  in
  {Ast_mapper.default_mapper with attribute; attributes}

let docstrings_impl c s =
  let docstrings = ref [] in
  let (_ : structure) =
    map_structure (make_docstring_mapper c docstrings) s
  in
  !docstrings

let docstrings_intf c s =
  let docstrings = ref [] in
  let (_ : signature) =
    map_signature (make_docstring_mapper c docstrings) s
  in
  !docstrings

let docstrings_use_file c s =
  let docstrings = ref [] in
  let (_ : toplevel_phrase list) =
    map_use_file (make_docstring_mapper c docstrings) s
  in
  !docstrings

type docstring_error =
  | Moved of Location.t * Location.t * string
  | Unstable of Location.t * string

let moved_docstrings c get_docstrings s1 s2 =
  let d1 = get_docstrings c s1 in
  let d2 = get_docstrings c s2 in
  let equal (_, x) (_, y) = String.equal (docstring c x) (docstring c y) in
  let unstable (x, y) = Unstable (x, y) in
  match List.zip_exn d1 d2 with
  | exception _ ->
      (* We only return the ones that are not in both lists. *)
      (* [l1] contains the ones that disappeared. *)
      let l1 = List.filter d1 ~f:(fun x -> not (List.mem ~equal d2 x)) in
      let l1 = List.map ~f:unstable l1 in
      (* [l2] contains the ones that appeared. *)
      let l2 = List.filter d2 ~f:(fun x -> not (List.mem ~equal d1 x)) in
      let l2 = List.map ~f:unstable l2 in
      List.rev_append l1 l2
  | l ->
      let l = List.filter l ~f:(fun (x, y) -> not (equal x y)) in
      let l1, l2 = List.unzip l in
      let both, l1 =
        List.partition_map l1 ~f:(fun x ->
            match List.find l2 ~f:(equal x) with
            | Some (l, s) -> `Fst (Moved (fst x, l, s))
            | None -> `Snd x)
      in
      let l2 = List.filter l2 ~f:(fun x -> not (List.mem ~equal l1 x)) in
      let l1 = List.map ~f:unstable l1 in
      let l2 = List.map ~f:unstable l2 in
      List.rev_append both (List.rev_append l1 l2)

let moved_docstrings_impl c s1 s2 = moved_docstrings c docstrings_impl s1 s2

let moved_docstrings_intf c s1 s2 = moved_docstrings c docstrings_intf s1 s2

let moved_docstrings_use_file c s1 s2 =
  moved_docstrings c docstrings_use_file s1 s2
