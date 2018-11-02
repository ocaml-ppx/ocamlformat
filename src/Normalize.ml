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

let docstring s =
  match Octavius.parse (Lexing.from_string s) with
  | Ok parsed ->
      Format_.asprintf "%a@!" (fun fs x -> Fmt_odoc.fmt x fs) parsed
  | Error _ ->
      (* normalize consecutive whitespace chars to a single space *)
      String.concat ~sep:" "
        (List.filter ~f:(Fn.non String.is_empty)
           (String.split_on_chars s
              ~on:['\t'; '\n'; '\011'; '\012'; '\r'; ' ']))

let make_mapper ~ignore_doc_comment =
  (* remove locations *)
  let location _ _ = Location.none in
  let doc_attribute = function
    | {txt= "ocaml.doc" | "ocaml.text"; _}, _ -> true
    | _ -> false
  in
  let attribute (m : Ast_mapper.mapper) attr =
    match attr with
    | ( {txt= ("ocaml.doc" | "ocaml.text") as txt; loc}
      , PStr
          [ { pstr_desc=
                Pstr_eval
                  ( { pexp_desc= Pexp_constant (Pconst_string (doc, None))
                    ; pexp_loc
                    ; pexp_attributes }
                  , [] )
            ; pstr_loc } ] ) ->
        let doc' =
          if ignore_doc_comment then "IGNORED" else docstring doc
        in
        ( {txt; loc= m.location m loc}
        , m.payload m
            (PStr
               [ { pstr_desc=
                     Pstr_eval
                       ( { pexp_desc=
                             Pexp_constant (Pconst_string (doc', None))
                         ; pexp_loc= m.location m pexp_loc
                         ; pexp_attributes= m.attributes m pexp_attributes
                         }
                       , [] )
                 ; pstr_loc= m.location m pstr_loc } ]) )
    | attr -> Ast_mapper.default_mapper.attribute m attr
  in
  (* sort attributes *)
  let attributes (m : Ast_mapper.mapper) atrs =
    let atrs =
      if ignore_doc_comment then
        List.filter atrs ~f:(fun a -> not (doc_attribute a))
      else atrs
    in
    Ast_mapper.default_mapper.attributes m
      (List.sort ~compare:Poly.compare atrs)
  in
  let expr (m : Ast_mapper.mapper) exp =
    let {pexp_desc; pexp_attributes} = exp in
    match pexp_desc with
    (* convert [(c1; c2); c3] to [c1; (c2; c3)] *)
    | Pexp_sequence
        ({pexp_desc= Pexp_sequence (e1, e2); pexp_attributes= []}, e3) ->
        m.expr m
          (Exp.sequence e1 (Exp.sequence ~attrs:pexp_attributes e2 e3))
    | Pexp_poly ({pexp_desc= Pexp_constraint (e, t)}, None) ->
        m.expr m {exp with pexp_desc= Pexp_poly (e, Some t)}
    | Pexp_constraint (e, {ptyp_desc= Ptyp_poly ([], _t)}) -> m.expr m e
    | _ -> Ast_mapper.default_mapper.expr m exp
  in
  let pat (m : Ast_mapper.mapper) pat =
    let {ppat_desc; ppat_loc= loc1; ppat_attributes= attrs1} = pat in
    (* normalize nested or patterns *)
    match ppat_desc with
    | Ppat_or
        ( pat1
        , { ppat_desc= Ppat_or (pat2, pat3)
          ; ppat_loc= loc2
          ; ppat_attributes= attrs2 } ) ->
        m.pat m
          (Pat.or_ ~loc:loc1 ~attrs:attrs1
             (Pat.or_ ~loc:loc2 ~attrs:attrs2 pat1 pat2)
             pat3)
    | _ -> Ast_mapper.default_mapper.pat m pat
  in
  let value_binding (m : Ast_mapper.mapper) vb =
    let { pvb_pat= {ppat_desc; ppat_loc; ppat_attributes}
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
          (({ppat_desc= Ppat_var _} as p0), {ptyp_desc= Ptyp_poly ([], t0)})
      , Pexp_constraint (e0, t1) )
      when Poly.equal t0 t1 ->
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
    | Pstr_eval ({pexp_desc= Pexp_extension e}, []) ->
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
            | _ -> true )
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
            | _ -> true )
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
              | _ -> true )
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
              | _ -> true )
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

let impl = map_structure mapper

let intf = map_signature mapper

let use_file = map_use_file mapper

let equal_impl ~ignore_doc_comments ast1 ast2 =
  let map =
    if ignore_doc_comments then map_structure mapper_ignore_doc_comment
    else map_structure mapper
  in
  Poly.equal (map ast1) (map ast2)

let equal_intf ~ignore_doc_comments ast1 ast2 =
  let map =
    if ignore_doc_comments then map_signature mapper_ignore_doc_comment
    else map_signature mapper
  in
  Poly.equal (map ast1) (map ast2)

let equal_use_file ~ignore_doc_comments ast1 ast2 =
  let map =
    if ignore_doc_comments then map_use_file mapper_ignore_doc_comment
    else map_use_file mapper
  in
  Poly.equal (map ast1) (map ast2)

let make_docstring_mapper docstrings =
  let doc_attribute = function
    | {txt= "ocaml.doc" | "ocaml.text"; _}, _ -> true
    | _ -> false
  in
  let attribute (m : Ast_mapper.mapper) attr =
    match attr with
    | ( {txt= ("ocaml.doc" | "ocaml.text") as txt; loc}
      , PStr
          [ { pstr_desc=
                Pstr_eval
                  ( { pexp_desc= Pexp_constant (Pconst_string (doc, None))
                    ; pexp_loc
                    ; pexp_attributes }
                  , [] )
            ; pstr_loc } ] ) ->
        let doc' = docstring doc in
        docstrings := (loc, doc') :: !docstrings ;
        ( {txt; loc}
        , m.payload m
            (PStr
               [ { pstr_desc=
                     Pstr_eval
                       ( { pexp_desc=
                             Pexp_constant (Pconst_string (doc', None))
                         ; pexp_loc
                         ; pexp_attributes }
                       , [] )
                 ; pstr_loc } ]) )
    | attr -> Ast_mapper.default_mapper.attribute m attr
  in
  (* sort attributes *)
  let attributes (m : Ast_mapper.mapper) atrs =
    let atrs = List.filter atrs ~f:doc_attribute in
    Ast_mapper.default_mapper.attributes m
      (List.sort ~compare:Poly.compare atrs)
  in
  {Ast_mapper.default_mapper with attribute; attributes}

let docstrings_impl s =
  let docstrings = ref [] in
  let _ : structure = map_structure (make_docstring_mapper docstrings) s in
  !docstrings

let docstrings_intf s =
  let docstrings = ref [] in
  let _ : signature = map_signature (make_docstring_mapper docstrings) s in
  !docstrings

let docstrings_use_file s =
  let docstrings = ref [] in
  let _ : toplevel_phrase list =
    map_use_file (make_docstring_mapper docstrings) s
  in
  !docstrings

let moved_docstrings get_docstrings s1 s2 =
  let d1 = get_docstrings s1 in
  let d2 = get_docstrings s2 in
  let equal (_, x) (_, y) = String.equal (docstring x) (docstring y) in
  match List.zip d1 d2 with
  | None ->
      (* We only return the ones that are not in both lists. *)
      (* [l1] contains the ones that disappeared. *)
      let l1 = List.filter d1 ~f:(fun x -> not (List.mem ~equal d2 x)) in
      let l1 = List.map ~f:(fun (l, s) -> (l, Location.none, s)) l1 in
      (* [l2] contains the ones that appeared. *)
      let l2 = List.filter d2 ~f:(fun x -> not (List.mem ~equal d1 x)) in
      let l2 = List.map ~f:(fun (l, s) -> (Location.none, l, s)) l2 in
      List.rev_append l1 l2
  | Some l ->
      let l = List.filter l ~f:(fun (x, y) -> not (equal x y)) in
      let d1, d2 = List.unzip l in
      List.map d1 ~f:(fun (l, s) ->
          let new_loc =
            List.find d2 ~f:(equal (l, s))
            |> Option.value_map ~default:Location.none ~f:fst
          in
          (l, new_loc, s) )

let moved_docstrings_impl s1 s2 = moved_docstrings docstrings_impl s1 s2

let moved_docstrings_intf s1 s2 = moved_docstrings docstrings_intf s1 s2

let moved_docstrings_use_file s1 s2 =
  moved_docstrings docstrings_use_file s1 s2
