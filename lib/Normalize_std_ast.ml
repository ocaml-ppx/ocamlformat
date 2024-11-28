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

open Ocamlformat_parser_standard
open Std_ast

type 'a t = 'a Std_ast.t

let is_doc = function
  | {attr_name= {Location.txt= "ocaml.doc" | "ocaml.text"; _}; _} -> true
  | _ -> false

let normalize_code conf txt =
  (* Normalize code blocks in docstrings using the extended AST. This
     correctly handles repl phrases. *)
  Normalize_extended_ast.normalize_code conf txt

let docstring (c : Conf.t) =
  Docstring.normalize ~parse_docstrings:c.fmt_opts.parse_docstrings.v

let sort_attributes : attributes -> attributes =
  List.sort ~compare:Poly.compare

let make_mapper conf ~ignore_doc_comments =
  let open Ast_helper in
  (* remove locations *)
  let location _ _ = Location.none in
  let attribute (m : Ast_mapper.mapper) (attr : attribute) =
    match attr.attr_payload with
    | PStr
        [ ( { pstr_desc=
                Pstr_eval
                  ( ( { pexp_desc=
                          Pexp_constant
                            ( { pconst_desc=
                                  Pconst_string (doc, str_loc, None)
                              ; _ } as const )
                      ; _ } as exp )
                  , [] )
            ; _ } as pstr ) ]
      when is_doc attr ->
        let normalize_code = normalize_code conf in
        let doc' = docstring conf ~normalize_code doc in
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
                                { const with
                                  pconst_desc=
                                    Pconst_string (doc', str_loc, None) }
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
    Ast_mapper.default_mapper.attributes m atrs |> sort_attributes
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
    | Ppat_constraint (pat1, {ptyp_desc= Ptyp_poly ([], _t); _}) ->
        (* The parser put the same type constraint in two different nodes:
           [let _ : typ = exp] is represented as [let _ : typ = (exp :
           typ)]. *)
        m.pat m pat1
    | _ -> Ast_mapper.default_mapper.pat m pat
  in
  let typ (m : Ast_mapper.mapper) typ =
    let typ = {typ with ptyp_loc_stack= []} in
    Ast_mapper.default_mapper.typ m typ
  in
  let structure =
    if ignore_doc_comments then fun (m : Ast_mapper.mapper) l ->
      List.filter l ~f:(function
        | {pstr_desc= Pstr_attribute a; _} -> not (is_doc a)
        | _ -> true )
      |> Ast_mapper.default_mapper.structure m
    else Ast_mapper.default_mapper.structure
  in
  let signature =
    if ignore_doc_comments then fun (m : Ast_mapper.mapper) l ->
      List.filter l ~f:(function
        | {psig_desc= Psig_attribute a; _} -> not (is_doc a)
        | _ -> true )
      |> Ast_mapper.default_mapper.signature m
    else Ast_mapper.default_mapper.signature
  in
  let class_structure =
    if ignore_doc_comments then fun (m : Ast_mapper.mapper) x ->
      let pcstr_fields =
        List.filter x.pcstr_fields ~f:(function
          | {pcf_desc= Pcf_attribute a; _} -> not (is_doc a)
          | _ -> true )
      in
      Ast_mapper.default_mapper.class_structure m {x with pcstr_fields}
    else Ast_mapper.default_mapper.class_structure
  in
  let class_signature =
    if ignore_doc_comments then fun (m : Ast_mapper.mapper) x ->
      let pcsig_fields =
        List.filter x.pcsig_fields ~f:(function
          | {pctf_desc= Pctf_attribute a; _} -> not (is_doc a)
          | _ -> true )
      in
      Ast_mapper.default_mapper.class_signature m {x with pcsig_fields}
    else Ast_mapper.default_mapper.class_signature
  in
  { Ast_mapper.default_mapper with
    location
  ; attribute
  ; attributes
  ; structure
  ; signature
  ; class_signature
  ; class_structure
  ; expr
  ; pat
  ; typ }

let ast fragment ~ignore_doc_comments c =
  map fragment (make_mapper c ~ignore_doc_comments)

let equal fragment ~ignore_doc_comments c ast1 ast2 =
  let map = ast fragment c ~ignore_doc_comments in
  equal fragment (map ast1) (map ast2)

let ast = ast ~ignore_doc_comments:false

let make_docstring_mapper docstrings =
  let attribute (m : Ast_mapper.mapper) attr =
    match (attr.attr_name, attr.attr_payload) with
    | ( {txt= "ocaml.doc" | "ocaml.text"; loc}
      , PStr
          [ { pstr_desc=
                Pstr_eval
                  ( { pexp_desc=
                        Pexp_constant
                          {pconst_desc= Pconst_string (doc, _, None); _}
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
    Ast_mapper.default_mapper.attributes m atrs |> sort_attributes
  in
  {Ast_mapper.default_mapper with attribute; attributes}

let docstrings (type a) (fragment : a t) s =
  let docstrings = ref [] in
  let (_ : a) = map fragment (make_docstring_mapper docstrings) s in
  !docstrings

let docstring conf =
  let normalize_code = normalize_code conf in
  docstring conf ~normalize_code

let moved_docstrings fragment c s1 s2 =
  let d1 = docstrings fragment s1 in
  let d2 = docstrings fragment s2 in
  let equal (_, x) (_, y) = String.equal (docstring c x) (docstring c y) in
  let cmt_kind = `Doc_comment in
  let cmt (loc, x) = Cmt.create_docstring x loc in
  let dropped x = {Cmt.kind= `Dropped (cmt x); cmt_kind} in
  let added x = {Cmt.kind= `Added (cmt x); cmt_kind} in
  let modified (x, y) = {Cmt.kind= `Modified (cmt x, cmt y); cmt_kind} in
  match List.zip d1 d2 with
  | Unequal_lengths ->
      (* We only return the ones that are not in both lists. *)
      let l1 = List.filter d1 ~f:(fun x -> not (List.mem ~equal d2 x)) in
      let l1 = List.map ~f:dropped l1 in
      let l2 = List.filter d2 ~f:(fun x -> not (List.mem ~equal d1 x)) in
      let l2 = List.map ~f:added l2 in
      List.rev_append l1 l2
  | Ok l ->
      let l = List.filter l ~f:(fun (x, y) -> not (equal x y)) in
      List.map ~f:modified l
