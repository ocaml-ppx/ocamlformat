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

open Parser_standard
open Std_ast

let is_doc = function
  | {attr_name= {Location.txt= "ocaml.doc" | "ocaml.text"; _}; _} -> true
  | _ -> false

let globalized_attr =
  Ast_helper.Attr.mk ~loc:Location.none
    (Location.mknoloc "globalized")
    (PStr [])

(* We rewrite both "[@immediate]" and "[@ocaml.immediate]" into ":
   immediate", which is then turned into just "[@immediate]" if we are
   erasing jane syntax. This means "[@ocaml.immediate]" get rewritten to
   "[@immediate]" in that mode, so we normalize these attributes. *)
let normalize_immediate_attr attrs =
  let overwrite_attr_name attr new_name =
    { attr with
      attr_name= {attr.attr_name with txt= new_name}
    ; attr_payload= PStr [] }
  in
  List.map attrs ~f:(fun attr ->
      match (attr.attr_name.txt, attr.attr_payload) with
      | "ocaml.immediate", PStr [] -> overwrite_attr_name attr "immediate"
      | "ocaml.immediate64", PStr [] ->
          overwrite_attr_name attr "immediate64"
      | _, _ -> attr )

let dedup_cmts fragment ast comments =
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
          docs := Set.add !docs (Cmt.create_docstring doc pexp_loc) ;
          atr
      | _ -> Ast_mapper.default_mapper.attribute m atr
    in
    map fragment {Ast_mapper.default_mapper with attribute} ast |> ignore ;
    !docs
  in
  Set.(to_list (diff (of_list (module Cmt) comments) (of_ast ast)))

let normalize_code conf (m : Ast_mapper.mapper) txt =
  let input_name = "<output>" in
  match
    Parse_with_comments.parse Parse.ast Structure conf ~input_name
      ~source:txt
  with
  | {ast; comments; _} ->
      let comments = dedup_cmts Structure ast comments in
      let print_comments fmt (l : Cmt.t list) =
        List.sort l ~compare:(fun a b ->
            Migrate_ast.Location.compare (Cmt.loc a) (Cmt.loc b) )
        |> List.iter ~f:(fun cmt -> Format.fprintf fmt "%s," (Cmt.txt cmt))
      in
      let ast = m.structure m ast in
      Format.asprintf "AST,%a,COMMENTS,[%a]" Printast.implementation ast
        print_comments comments
  | exception _ -> txt

let docstring (c : Conf.t) =
  Docstring.normalize ~parse_docstrings:c.fmt_opts.parse_docstrings.v

let sort_attributes : attributes -> attributes =
  List.sort ~compare:Poly.compare

let dummy_position ~loc =
  Ast_helper.Exp.ident
    {loc; txt= Ldot (Ldot (Lident "Stdlib", "Lexing"), "dummy_pos")}

let make_mapper conf ~ignore_doc_comments ~erase_jane_syntax =
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
        let normalize_code = normalize_code conf m in
        let doc' =
          docstring conf ~normalize_code ~location:attr.attr_loc ~pro:"(**"
            doc
        in
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
  let map_attributes_no_sort (m : Ast_mapper.mapper) (atrs : attribute list)
      =
    let atrs =
      if erase_jane_syntax then
        List.filter atrs ~f:(fun a ->
            (* CR jane-syntax: When erasing jane syntax, [int -> (int ->
               int)] is reformatted to [int -> int -> int]. This causes the
               removal of [extension.curry] attributes, so these attributes
               should be dropped when normalizing under jane syntax
               erasure. *)
            not (String.equal "extension.curry" a.attr_name.txt) )
      else atrs
    in
    let atrs =
      if ignore_doc_comments then
        List.filter atrs ~f:(fun a -> not (is_doc a))
      else atrs
    in
    Ast_mapper.default_mapper.attributes m atrs
  in
  let attributes (m : Ast_mapper.mapper) (atrs : attribute list) =
    sort_attributes (map_attributes_no_sort m atrs)
  in
  let expr (m : Ast_mapper.mapper) exp =
    let exp =
      { exp with
        pexp_loc_stack= []
      ; pexp_attributes=
          (* CR jane-syntax: This ensures that jane syntax attributes are
             removed *)
          ( exp.pexp_attributes
          |> if erase_jane_syntax then map_attributes_no_sort m else Fn.id )
      }
    in
    let {pexp_desc; pexp_loc= loc1; pexp_attributes= attrs1; _} = exp in
    match pexp_desc with
    | Pexp_apply
        ( {pexp_desc= Pexp_extension ({txt= "extension.exclave"; _}, _); _}
        , [(Nolabel, expr)] )
      when erase_jane_syntax ->
        m.expr m expr
    | Pexp_poly ({pexp_desc= Pexp_constraint (e, Some t, []); _}, None) ->
        m.expr m {exp with pexp_desc= Pexp_poly (e, Some t)}
    | Pexp_constraint (exp1, None, _ :: _) when erase_jane_syntax ->
        (* When erasing jane syntax, if [Pexp_constraint] was only
           constraining based on modes, remove the node entirely instead of
           just making the modes list empty *)
        m.expr m exp1
    | Pexp_constraint (e, Some {ptyp_desc= Ptyp_poly ([], _t); _}, []) ->
        m.expr m e
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
    | Pexp_function
        ( ps
        , c
        , Pfunction_body
            { pexp_desc=
                Pexp_function
                  ( ps'
                  , { mode_annotations= []
                    ; ret_mode_annotations= []
                    ; ret_type_constraint= None }
                  , b' )
            ; _ } ) ->
        m.expr m {exp with pexp_desc= Pexp_function (ps @ ps', c, b')}
    | Pexp_newtype
        (l, kind, {pexp_desc= Pexp_constraint (exp1, Some ty, []); _}) -> (
      (* This is a hack. Our version of ocamlformat rewrites [fun (type a) ->
         (function x -> x)] into [fun (type a) -> function x -> x], but these
         two things parse differently by design. We shouldn't do this, but
         have decided to avoid making ocamlformat work sanely for syntactic
         function arity until upstream does. We should delete this, and other
         similar bits of normalization, when we merge with 5.2
         ocamlforamt. *)
      match exp1.pexp_desc with
      | Pexp_function _ | Pexp_newtype _ ->
          (* We can only perform the rewrite if the newtype isn't the only
             "function argument" *)
          m.expr m
            { exp with
              pexp_desc=
                Pexp_function
                  ( [ { pparam_loc= l.loc
                      ; pparam_desc= Pparam_newtype (l, kind) } ]
                  , { mode_annotations= []
                    ; ret_mode_annotations= []
                    ; ret_type_constraint= Some (Pconstraint ty) }
                  , Pfunction_body exp1 ) }
      | _ -> Ast_mapper.default_mapper.expr m exp )
    | Pexp_newtype (l, _, ty) when erase_jane_syntax ->
        Ast_mapper.default_mapper.expr m
          {exp with pexp_desc= Pexp_newtype (l, None, ty)}
    | Pexp_function
        ( ps
        , { mode_annotations= []
          ; ret_mode_annotations= []
          ; ret_type_constraint= None }
        , Pfunction_body
            {pexp_desc= Pexp_constraint (exp1, Some ty, modes); _} )
      when List.is_empty modes || Erase_jane_syntax.should_erase () ->
        let c =
          { mode_annotations= []
          ; ret_mode_annotations= []
          ; ret_type_constraint= Some (Pconstraint ty) }
        in
        m.expr m
          {exp with pexp_desc= Pexp_function (ps, c, Pfunction_body exp1)}
    | Pexp_function (ps, c, b) when erase_jane_syntax ->
        let ps =
          List.map ps ~f:(fun param ->
              match param.pparam_desc with
              | Pparam_newtype (x, Some _) ->
                  {param with pparam_desc= Pparam_newtype (x, None)}
              | Pparam_val
                  ( Labelled l
                  , None
                  , { ppat_desc=
                        Ppat_constraint
                          ( pat
                          , Some
                              { ptyp_desc=
                                  Ptyp_extension ({txt= "call_pos"; loc}, _)
                              ; _ }
                          , _ )
                    ; _ } ) ->
                  let default_pos = dummy_position ~loc in
                  { param with
                    pparam_desc=
                      Pparam_val (Optional l, Some default_pos, pat) }
              | _ -> param )
        in
        Ast_mapper.default_mapper.expr m
          {exp with pexp_desc= Pexp_function (ps, c, b)}
    | Pexp_extension ({txt= "src_pos"; loc}, _) when erase_jane_syntax ->
        m.expr m (dummy_position ~loc)
    | Pexp_stack expr when erase_jane_syntax -> m.expr m expr
    | Pexp_unboxed_tuple es when erase_jane_syntax ->
        Ast_mapper.default_mapper.expr m {exp with pexp_desc= Pexp_tuple es}
    | Pexp_record_unboxed_product (es, e) when erase_jane_syntax ->
        Ast_mapper.default_mapper.expr m
          {exp with pexp_desc= Pexp_record (es, e)}
    | Pexp_unboxed_field (e, l) when erase_jane_syntax ->
        Ast_mapper.default_mapper.expr m
          {exp with pexp_desc= Pexp_field (e, l)}
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
    | Ppat_constraint (pat1, None, _ :: _) when erase_jane_syntax ->
        m.pat m pat1
    | Ppat_constraint (pat1, Some {ptyp_desc= Ptyp_poly ([], _t); _}, _) ->
        (* The parser put the same type constraint in two different nodes:
           [let _ : typ = exp] is represented as [let _ : typ = (exp :
           typ)]. *)
        m.pat m pat1
    | Ppat_unboxed_tuple (ps, oc) when erase_jane_syntax ->
        Ast_mapper.default_mapper.pat m
          {pat with ppat_desc= Ppat_tuple (ps, oc)}
    | Ppat_record_unboxed_product (ps, oc) when erase_jane_syntax ->
        Ast_mapper.default_mapper.pat m
          {pat with ppat_desc= Ppat_record (ps, oc)}
    | _ -> Ast_mapper.default_mapper.pat m pat
  in
  let typ (m : Ast_mapper.mapper) typ =
    let typ =
      { typ with
        ptyp_loc_stack= []
      ; ptyp_attributes=
          (* CR jane-syntax: This ensures that jane syntax attributes are
             removed *)
          ( typ.ptyp_attributes
          |> if erase_jane_syntax then map_attributes_no_sort m else Fn.id )
      }
    in
    let typ =
      match typ with
      (* Allow [???#] to [???] change when erasing jane syntax. *)
      | {ptyp_desc= Ptyp_constr (({txt= Lident s; _} as ident_loc), l); _}
        when String.is_suffix s ~suffix:"#" && erase_jane_syntax ->
          { typ with
            ptyp_desc=
              Ptyp_constr
                ( { ident_loc with
                    txt= Lident (String.chop_suffix_exn s ~suffix:"#") }
                , l ) }
      | { ptyp_desc=
            Ptyp_arrow
              ( Labelled l
              , {ptyp_desc= Ptyp_extension ({txt= "call_pos"; loc}, _); _}
              , return_type
              , _
              , _ )
        ; _ }
        when erase_jane_syntax ->
          let lexing_position_type =
            Ast_helper.Typ.constr
              {loc; txt= Ldot (Ldot (Lident "Stdlib", "Lexing"), "position")}
              []
          in
          let desc =
            Ptyp_arrow (Optional l, lexing_position_type, return_type, [], [])
          in
          {typ with ptyp_desc= desc}
      | {ptyp_desc= Ptyp_any _; _} when erase_jane_syntax ->
          {typ with ptyp_desc= Ptyp_any None}
      | {ptyp_desc= Ptyp_var (n, _); _} when erase_jane_syntax ->
          {typ with ptyp_desc= Ptyp_var (n, None)}
      | {ptyp_desc= Ptyp_alias (t, n, _); _} when erase_jane_syntax -> (
        match n with
        | Some _ -> {typ with ptyp_desc= Ptyp_alias (t, n, None)}
        | None ->
            {t with ptyp_attributes= typ.ptyp_attributes @ t.ptyp_attributes}
        )
      | {ptyp_desc= Ptyp_poly (l, t); _} when erase_jane_syntax ->
          let l = List.map l ~f:(fun (n, _) -> (n, None)) in
          {typ with ptyp_desc= Ptyp_poly (l, t)}
      | {ptyp_desc= Ptyp_unboxed_tuple ts; _} when erase_jane_syntax ->
          {typ with ptyp_desc= Ptyp_tuple ts}
      | _ -> typ
    in
    Ast_mapper.default_mapper.typ m typ
  in
  let structure m str =
    List.filter str ~f:(function
      | {pstr_desc= Pstr_kind_abbrev _; _} when erase_jane_syntax -> false
      | {pstr_desc= Pstr_attribute a; _} when ignore_doc_comments && is_doc a
        ->
          false
      | _ -> true )
    |> Ast_mapper.default_mapper.structure m
  in
  let signature m {psg_modalities; psg_items; psg_loc} =
    let psg_modalities = if erase_jane_syntax then [] else psg_modalities in
    let psg_items =
      List.filter psg_items ~f:(function
        | {psig_desc= Psig_kind_abbrev _; _} when erase_jane_syntax -> false
        | _ -> true )
      |> List.filter ~f:(function
           | {psig_desc= Psig_attribute a; _}
             when ignore_doc_comments && is_doc a ->
               false
           | _ -> true )
    in
    {psg_modalities; psg_items; psg_loc}
    |> Ast_mapper.default_mapper.signature m
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
  let type_declaration (m : Ast_mapper.mapper) decl =
    let ptype_jkind_annotation, extra_attributes =
      match decl.ptype_jkind_annotation with
      | Some {pjkind_desc= Abbreviation "immediate"; _} ->
          ( None
          , [ Ast_helper.Attr.mk
                {txt= "immediate"; loc= Location.none}
                (PStr []) ] )
      | Some {pjkind_desc= Abbreviation "immediate64"; _} ->
          ( None
          , [ Ast_helper.Attr.mk
                {txt= "immediate64"; loc= Location.none}
                (PStr []) ] )
      | ann -> ((if erase_jane_syntax then None else ann), [])
    in
    let ptype_attributes =
      extra_attributes @ decl.ptype_attributes
      |> normalize_immediate_attr
      (* CR jane-syntax: This ensures that jane syntax attributes are
         removed *)
      |> if erase_jane_syntax then map_attributes_no_sort m else Fn.id
    in
    let ptype_kind =
      match decl.ptype_kind with
      | Ptype_record_unboxed_product lds when erase_jane_syntax ->
          Ptype_record lds
      | _ -> decl.ptype_kind
    in
    Ast_mapper.default_mapper.type_declaration m
      {decl with ptype_attributes; ptype_jkind_annotation; ptype_kind}
  in
  let modes (m : Ast_mapper.mapper) ms =
    Ast_mapper.default_mapper.modes m
      ( if erase_jane_syntax then []
        else
          List.sort
            ~compare:(fun
                {Location.txt= Mode m1; _} {Location.txt= Mode m2; _} ->
              String.compare m1 m2 )
            ms )
  in
  let modalities (m : Ast_mapper.mapper) ms =
    Ast_mapper.default_mapper.modalities m
      ( if erase_jane_syntax then []
        else
          List.sort
            ~compare:(fun
                {Location.txt= Modality m1; _}
                {Location.txt= Modality m2; _}
              -> String.compare m1 m2 )
            ms )
  in
  let value_binding (m : Ast_mapper.mapper) vb =
    let vb =
      (* ocamlformat currently formats [let x = local_ ("" : string)] into
         [let local_ x = ("" : string)]. This normalizes against that *)
      match vb.pvb_expr.pexp_desc with
      | Pexp_constraint (exp, cty, modes) when not (List.is_empty modes) ->
          let pvb_expr =
            match cty with
            | None -> exp
            | _ ->
                {vb.pvb_expr with pexp_desc= Pexp_constraint (exp, cty, [])}
          in
          {vb with pvb_modes= vb.pvb_modes @ modes; pvb_expr}
      | _ -> vb
    in
    Ast_mapper.default_mapper.value_binding m vb
  in
  let constructor_declaration (m : Ast_mapper.mapper) cd =
    (* CR jane-syntax: This ensures that jane syntax attributes are
       removed *)
    ( if erase_jane_syntax then
        { cd with
          pcd_attributes= map_attributes_no_sort m cd.pcd_attributes
        ; pcd_vars= List.map cd.pcd_vars ~f:(fun (s, _) -> (s, None)) }
      else cd )
    |> Ast_mapper.default_mapper.constructor_declaration m
  in
  let extension_constructor_kind = function
    | Pext_decl (l, cargs, t) when erase_jane_syntax ->
        Pext_decl (List.map l ~f:(fun (n, _) -> (n, None)), cargs, t)
    | k -> k
  in
  let extension_constructor (m : Ast_mapper.mapper) ext =
    (* CR jane-syntax: This ensures that jane syntax attributes are
       removed *)
    ( if erase_jane_syntax then
        { ext with
          pext_attributes= map_attributes_no_sort m ext.pext_attributes
        ; pext_kind= extension_constructor_kind ext.pext_kind }
      else ext )
    |> Ast_mapper.default_mapper.extension_constructor m
  in
  let label_declaration (m : Ast_mapper.mapper) ld =
    (* CR modes: Develop a more general mechanism for "erasing" modalities
       into attributes *)
    let global__ =
      List.exists ld.pld_modalities ~f:(function
        | {Location.txt= Modality "global"; _} -> true
        | {txt= Modality _; _} -> false )
    in
    let ld = Ast_mapper.default_mapper.label_declaration m ld in
    { ld with
      pld_attributes=
        ( if erase_jane_syntax && global__ then
            globalized_attr :: ld.pld_attributes
          else ld.pld_attributes ) }
  in
  let constructor_argument (m : Ast_mapper.mapper) ca =
    (* CR modes: Develop a more general mechanism for "erasing" modalities
       into attributes *)
    let global__ =
      List.exists ca.pca_modalities ~f:(function
        | {Location.txt= Modality "global"; _} -> true
        | {txt= Modality _; _} -> false )
    in
    let ca = Ast_mapper.default_mapper.constructor_argument m ca in
    { ca with
      pca_type=
        ( if erase_jane_syntax && global__ then
            { ca.pca_type with
              ptyp_attributes= globalized_attr :: ca.pca_type.ptyp_attributes
            }
          else ca.pca_type ) }
  in
  { Ast_mapper.default_mapper with
    location
  ; attribute
  ; attributes
  ; structure
  ; signature
  ; class_signature
  ; class_structure
  ; constructor_argument
  ; expr
  ; pat
  ; typ
  ; type_declaration
  ; label_declaration
  ; modes
  ; modalities
  ; value_binding
  ; constructor_declaration
  ; extension_constructor }

let ast fragment ~ignore_doc_comments ~erase_jane_syntax c =
  map fragment (make_mapper c ~ignore_doc_comments ~erase_jane_syntax)

let equal fragment ~ignore_doc_comments ~erase_jane_syntax c ~old:ast1
    ~new_:ast2 =
  let map = ast fragment c ~ignore_doc_comments in
  equal fragment
    (map ~erase_jane_syntax ast1)
    (map ~erase_jane_syntax:false ast2)

let ast = ast ~ignore_doc_comments:false

let make_docstring_mapper docstrings =
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
    Ast_mapper.default_mapper.attributes m atrs |> sort_attributes
  in
  {Ast_mapper.default_mapper with attribute; attributes}

let docstrings (type a) (fragment : a t) s =
  let docstrings = ref [] in
  let (_ : a) = map fragment (make_docstring_mapper docstrings) s in
  !docstrings

let docstring conf ~erase_jane_syntax =
  let mapper =
    make_mapper conf ~ignore_doc_comments:false ~erase_jane_syntax
  in
  let normalize_code = normalize_code conf mapper in
  docstring conf ~normalize_code

let moved_docstrings fragment ~erase_jane_syntax c ~old:s1 ~new_:s2 =
  let d1 = docstrings fragment s1 in
  let d2 = docstrings fragment s2 in
  let equal ~old:(lx, x) ~new_:(ly, y) =
    String.equal
      (docstring c x ~erase_jane_syntax ~location:lx ~pro:"(**")
      (docstring c y ~erase_jane_syntax:false ~location:ly ~pro:"(**")
  in
  let cmt_kind = `Doc_comment in
  let cmt (loc, x) = Cmt.create_docstring x loc in
  let dropped x = {Cmt.kind= `Dropped (cmt x); cmt_kind} in
  let added x = {Cmt.kind= `Added (cmt x); cmt_kind} in
  let modified (x, y) = {Cmt.kind= `Modified (cmt x, cmt y); cmt_kind} in
  match List.zip d1 d2 with
  | Unequal_lengths ->
      (* We only return the ones that are not in both lists. *)
      let l1 =
        List.filter d1 ~f:(fun old ->
            List.for_all d2 ~f:(fun new_ -> not (equal ~old ~new_)) )
      in
      let l1 = List.map ~f:dropped l1 in
      let l2 =
        List.filter d2 ~f:(fun new_ ->
            List.for_all d1 ~f:(fun old -> not (equal ~old ~new_)) )
      in
      let l2 = List.map ~f:added l2 in
      List.rev_append l1 l2
  | Ok l ->
      let l = List.filter l ~f:(fun (old, new_) -> not (equal ~old ~new_)) in
      List.map ~f:modified l
