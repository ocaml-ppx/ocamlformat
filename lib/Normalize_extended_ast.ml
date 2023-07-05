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

open Extended_ast

let start_column loc =
  let pos = loc.Location.loc_start in
  pos.pos_cnum - pos.pos_bol

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
                            Pexp_constant
                              {pconst_desc= Pconst_string (doc, _, None); _}
                        ; pexp_loc
                        ; _ }
                      , [] )
                ; _ } ]
        ; _ }
        when Ast.Attr.is_doc atr ->
          docs := Set.add !docs (Cmt.create_docstring doc pexp_loc) ;
          atr
      | _ -> Ast_mapper.default_mapper.attribute m atr
    in
    map fragment {Ast_mapper.default_mapper with attribute} ast |> ignore ;
    !docs
  in
  Set.(to_list (diff (of_list (module Cmt) comments) (of_ast ast)))

let normalize_comments dedup fmt comments =
  let comments = dedup comments in
  List.sort comments ~compare:(fun a b ->
      Migrate_ast.Location.compare (Cmt.loc a) (Cmt.loc b) )
  |> List.iter ~f:(fun cmt -> Format.fprintf fmt "%s," (Cmt.txt cmt))

let normalize_parse_result ast_kind ast comments =
  Format.asprintf "AST,%a,COMMENTS,[%a]" (Printast.ast ast_kind) ast
    (normalize_comments (dedup_cmts ast_kind ast))
    comments

let normalize_code conf (m : Ast_mapper.mapper) ~offset txt =
  let txt =
    String.split_lines txt
    |> Cmt.unindent_lines ~offset
    |> String.concat ~sep:"\n"
  in
  let input_name = "<output>" in
  match Parse_with_comments.parse_toplevel conf ~input_name ~source:txt with
  | First {ast; comments; _} ->
      normalize_parse_result Use_file
        (List.map ~f:(m.toplevel_phrase m) ast)
        comments
  | Second {ast; comments; _} ->
      normalize_parse_result Repl_file
        (List.map ~f:(m.repl_phrase m) ast)
        comments
  | exception _ -> txt

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
      when Ast.Attr.is_doc attr ->
        let normalize_code =
          (* Indentation is already stripped by odoc-parser. *)
          normalize_code conf m ~offset:0
        in
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
        List.filter atrs ~f:(fun a -> not (Ast.Attr.is_doc a))
      else atrs
    in
    Ast_mapper.default_mapper.attributes m atrs |> sort_attributes
  in
  let repl_phrase (m : Ast_mapper.mapper) {prepl_phrase; prepl_output} =
    let p =
      {prepl_phrase; prepl_output= Docstring.normalize_text prepl_output}
    in
    Ast_mapper.default_mapper.repl_phrase m p
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
  let typ (m : Ast_mapper.mapper) typ =
    let typ = {typ with ptyp_loc_stack= []} in
    Ast_mapper.default_mapper.typ m typ
  in
  { Ast_mapper.default_mapper with
    location
  ; attribute
  ; attributes
  ; repl_phrase
  ; expr
  ; typ }

let ast fragment ~ignore_doc_comments c =
  map fragment (make_mapper c ~ignore_doc_comments)

let diff ~f ~cmt_kind x y =
  let dropped x = {Cmt.kind= `Dropped x; cmt_kind} in
  let added x = {Cmt.kind= `Added x; cmt_kind} in
  (*= [symmetric_diff x y] returns a sequence of changes between [x] and [y]:
      - [First k] means [k] is in [x] but not [y]
      - [Second k] means [k] is in [y] but not [x] *)
  Set.symmetric_diff (f x) (f y)
  |> Sequence.to_list
  (*= - [First _] is reported as a comment dropped
      - [Second _] is reported as a comment added *)
  |> List.map ~f:(Either.value_map ~first:dropped ~second:added)
  |> function [] -> Ok () | errors -> Error errors

let diff_docstrings c x y =
  let mapper = make_mapper c ~ignore_doc_comments:false in
  let docstring cmt =
    let offset = start_column (Cmt.loc cmt) + 3 in
    let normalize_code = normalize_code c mapper ~offset in
    docstring c ~normalize_code (Cmt.txt cmt)
  in
  let norm z =
    let f cmt = Cmt.create_docstring (docstring cmt) (Cmt.loc cmt) in
    Set.of_list (module Cmt.Comparator_no_loc) (List.map ~f z)
  in
  diff ~f:norm ~cmt_kind:`Doc_comment x y

let diff_cmts (conf : Conf.t) x y =
  let mapper = make_mapper conf ~ignore_doc_comments:false in
  let normalize_code = normalize_code conf mapper in
  let norm z =
    let norm_non_code cmt =
      Cmt.create_comment
        (Docstring.normalize_text (Cmt.txt cmt))
        (Cmt.loc cmt)
    in
    let f z =
      match Cmt.txt z with
      | "" | "$" -> norm_non_code z
      | str ->
          if Char.equal str.[0] '$' then
            let chars_removed =
              if Char.equal str.[String.length str - 1] '$' then 2 else 1
            in
            let len = String.length str - chars_removed in
            let source = String.sub ~pos:1 ~len str in
            let loc = Cmt.loc z in
            let offset = start_column loc + 3 in
            Cmt.create_comment (normalize_code ~offset source) loc
          else norm_non_code z
    in
    Set.of_list (module Cmt.Comparator_no_loc) (List.map ~f z)
  in
  diff ~f:norm ~cmt_kind:`Comment x y

let equal fragment ~ignore_doc_comments c ast1 ast2 =
  let map = ast fragment c ~ignore_doc_comments in
  equal fragment (map ast1) (map ast2)
