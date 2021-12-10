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

open Ocaml_413
include Parsetree

type use_file = toplevel_phrase list

type 'a t =
  | Structure : structure t
  | Signature : signature t
  | Use_file : use_file t
  | Core_type : core_type t
  | Module_type : module_type t
  | Expression : expression t

let equal (type a) (_ : a t) : a -> a -> bool = Poly.equal

let map (type a) (x : a t) (m : Ast_mapper.mapper) : a -> a =
  match x with
  | Structure -> m.structure m
  | Signature -> m.signature m
  | Use_file -> List.map ~f:(m.toplevel_phrase m)
  | Core_type -> m.typ m
  | Module_type -> m.module_type m
  | Expression -> m.expr m

module Parse = struct
  let ast (type a) (fg : a t) lexbuf : a =
    match fg with
    | Structure -> Parse.implementation lexbuf
    | Signature -> Parse.interface lexbuf
    | Use_file -> Parse.use_file lexbuf
    | Core_type -> Parse.core_type lexbuf
    | Module_type -> Parse.module_type lexbuf
    | Expression -> Parse.expression lexbuf
end

module Pprintast = struct
  include Pprintast

  let use_file = Format.pp_print_list top_phrase

  let ast (type a) : a t -> _ -> a -> _ = function
    | Structure -> structure
    | Signature -> signature
    | Use_file -> use_file
    | Core_type -> core_type
    | Module_type -> module_type
    | Expression -> expression
end

module Normalize = struct
  let is_doc = function
    | {attr_name= {Location.txt= "ocaml.doc" | "ocaml.text"; _}; _} -> true
    | _ -> false

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
            docs := Set.add !docs (Cmt.create ("*" ^ doc) pexp_loc) ;
            atr
        | _ -> Ast_mapper.default_mapper.attribute m atr
      in
      map fragment {Ast_mapper.default_mapper with attribute} ast |> ignore ;
      !docs
    in
    Set.(to_list (diff (of_list (module Cmt) comments) (of_ast ast)))

  let normalize_code conf (m : Ast_mapper.mapper) txt =
    match Parse_with_comments.parse Parse.ast Structure conf ~source:txt with
    | {ast; comments; _} ->
        let comments = dedup_cmts Structure ast comments in
        let print_comments fmt (l : Cmt.t list) =
          List.sort l ~compare:(fun {Cmt.loc= a; _} {Cmt.loc= b; _} ->
              Migrate_ast.Location.compare a b )
          |> List.iter ~f:(fun {Cmt.txt; _} -> Format.fprintf fmt "%s," txt)
        in
        let ast = m.structure m ast in
        Format.asprintf "AST,%a,COMMENTS,[%a]" Pprintast.structure ast
          print_comments comments
    | exception _ -> txt

  let docstring (c : Conf.t) =
    Docstring.normalize ~parse_docstrings:c.parse_docstrings

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
                            Pexp_constant (Pconst_string (doc, str_loc, None))
                        ; _ } as exp )
                    , [] )
              ; _ } as pstr ) ]
        when is_doc attr ->
          let normalize_code = normalize_code conf m in
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
                          Pexp_constant (Pconst_string (doc, _, None))
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

  let docstrings (type a) (fragment : a t) s =
    let docstrings = ref [] in
    let (_ : a) = map fragment (make_docstring_mapper docstrings) s in
    !docstrings

  let docstring conf =
    let mapper = make_mapper conf ~ignore_doc_comments:false in
    let normalize_code = normalize_code conf mapper in
    docstring conf ~normalize_code

  let moved_docstrings fragment c s1 s2 =
    let d1 = docstrings fragment s1 in
    let d2 = docstrings fragment s2 in
    let equal (_, x) (_, y) = String.equal (docstring c x) (docstring c y) in
    match List.zip d1 d2 with
    | Unequal_lengths ->
        (* We only return the ones that are not in both lists. *)
        let l1 = List.filter d1 ~f:(fun x -> not (List.mem ~equal d2 x)) in
        let l1 =
          List.map ~f:(fun (loc, x) -> Docstring.Removed (loc, x)) l1
        in
        let l2 = List.filter d2 ~f:(fun x -> not (List.mem ~equal d1 x)) in
        let l2 = List.map ~f:(fun (loc, x) -> Docstring.Added (loc, x)) l2 in
        List.rev_append l1 l2
    | Ok l ->
        let l = List.filter l ~f:(fun (x, y) -> not (equal x y)) in
        List.map
          ~f:(fun ((loc, x), (_, y)) -> Docstring.Unstable (loc, x, y))
          l

  let diff_docstrings c x y =
    let norm z =
      let f Cmt.{txt; _} = docstring c txt in
      Set.of_list (module String) (List.map ~f z)
    in
    Set.symmetric_diff (norm x) (norm y)

  let diff_cmts (conf : Conf.t) x y =
    let norm z =
      let norm_non_code {Cmt.txt; _} = Docstring.normalize_text txt in
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
              match
                Parse_with_comments.parse Parse.ast Structure conf ~source
              with
              | exception _ -> norm_non_code z
              | {ast= s; _} ->
                  Format.asprintf "%a" Pprintast.structure
                    (ast Structure conf s)
            else norm_non_code z
      in
      Set.of_list (module String) (List.map ~f z)
    in
    Set.symmetric_diff (norm x) (norm y)
end
