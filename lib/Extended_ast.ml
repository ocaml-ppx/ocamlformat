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

open Ocamlformat_parser_extended
include Parsetree

type use_file = toplevel_phrase list

type repl_file = repl_phrase list

module Std_parsetree = Ocamlformat_parser_standard.Parsetree

type ('a, 'b) paired = {extended: 'a; std: 'b}

type 'a t =
  | Structure : (structure, Std_parsetree.structure) paired t
  | Signature : (signature, Std_parsetree.signature) paired t
  | Use_file : (use_file, Std_parsetree.toplevel_phrase list) paired t
  | Core_type : (core_type, Std_parsetree.core_type) paired t
  | Module_type : (module_type, Std_parsetree.module_type) paired t
  | Expression : (expression, Std_parsetree.expression) paired t
  | Pattern : (pattern, Std_parsetree.pattern) paired t
  | Repl_file : repl_file t
  | Documentation : Ocamlformat_odoc_parser.Ast.t t

type any_t = Any : 'a t -> any_t [@@unboxed]

let of_syntax = function
  | Syntax.Structure -> Any Structure
  | Signature -> Any Signature
  | Use_file -> Any Use_file
  | Core_type -> Any Core_type
  | Module_type -> Any Module_type
  | Expression -> Any Expression
  | Pattern -> Any Pattern
  | Repl_file -> Any Repl_file
  | Documentation -> Any Documentation

let map (type a) (x : a t) (m : Ast_mapper.mapper) : a -> a =
  match x with
  | Structure -> fun v -> {v with extended= m.structure m v.extended}
  | Signature -> fun v -> {v with extended= m.signature m v.extended}
  | Use_file ->
      fun v -> {v with extended= List.map ~f:(m.toplevel_phrase m) v.extended}
  | Core_type -> fun v -> {v with extended= m.typ m v.extended}
  | Module_type -> fun v -> {v with extended= m.module_type m v.extended}
  | Expression -> fun v -> {v with extended= m.expr m v.extended}
  | Pattern -> fun v -> {v with extended= m.pat m v.extended}
  | Repl_file -> List.map ~f:(m.repl_phrase m)
  | Documentation -> Fn.id

module Parse = struct
  let normalize_mapper ~ocaml_version ~preserve_beginend ~prefer_let_puns =
    let open Asttypes in
    let open Ast_mapper in
    let enable_short_field_annot =
      Ocaml_version.compare ocaml_version Ocaml_version.Releases.v4_03_0 >= 0
    in
    let record_field m (f, t, v) =
      match (t, v) with
      (* [{ x = x }] -> [{ x }] *)
      | ( _
        , Some {pexp_desc= Pexp_ident {txt= v_txt; _}; pexp_attributes= []; _}
        )
        when Std_longident.field_alias ~field:f.txt v_txt ->
          (f, t, None)
      (* [{ x = (x : t) }] -> [{ x : t }] *)
      | ( None
        , Some
            { pexp_desc=
                Pexp_constraint
                  ( { pexp_desc= Pexp_ident {txt= v_txt; _}
                    ; pexp_attributes= []
                    ; _ }
                  , t1 )
            ; pexp_attributes= []
            ; _ } )
        when enable_short_field_annot
             && Std_longident.field_alias ~field:f.txt v_txt ->
          (f, Some (Pconstraint t1), None)
      (* [{ x :> t = (x : t) }] -> [{ x : t :> t }] *)
      | ( Some (Pcoerce (None, t2))
        , Some
            { pexp_desc=
                Pexp_constraint
                  ( { pexp_desc= Pexp_ident {txt= v_txt; _}
                    ; pexp_attributes= []
                    ; _ }
                  , t1 )
            ; pexp_attributes= []
            ; _ } )
        when enable_short_field_annot
             && Std_longident.field_alias ~field:f.txt v_txt ->
          (f, Some (Pcoerce (Some t1, t2)), None)
      (* [{ x = (x :> t) }] -> [{ x :> t }] *)
      (* [{ x = (x : t :> t) }] -> [{ x : t :> t }] *)
      | ( None
        , Some
            { pexp_desc=
                Pexp_coerce
                  ( { pexp_desc= Pexp_ident {txt= v_txt; _}
                    ; pexp_attributes= []
                    ; _ }
                  , t1
                  , t2 )
            ; pexp_attributes= []
            ; _ } )
        when enable_short_field_annot
             && Std_longident.field_alias ~field:f.txt v_txt ->
          (f, Some (Pcoerce (t1, t2)), None)
      (* [{ x : t = (x :> t) }] -> [{ x : t :> t }] *)
      | ( Some (Pconstraint t1)
        , Some
            { pexp_desc=
                Pexp_coerce
                  ( { pexp_desc= Pexp_ident {txt= v_txt; _}
                    ; pexp_attributes= []
                    ; _ }
                  , None
                  , t2 )
            ; pexp_attributes= []
            ; _ } )
        when enable_short_field_annot
             && Std_longident.field_alias ~field:f.txt v_txt ->
          (f, Some (Pcoerce (Some t1, t2)), None)
      | _ -> (f, t, Option.map ~f:(m.expr m) v)
    in
    let pat_record_field m (f, t, v) =
      match (t, v) with
      (* [{ x = x }] -> [{ x }] *)
      | _, Some {ppat_desc= Ppat_var {txt= v_txt; _}; ppat_attributes= []; _}
        when Std_longident.field_alias ~field:f.txt (Lident v_txt) ->
          (f, t, None)
      (* [{ x = (x : t) }] -> [{ x : t}] *)
      | ( None
        , Some
            { ppat_desc=
                Ppat_constraint
                  ( { ppat_desc= Ppat_var {txt= v_txt; _}
                    ; ppat_attributes= []
                    ; _ }
                  , t )
            ; ppat_attributes= []
            ; _ } )
        when enable_short_field_annot
             && Std_longident.field_alias ~field:f.txt (Lident v_txt) ->
          (f, Some t, None)
      | _ -> (f, t, Option.map ~f:(m.pat m) v)
    in
    let map_labeled_tuple_element m f = function
      | Lte_simple lte -> f m lte
      | (Lte_constrained_pun _ | Lte_pun _) as x -> x
    in
    let pat_tuple_elt m te =
      match (te.lte_label, te.lte_elt) with
      (* [ ~x:x ] -> [ ~x ] *)
      | ( Some lbl
        , {ppat_desc= Ppat_var {txt= v_txt; _}; ppat_attributes= []; _} )
        when String.equal lbl.txt v_txt ->
          Lte_pun lbl
      (* [~x:(x : t)] -> [ ~(x : t)] *)
      | ( Some lbl
        , { ppat_desc=
              Ppat_constraint
                ( { ppat_desc= Ppat_var {txt= v_txt; _}
                  ; ppat_attributes= []
                  ; _ }
                , t )
          ; ppat_attributes= []
          ; ppat_loc
          ; _ } )
        when String.equal lbl.txt v_txt ->
          Lte_constrained_pun
            { loc= {lbl.loc with loc_end= ppat_loc.loc_end}
            ; label= lbl
            ; type_constraint= t }
      | lte_label, pat -> Lte_simple {lte_label; lte_elt= m.pat m pat}
    in
    let pat_tuple_elt m lte =
      map_labeled_tuple_element m pat_tuple_elt lte
    in
    let exp_tuple_elt m te =
      match (te.lte_label, te.lte_elt) with
      (* [ ~x:x ] -> [ ~x ] *)
      | ( Some lbl
        , { pexp_desc= Pexp_ident {txt= Lident v_txt; _}
          ; pexp_attributes= []
          ; _ } )
        when String.equal lbl.txt v_txt ->
          Lte_pun lbl
      (* [~x:(x : t)] -> [ ~(x : t)] *)
      | ( Some lbl
        , { pexp_desc=
              Pexp_constraint
                ( { pexp_desc= Pexp_ident {txt= Lident v_txt; _}
                  ; pexp_attributes= []
                  ; _ }
                , t )
          ; pexp_attributes= []
          ; pexp_loc
          ; _ } )
        when String.equal lbl.txt v_txt ->
          Lte_constrained_pun
            { loc= {lbl.loc with loc_end= pexp_loc.loc_end}
            ; label= lbl
            ; type_constraint= Pconstraint t }
      (* [~x:(x : t1 :> t2)] -> [ ~(x : t1 :> t2)] *)
      | ( Some lbl
        , { pexp_desc=
              Pexp_coerce
                ({pexp_desc= Pexp_ident {txt= Lident v_txt; _}; _}, bty, tty)
          ; pexp_attributes= []
          ; pexp_loc
          ; _ } )
        when String.equal lbl.txt v_txt ->
          Lte_constrained_pun
            { loc= {lbl.loc with loc_end= pexp_loc.loc_end}
            ; label= lbl
            ; type_constraint= Pcoerce (bty, tty) }
      | lte_label, exp -> Lte_simple {lte_label; lte_elt= m.expr m exp}
    in
    let exp_tuple_elt m lte =
      map_labeled_tuple_element m exp_tuple_elt lte
    in
    let binding_op (m : Ast_mapper.mapper) b =
      let b' =
        let loc_start = b.pbop_op.loc.loc_start in
        let loc_end = b.pbop_exp.pexp_loc.loc_end in
        let pbop_is_pun =
          match prefer_let_puns with
          | None -> b.pbop_is_pun
          | Some false -> false
          | Some true -> (
              b.pbop_is_pun
              ||
              match (b.pbop_pat.ppat_desc, b.pbop_exp.pexp_desc) with
              | Ppat_var {txt; _}, Pexp_ident {txt= Lident e; _} ->
                  String.equal txt e
              | _ -> false )
        in
        {b with pbop_loc= {b.pbop_loc with loc_start; loc_end}; pbop_is_pun}
      in
      Ast_mapper.default_mapper.binding_op m b'
    in
    let value_bindings (m : Ast_mapper.mapper) vbs =
      let punning is_extension vb =
        let is_extension =
          (* [and] nodes don't have extensions, so we need to track if the
             earlier [let] did *)
          is_extension || Option.is_some vb.pvb_attributes.attrs_extension
        in
        let pvb_is_pun =
          is_extension
          &&
          match prefer_let_puns with
          | None -> vb.pvb_is_pun
          | Some false -> false
          | Some true -> (
              vb.pvb_is_pun
              ||
              match (vb.pvb_pat.ppat_desc, vb.pvb_body) with
              | ( Ppat_var {txt; _}
                , Pfunction_body {pexp_desc= Pexp_ident {txt= Lident e; _}; _}
                ) ->
                  String.equal txt e
              | _ -> false )
        in
        (is_extension, {vb with pvb_is_pun})
      in
      let vbs' =
        { vbs with
          pvbs_bindings=
            snd @@ List.fold_map ~init:false ~f:punning vbs.pvbs_bindings }
      in
      Ast_mapper.default_mapper.value_bindings m vbs'
    in
    let pat m = function
      | {ppat_desc= Ppat_cons (_ :: _ :: _ :: _ as l); _} as p
        when match List.last_exn l with
             (* Empty lists are always represented as Lident [] *)
             | { ppat_desc= Ppat_construct ({txt= Lident "[]"; loc= _}, None)
               ; ppat_attributes= []
               ; _ } ->
                 true
             | _ -> false ->
          let pats = List.(rev (tl_exn (rev l))) in
          {p with ppat_desc= Ppat_list pats}
      (* Field alias shorthand *)
      | {ppat_desc= Ppat_record (fields, flag); _} as e ->
          let fields = List.map ~f:(pat_record_field m) fields in
          {e with ppat_desc= Ppat_record (fields, flag)}
      | {ppat_desc= Ppat_tuple (l, oc); _} as p ->
          let l = List.map ~f:(pat_tuple_elt m) l in
          {p with ppat_desc= Ppat_tuple (l, oc)}
      | p -> Ast_mapper.default_mapper.pat m p
    in
    let expr (m : Ast_mapper.mapper) = function
      | {pexp_desc= Pexp_cons (_ :: _ :: _ :: _ as l); _} as e
        when match List.last_exn l with
             (* Empty lists are always represented as Lident [] *)
             | { pexp_desc= Pexp_construct ({txt= Lident "[]"; loc= _}, None)
               ; pexp_attributes= []
               ; _ } ->
                 true
             | _ -> false ->
          let exprs = List.(rev (tl_exn (rev l))) in
          {e with pexp_desc= Pexp_list exprs}
      (* Removing beginend *)
      | { pexp_desc= Pexp_beginend (e', {infix_ext= None; infix_attrs= []})
        ; pexp_attributes= []
        ; _ }
        when not preserve_beginend ->
          m.expr m e'
      (* Field alias shorthand *)
      | {pexp_desc= Pexp_record (fields, with_); _} as e ->
          let fields = List.map ~f:(record_field m) fields in
          { e with
            pexp_desc= Pexp_record (fields, Option.map ~f:(m.expr m) with_)
          }
      (* [( + ) 1 2] -> [1 + 2] *)
      | { pexp_desc=
            Pexp_apply
              ( { pexp_desc=
                    Pexp_ident {txt= Lident op as longident; loc= loc_op}
                ; pexp_attributes= []
                ; _ }
              , [(Nolabel, l); (Nolabel, r)] )
        ; _ } as e
        when Std_longident.is_infix longident
             && not (Std_longident.is_monadic_binding longident) ->
          let label_loc = {txt= op; loc= loc_op} in
          {e with pexp_desc= Pexp_infix (label_loc, m.expr m l, m.expr m r)}
      | {pexp_desc= Pexp_tuple l; _} as p ->
          let l = List.map ~f:(exp_tuple_elt m) l in
          {p with pexp_desc= Pexp_tuple l}
      | e -> Ast_mapper.default_mapper.expr m e
    in
    Ast_mapper.{default_mapper with expr; pat; binding_op; value_bindings}

  let ast (type a) (fg : a t) ~ocaml_version ~preserve_beginend
      ~prefer_let_puns ~input_name str : a =
    let nm =
      normalize_mapper ~ocaml_version ~preserve_beginend ~prefer_let_puns
    in
    let lexbuf = Lexing.from_string str in
    let ocaml_version_pair =
      Some Ocaml_version.(major ocaml_version, minor ocaml_version)
    in
    Location.init_info lexbuf input_name ;
    let parse_std std_fg =
      (* Suppress warnings during raw std parse to avoid duplicate w50
         warnings — w50 handling is done separately by parse_ocaml *)
      Warning.with_warning_filter
        ~filter_warning:(fun _loc _warn -> false)
        ~filter_alert:(fun _loc _alert -> false)
        ~f:(fun () -> Std_ast.Parse.ast std_fg ~ocaml_version ~input_name str)
    in
    let paired normalize parse_ext std_fg =
      let extended =
        normalize nm (parse_ext ~ocaml_version:ocaml_version_pair lexbuf)
      in
      {extended; std= parse_std std_fg}
    in
    match fg with
    | Structure ->
        paired
          (fun nm -> nm.structure nm)
          Parse.implementation Std_ast.Structure
    | Signature ->
        paired (fun nm -> nm.signature nm) Parse.interface Std_ast.Signature
    | Use_file ->
        paired
          (fun nm -> List.map ~f:(nm.toplevel_phrase nm))
          Parse.use_file Std_ast.Use_file
    | Core_type ->
        paired (fun nm -> nm.typ nm) Parse.core_type Std_ast.Core_type
    | Module_type ->
        paired
          (fun nm -> nm.module_type nm)
          Parse.module_type Std_ast.Module_type
    | Expression ->
        paired (fun nm -> nm.expr nm) Parse.expression Std_ast.Expression
    | Pattern -> paired (fun nm -> nm.pat nm) Parse.pattern Std_ast.Pattern
    | Repl_file ->
        List.map ~f:(nm.repl_phrase nm)
          (Toplevel_lexer.repl_file ~ocaml_version:ocaml_version_pair lexbuf)
    | Documentation ->
        let pos = (Location.curr lexbuf).loc_start in
        let pos = {pos with pos_fname= input_name} in
        Docstring.parse_file pos str
end

module Printast = struct
  include Printast

  let use_file = Format.pp_print_list top_phrase

  let repl_file = Format.pp_print_list repl_phrase

  let ast (type a) : a t -> _ -> a -> _ = function
    | Structure -> fun fmt v -> implementation fmt v.extended
    | Signature -> fun fmt v -> interface fmt v.extended
    | Use_file -> fun fmt v -> use_file fmt v.extended
    | Core_type -> fun fmt v -> core_type fmt v.extended
    | Module_type -> fun fmt v -> module_type fmt v.extended
    | Expression -> fun fmt v -> expression fmt v.extended
    | Pattern -> fun fmt v -> pattern fmt v.extended
    | Repl_file -> repl_file
    | Documentation -> Docstring.dump
end

module Asttypes = struct
  include Asttypes

  let is_override = function Override -> true | Fresh -> false

  let is_recursive = function Recursive -> true | Nonrecursive -> false
end
