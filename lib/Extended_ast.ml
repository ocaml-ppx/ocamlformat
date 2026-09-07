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

(** Internal kind tag GADT, used for parse-time dispatch and to recover the
    constructor of a parsed [t] without unpacking its data. *)
module Kind = struct
  type 'a t =
    | Structure : structure t
    | Signature : signature t
    | Use_file : use_file t
    | Core_type : core_type t
    | Module_type : module_type t
    | Expression : expression t
    | Pattern : pattern t
    | Repl_file : repl_file t
    | Documentation : Ocamlformat_odoc_parser.Ast.t t

  type any = Any : 'a t -> any [@@unboxed]

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
end

type 'a t =
  | Structure :
      {ast: structure; std: Std_parsetree.structure; cmts: Cmts.t}
      -> structure t
  | Signature :
      {ast: signature; std: Std_parsetree.signature; cmts: Cmts.t}
      -> signature t
  | Use_file :
      { ast: use_file
      ; std: Std_parsetree.toplevel_phrase list
      ; prefix: string
      ; cmts: Cmts.t }
      -> use_file t
  | Core_type :
      {ast: core_type; std: Std_parsetree.core_type; cmts: Cmts.t}
      -> core_type t
  | Module_type :
      {ast: module_type; std: Std_parsetree.module_type; cmts: Cmts.t}
      -> module_type t
  | Expression :
      {ast: expression; std: Std_parsetree.expression; cmts: Cmts.t}
      -> expression t
  | Pattern :
      {ast: pattern; std: Std_parsetree.pattern; cmts: Cmts.t}
      -> pattern t
  | Repl_file : {ast: repl_file; cmts: Cmts.t} -> repl_file t
  | Documentation :
      Ocamlformat_odoc_parser.Ast.t
      -> Ocamlformat_odoc_parser.Ast.t t

type any_t = Any : 'a t -> any_t [@@unboxed]

let ast (type a) (t : a t) : a =
  match t with
  | Structure {ast; _} -> ast
  | Signature {ast; _} -> ast
  | Use_file {ast; _} -> ast
  | Core_type {ast; _} -> ast
  | Module_type {ast; _} -> ast
  | Expression {ast; _} -> ast
  | Pattern {ast; _} -> ast
  | Repl_file {ast; _} -> ast
  | Documentation ast -> ast

let traverse (type a) (t : a t) : Ast_mapper.mapper -> a -> a =
  match t with
  | Structure _ -> fun m -> m.structure m
  | Signature _ -> fun m -> m.signature m
  | Use_file _ -> fun m -> List.map ~f:(m.toplevel_phrase m)
  | Core_type _ -> fun m -> m.typ m
  | Module_type _ -> fun m -> m.module_type m
  | Expression _ -> fun m -> m.expr m
  | Pattern _ -> fun m -> m.pat m
  | Repl_file _ -> fun m -> List.map ~f:(m.repl_phrase m)
  | Documentation _ -> fun _ x -> x

let cmts (type a) (t : a t) : Cmts.t option =
  match t with
  | Structure {cmts; _}
   |Signature {cmts; _}
   |Use_file {cmts; _}
   |Core_type {cmts; _}
   |Module_type {cmts; _}
   |Expression {cmts; _}
   |Pattern {cmts; _}
   |Repl_file {cmts; _} ->
      Some cmts
  | Documentation _ -> None

let copy_cmts (type a) (t : a t) : a t =
  match t with
  | Structure r -> Structure {r with cmts= Cmts.copy r.cmts}
  | Signature r -> Signature {r with cmts= Cmts.copy r.cmts}
  | Use_file r -> Use_file {r with cmts= Cmts.copy r.cmts}
  | Core_type r -> Core_type {r with cmts= Cmts.copy r.cmts}
  | Module_type r -> Module_type {r with cmts= Cmts.copy r.cmts}
  | Expression r -> Expression {r with cmts= Cmts.copy r.cmts}
  | Pattern r -> Pattern {r with cmts= Cmts.copy r.cmts}
  | Repl_file r -> Repl_file {r with cmts= Cmts.copy r.cmts}
  | Documentation _ as t -> t

let kind_of (type a) (t : a t) : a Kind.t =
  match t with
  | Structure _ -> Structure
  | Signature _ -> Signature
  | Use_file _ -> Use_file
  | Core_type _ -> Core_type
  | Module_type _ -> Module_type
  | Expression _ -> Expression
  | Pattern _ -> Pattern
  | Repl_file _ -> Repl_file
  | Documentation _ -> Documentation

(** Build a lexbuf for [source]. For [Use_file], also consumes the [#!]
    shebang line if present (so subsequent parsing/tokenizing has line
    numbers relative to the original source) and returns it as [prefix]. *)
let prepare_lexbuf (type a) (fg : a Kind.t) ~input_name source =
  let lexbuf = Lexing.from_string source in
  Location.init_info lexbuf input_name ;
  let prefix =
    match fg with
    | Kind.Use_file ->
        Lexer.skip_hash_bang lexbuf ;
        String.sub source ~pos:0 ~len:lexbuf.lex_last_pos
    | _ -> ""
  in
  (lexbuf, prefix)

let map (type a) (m : Ast_mapper.mapper) (t : a t) : a t =
  match t with
  | Structure r -> Structure {r with ast= m.structure m r.ast}
  | Signature r -> Signature {r with ast= m.signature m r.ast}
  | Use_file r ->
      Use_file {r with ast= List.map ~f:(m.toplevel_phrase m) r.ast}
  | Core_type r -> Core_type {r with ast= m.typ m r.ast}
  | Module_type r -> Module_type {r with ast= m.module_type m r.ast}
  | Expression r -> Expression {r with ast= m.expr m r.ast}
  | Pattern r -> Pattern {r with ast= m.pat m r.ast}
  | Repl_file r -> Repl_file {r with ast= List.map ~f:(m.repl_phrase m) r.ast}
  | Documentation _ as t -> t

let normalize_mapper ~ocaml_version ~preserve_beginend ~prefer_let_puns =
  let open Asttypes in
  let open Ast_mapper in
  let enable_short_field_annot =
    Ocaml_version.compare ocaml_version Ocaml_version.Releases.v4_03_0 >= 0
  in
  let record_field m (f, t, v) =
    match (t, v) with
    (* [{ x = x }] -> [{ x }] *)
    | _, Some {pexp_desc= Pexp_ident {txt= v_txt; _}; pexp_attributes= []; _}
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
    | Some lbl, {ppat_desc= Ppat_var {txt= v_txt; _}; ppat_attributes= []; _}
      when String.equal lbl.txt v_txt ->
        Lte_pun lbl
    (* [~x:(x : t)] -> [ ~(x : t)] *)
    | ( Some lbl
      , { ppat_desc=
            Ppat_constraint
              ( {ppat_desc= Ppat_var {txt= v_txt; _}; ppat_attributes= []; _}
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
  let pat_tuple_elt m lte = map_labeled_tuple_element m pat_tuple_elt lte in
  let exp_tuple_elt m te =
    match (te.lte_label, te.lte_elt) with
    (* [ ~x:x ] -> [ ~x ] *)
    | ( Some lbl
      , {pexp_desc= Pexp_ident {txt= Lident v_txt; _}; pexp_attributes= []; _}
      )
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
  let exp_tuple_elt m lte = map_labeled_tuple_element m exp_tuple_elt lte in
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
          pexp_desc= Pexp_record (fields, Option.map ~f:(m.expr m) with_) }
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

module Printast = struct
  include Printast

  let use_file = Format.pp_print_list top_phrase

  let repl_file = Format.pp_print_list repl_phrase

  let ast (type a) fmt (t : a t) =
    match t with
    | Structure {ast; _} -> implementation fmt ast
    | Signature {ast; _} -> interface fmt ast
    | Use_file {ast; _} -> use_file fmt ast
    | Core_type {ast; _} -> core_type fmt ast
    | Module_type {ast; _} -> module_type fmt ast
    | Expression {ast; _} -> expression fmt ast
    | Pattern {ast; _} -> pattern fmt ast
    | Repl_file {ast; _} -> repl_file fmt ast
    | Documentation ast -> Docstring.dump fmt ast
end

module Asttypes = struct
  include Asttypes

  let is_override = function Override -> true | Fresh -> false

  let is_recursive = function Recursive -> true | Nonrecursive -> false
end

exception Warning50 of (Location.t * Warnings.t) list

module W = struct
  type t = int

  let in_lexer : t list = [1; 2; 3; 14; 29]

  let disable x = -abs x

  let enable x = abs x

  let to_string x =
    String.concat ~sep:"" (List.map ~f:(Format.sprintf "%+d") x)
end

let tokens lexbuf =
  let rec loop acc =
    match Lexer.token_with_comments lexbuf with
    (* The location in lexbuf are invalid for comments *)
    | COMMENT (_, loc) as tok -> loop ((tok, loc) :: acc)
    | DOCSTRING ds as tok -> loop ((tok, Docstrings.docstring_loc ds) :: acc)
    | tok -> (
        let loc = Migrate_ast.Location.of_lexbuf lexbuf in
        let acc = (tok, loc) :: acc in
        match tok with EOF -> List.rev acc | _ -> loop acc )
  in
  loop []

let collect_comments () =
  List.map (Lexer.comments ()) ~f:(function
    | `Comment txt, loc -> Cmt.create_comment txt loc
    | `Docstring txt, loc -> Cmt.create_docstring txt loc )

let parse_ocaml (type a) ?(disable_w50 = false) ?(disable_deprecated = false)
    (fg : a Kind.t) (conf : Conf.t) ~input_name ~source : a t =
  let warnings =
    if conf.opr_opts.quiet.v then List.map ~f:W.disable W.in_lexer else []
  in
  let warnings = if disable_w50 then warnings else W.enable 50 :: warnings in
  ignore @@ Warnings.parse_options false (W.to_string warnings) ;
  let w50 = ref [] in
  let lexbuf, prefix = prepare_lexbuf fg ~input_name source in
  let t =
    Warning.with_warning_filter
      ~filter_warning:(fun loc warn ->
        if
          Warning.is_unexpected_docstring warn
          && conf.opr_opts.comment_check.v
        then (
          w50 := (loc, warn) :: !w50 ;
          false )
        else not conf.opr_opts.quiet.v )
      ~filter_alert:(fun _loc alert ->
        if Warning.is_deprecated_alert alert && disable_deprecated then false
        else not conf.opr_opts.quiet.v )
      ~f:(fun () ->
        let ocaml_version = conf.opr_opts.ocaml_version.v in
        let preserve_beginend =
          Poly.(conf.fmt_opts.exp_grouping.v = `Preserve)
        in
        let prefer_let_puns =
          match conf.fmt_opts.letop_punning.v with
          | `Always -> Some true
          | `Never -> Some false
          | `Preserve -> None
        in
        let nm =
          normalize_mapper ~ocaml_version ~preserve_beginend ~prefer_let_puns
        in
        let ocaml_version_pair =
          Some Ocaml_version.(major ocaml_version, minor ocaml_version)
        in
        let parse_std (type std) (std_fg : std Std_ast.t) : std =
          (* Suppress warnings during raw std parse to avoid duplicate w50
             warnings — w50 handling is done at the OCaml-parser level. *)
          let std_str =
            if String.is_empty prefix then source
            else
              let pos = String.length prefix in
              String.sub source ~pos ~len:(String.length source - pos)
          in
          Warning.with_warning_filter
            ~filter_warning:(fun _loc _warn -> false)
            ~filter_alert:(fun _loc _alert -> false)
            ~f:(fun () ->
              Std_ast.Parse.ast std_fg ~ocaml_version ~input_name std_str )
        in
        let debug = conf.opr_opts.debug.v in
        let metadata () =
          let comments = collect_comments () in
          Warnings.check_fatal () ;
          let tokens =
            let lexbuf, _ = prepare_lexbuf fg ~input_name source in
            tokens lexbuf
          in
          (comments, Source.create ~text:source ~tokens)
        in
        let make_cmts ~walk ~ast ~print_ast =
          let comments, source = metadata () in
          Cmts.init ~debug ~source ~ast ~comments ~traverse:walk ~print_ast
        in
        let make_paired (type ext std) ~parse_ext
            ~(walk : Ast_mapper.mapper -> ext -> ext)
            ~(std_fg : std Std_ast.t) ~print_ast : ext * std * Cmts.t =
          let ast =
            walk nm (parse_ext ~ocaml_version:ocaml_version_pair lexbuf)
          in
          let std = parse_std std_fg in
          let cmts = make_cmts ~walk ~ast ~print_ast in
          (ast, std, cmts)
        in
        ( match fg with
          | Structure ->
              let ast, std, cmts =
                make_paired ~parse_ext:Parse.implementation
                  ~walk:(fun m -> m.structure m)
                  ~std_fg:Std_ast.Structure
                  ~print_ast:Printast.implementation
              in
              Structure {ast; std; cmts}
          | Signature ->
              let ast, std, cmts =
                make_paired ~parse_ext:Parse.interface
                  ~walk:(fun m -> m.signature m)
                  ~std_fg:Std_ast.Signature ~print_ast:Printast.interface
              in
              Signature {ast; std; cmts}
          | Use_file ->
              let ast, std, cmts =
                make_paired ~parse_ext:Parse.use_file
                  ~walk:(fun m -> List.map ~f:(m.toplevel_phrase m))
                  ~std_fg:Std_ast.Use_file ~print_ast:Printast.use_file
              in
              Use_file {ast; std; prefix; cmts}
          | Core_type ->
              let ast, std, cmts =
                make_paired ~parse_ext:Parse.core_type
                  ~walk:(fun m -> m.typ m)
                  ~std_fg:Std_ast.Core_type ~print_ast:Printast.core_type
              in
              Core_type {ast; std; cmts}
          | Module_type ->
              let ast, std, cmts =
                make_paired ~parse_ext:Parse.module_type
                  ~walk:(fun m -> m.module_type m)
                  ~std_fg:Std_ast.Module_type ~print_ast:Printast.module_type
              in
              Module_type {ast; std; cmts}
          | Expression ->
              let ast, std, cmts =
                make_paired ~parse_ext:Parse.expression
                  ~walk:(fun m -> m.expr m)
                  ~std_fg:Std_ast.Expression ~print_ast:Printast.expression
              in
              Expression {ast; std; cmts}
          | Pattern ->
              let ast, std, cmts =
                make_paired ~parse_ext:Parse.pattern
                  ~walk:(fun m -> m.pat m)
                  ~std_fg:Std_ast.Pattern ~print_ast:Printast.pattern
              in
              Pattern {ast; std; cmts}
          | Repl_file ->
              let walk (m : Ast_mapper.mapper) =
                List.map ~f:(m.repl_phrase m)
              in
              let ast =
                walk nm
                  (Toplevel_lexer.repl_file ~ocaml_version:ocaml_version_pair
                     lexbuf )
              in
              let cmts =
                make_cmts ~walk ~ast ~print_ast:Printast.repl_file
              in
              Repl_file {ast; cmts}
          | Documentation -> assert false
          : a t ) )
  in
  match List.rev !w50 with [] -> t | w50 -> raise (Warning50 w50)

let parse (type a) ?disable_w50 ?disable_deprecated (k : a Kind.t) conf
    ~input_name ~source : a t =
  match k with
  | Documentation ->
      let pos = {Lexing.dummy_pos with pos_fname= input_name} in
      Documentation (Docstring.parse_file pos source)
  | k ->
      parse_ocaml ?disable_w50 ?disable_deprecated k conf ~input_name ~source

(** [is_repl_block x] returns whether [x] is a list of REPL phrases and
    outputs of the form:

    {v
    # let this is = some phrase;;
    this is some output
    v} *)
let is_repl_block x =
  String.length x >= 2 && Char.equal x.[0] '#' && Char.is_whitespace x.[1]

let parse_toplevel ?disable_w50 ?disable_deprecated (conf : Conf.t)
    ~input_name ~source : (use_file t, repl_file t) Either.t =
  if is_repl_block source && conf.fmt_opts.parse_toplevel_phrases.v then
    Either.Second
      (parse ?disable_w50 ?disable_deprecated Repl_file conf ~input_name
         ~source )
  else
    First
      (parse ?disable_w50 ?disable_deprecated Use_file conf ~input_name
         ~source )

type std_value = Std_value : 'a Std_ast.t * 'a -> std_value

let get_std (type a) (t : a t) : std_value option =
  match t with
  | Structure {std; _} -> Some (Std_value (Structure, std))
  | Signature {std; _} -> Some (Std_value (Signature, std))
  | Use_file {std; _} -> Some (Std_value (Use_file, std))
  | Core_type {std; _} -> Some (Std_value (Core_type, std))
  | Module_type {std; _} -> Some (Std_value (Module_type, std))
  | Expression {std; _} -> Some (Std_value (Expression, std))
  | Pattern {std; _} -> Some (Std_value (Pattern, std))
  | Repl_file _ -> None
  | Documentation _ -> None

type std_pair = Std_pair : 'a Std_ast.t * 'a * 'a -> std_pair

let get_std_pair (type a) (t1 : a t) (t2 : a t) : std_pair option =
  match (t1, t2) with
  | Structure {std= s1; _}, Structure {std= s2; _} ->
      Some (Std_pair (Structure, s1, s2))
  | Signature {std= s1; _}, Signature {std= s2; _} ->
      Some (Std_pair (Signature, s1, s2))
  | Use_file {std= s1; _}, Use_file {std= s2; _} ->
      Some (Std_pair (Use_file, s1, s2))
  | Core_type {std= s1; _}, Core_type {std= s2; _} ->
      Some (Std_pair (Core_type, s1, s2))
  | Module_type {std= s1; _}, Module_type {std= s2; _} ->
      Some (Std_pair (Module_type, s1, s2))
  | Expression {std= s1; _}, Expression {std= s2; _} ->
      Some (Std_pair (Expression, s1, s2))
  | Pattern {std= s1; _}, Pattern {std= s2; _} ->
      Some (Std_pair (Pattern, s1, s2))
  | Repl_file _, Repl_file _ -> None
  | Documentation _, Documentation _ -> None

let dump (type a) fmt (t : a t) =
  match get_std t with
  | Some (Std_value (std_fg, std_v)) -> Std_ast.Printast.ast std_fg fmt std_v
  | None -> Printast.ast fmt t

let dump_normalized (type a) ~normalize_code conf fmt (t : a t) =
  match get_std t with
  | Some (Std_value (std_fg, std_v)) ->
      Std_ast.Printast.ast std_fg fmt
        (Normalize_std_ast.ast std_fg ~normalize_code conf std_v)
  | None -> Printast.ast fmt t

type ast_check_result =
  | Ast_preserved
  | Docstrings_moved of Cmt.error list
  | Ast_changed

let equivalent (type a) ~normalize_code conf (old_t : a t) (new_t : a t) :
    ast_check_result =
  match get_std_pair old_t new_t with
  | None ->
      (* TODO: Repl_file and Documentation have no std AST, so we skip the
         equivalence check.

         - Repl_file: could validate each toplevel phrase individually.

         - Documentation: could check each formatted code block for AST
         preservation. *)
      Ast_preserved
  | Some (Std_pair (std_fg, old_std, new_std)) ->
      if
        Normalize_std_ast.equal std_fg ~normalize_code
          ~ignore_doc_comments:(not conf.Conf.opr_opts.comment_check.v)
          conf old_std new_std
      then Ast_preserved
      else if
        Normalize_std_ast.equal std_fg ~normalize_code
          ~ignore_doc_comments:true conf old_std new_std
      then
        Docstrings_moved
          (Normalize_std_ast.moved_docstrings ~normalize_code std_fg conf
             old_std new_std )
      else Ast_changed
