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
  | Mll_file : Ocamlformat_mll_parser.Mll_ast.lexer_def t

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
  | Mll_file -> Any Mll_file

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
  | Mll_file ->
      fun d ->
        let open Ocamlformat_mll_parser.Mll_ast in
        let loc l = m.location m l in
        let map_located x = {x with loc= loc x.loc} in
        let map_code (c : ocaml_code) = map_located c in
        let map_code_opt = Option.map ~f:map_code in
        let map_named_def nd =
          { def_name= map_located nd.def_name
          ; def_body= nd.def_body
          ; def_loc= loc nd.def_loc }
        in
        let map_case c = {c with action= map_code c.action} in
        let map_entry entry =
          { entry_name= map_located entry.entry_name
          ; entry_args= List.map ~f:map_located entry.entry_args
          ; entry_is_shortest= entry.entry_is_shortest
          ; entry_cases= List.map ~f:map_case entry.entry_cases }
        in
        { header= map_code_opt d.header
        ; named_defs= List.map ~f:map_named_def d.named_defs
        ; rules= List.map ~f:map_entry d.rules
        ; trailer= map_code_opt d.trailer
        ; comments= d.comments }

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
    | Mll_file ->
        Ocamlformat_mll_parser.Mll_parser.parse_string ~input_name str
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
    | Mll_file -> Ocamlformat_mll_parser.Mll_printast.pp
end

(* Strip comment delimiters: [(* text *)] -> [text], [/* text */] ->
   [text] *)
let strip_comment_delimiters s =
  let len = String.length s in
  if
    len >= 4
    && Char.equal s.[0] '('
    && Char.equal s.[1] '*'
    && Char.equal s.[len - 2] '*'
    && Char.equal s.[len - 1] ')'
  then String.sub s ~pos:2 ~len:(len - 4)
  else if
    len >= 4
    && Char.equal s.[0] '/'
    && Char.equal s.[1] '*'
    && Char.equal s.[len - 2] '*'
    && Char.equal s.[len - 1] '/'
  then String.sub s ~pos:2 ~len:(len - 4)
  else s

module Asttypes = struct
  include Asttypes

  let is_override = function Override -> true | Fresh -> false

  let is_recursive = function Recursive -> true | Nonrecursive -> false
end

type std_value = Std_value : 'a Std_ast.t * 'a -> std_value

let get_std (type a) (fg : a t) (v : a) : std_value option =
  match fg with
  | Structure -> Some (Std_value (Structure, v.std))
  | Signature -> Some (Std_value (Signature, v.std))
  | Use_file -> Some (Std_value (Use_file, v.std))
  | Core_type -> Some (Std_value (Core_type, v.std))
  | Module_type -> Some (Std_value (Module_type, v.std))
  | Expression -> Some (Std_value (Expression, v.std))
  | Pattern -> Some (Std_value (Pattern, v.std))
  | Repl_file -> None
  | Documentation -> None
  | Mll_file -> None

type std_pair = Std_pair : 'a Std_ast.t * 'a * 'a -> std_pair

let get_std_pair (type a) (fg : a t) (v1 : a) (v2 : a) : std_pair option =
  match fg with
  | Structure -> Some (Std_pair (Structure, v1.std, v2.std))
  | Signature -> Some (Std_pair (Signature, v1.std, v2.std))
  | Use_file -> Some (Std_pair (Use_file, v1.std, v2.std))
  | Core_type -> Some (Std_pair (Core_type, v1.std, v2.std))
  | Module_type -> Some (Std_pair (Module_type, v1.std, v2.std))
  | Expression -> Some (Std_pair (Expression, v1.std, v2.std))
  | Pattern -> Some (Std_pair (Pattern, v1.std, v2.std))
  | Repl_file -> None
  | Documentation -> None
  | Mll_file -> None

let dump (type a) (fg : a t) fmt (v : a) =
  match get_std fg v with
  | Some (Std_value (std_fg, std_v)) -> Std_ast.Printast.ast std_fg fmt std_v
  | None -> Printast.ast fg fmt v

let dump_normalized (type a) (fg : a t) ~normalize_code conf fmt (v : a) =
  match get_std fg v with
  | Some (Std_value (std_fg, std_v)) ->
      Std_ast.Printast.ast std_fg fmt
        (Normalize_std_ast.ast std_fg ~normalize_code conf std_v)
  | None -> Printast.ast fg fmt v

type ast_check_result =
  | Ast_preserved
  | Docstrings_moved of Cmt.error list
  | Ast_changed

let equal_mll ~normalize_code (_conf : Conf.t)
    (a : Ocamlformat_mll_parser.Mll_ast.lexer_def)
    (b : Ocamlformat_mll_parser.Mll_ast.lexer_def) =
  let open Ocamlformat_mll_parser.Mll_ast in
  let code_equal (a : ocaml_code) (b : ocaml_code) =
    let strip s =
      let len = String.length s in
      if len >= 2 && Char.equal s.[0] '{' && Char.equal s.[len - 1] '}' then
        String.strip (String.sub s ~pos:1 ~len:(len - 2))
      else s
    in
    let sa = strip a.value and sb = strip b.value in
    String.equal sa sb
    || String.equal (normalize_code sa) (normalize_code sb)
  in
  let code_opt_equal a b =
    match (a, b) with
    | None, None -> true
    | Some a, Some b -> code_equal a b
    | _ -> false
  in
  let case_equal a b =
    Poly.equal a.pattern b.pattern && code_equal a.action b.action
  in
  let entry_equal a b =
    String.equal a.entry_name.value b.entry_name.value
    && List.equal
         (fun a b -> String.equal a.value b.value)
         a.entry_args b.entry_args
    && Bool.equal a.entry_is_shortest b.entry_is_shortest
    && List.equal case_equal a.entry_cases b.entry_cases
  in
  let def_equal a b =
    String.equal a.def_name.value b.def_name.value
    && Poly.equal a.def_body b.def_body
  in
  code_opt_equal a.header b.header
  && List.equal def_equal a.named_defs b.named_defs
  && List.equal entry_equal a.rules b.rules
  && code_opt_equal a.trailer b.trailer

let equivalent (type a) (fg : a t) ~normalize_code conf (old_v : a)
    (new_v : a) : ast_check_result =
  match get_std_pair fg old_v new_v with
  | None -> (
    match fg with
    | Mll_file ->
        if equal_mll ~normalize_code conf old_v new_v then Ast_preserved
        else Ast_changed
    | _ ->
        (* TODO: Repl_file and Documentation have no std AST, so we skip the
           equivalence check. - Repl_file: each toplevel phrase is OCaml code
           that could be validated individually by parsing it with the
           standard parser. - Documentation: OCaml code blocks inside .mld
           files are formatted but never validated for AST preservation. We
           should check each formatted code block by parsing it with the
           standard parser and comparing. *)
        Ast_preserved )
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

module Parsed = struct
  type 'a t =
    {ast: 'a; comments: Cmt.t list; prefix: string; source: Source.t}
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

let fresh_lexbuf source =
  let lexbuf = Lexing.from_string source in
  Location.init_info lexbuf !Location.input_name ;
  let hash_bang =
    Lexer.skip_hash_bang lexbuf ;
    let len = lexbuf.lex_last_pos in
    String.sub source ~pos:0 ~len
  in
  (lexbuf, hash_bang)

let split_hash_bang source =
  let lexbuf = Lexing.from_string source in
  Location.init_info lexbuf !Location.input_name ;
  Lexer.skip_hash_bang lexbuf ;
  let len = lexbuf.lex_last_pos in
  let hash_bang = String.sub source ~pos:0 ~len in
  let rest = String.sub source ~pos:len ~len:(String.length source - len) in
  (rest, hash_bang)

let collect_comments () =
  List.map (Lexer.comments ()) ~f:(function
    | `Comment txt, loc -> Cmt.create_comment txt loc
    | `Docstring txt, loc -> Cmt.create_docstring txt loc )

let parse_ocaml ?(disable_w50 = false) ?(disable_deprecated = false) fg
    (conf : Conf.t) ~input_name ~source =
  let warnings =
    if conf.opr_opts.quiet.v then List.map ~f:W.disable W.in_lexer else []
  in
  let warnings = if disable_w50 then warnings else W.enable 50 :: warnings in
  ignore @@ Warnings.parse_options false (W.to_string warnings) ;
  let w50 = ref [] in
  let t =
    let source, hash_bang = split_hash_bang source in
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
        let ast =
          Parse.ast fg ~ocaml_version ~preserve_beginend ~prefer_let_puns
            ~input_name source
        in
        let comments = collect_comments () in
        Warnings.check_fatal () ;
        let tokens =
          let lexbuf, _ = fresh_lexbuf source in
          tokens lexbuf
        in
        let source = Source.create ~text:source ~tokens in
        {Parsed.ast; comments; prefix= hash_bang; source} )
  in
  match List.rev !w50 with [] -> t | w50 -> raise (Warning50 w50)

let parse (type a) ?disable_w50 ?disable_deprecated (fg : a t) conf
    ~input_name ~source : a Parsed.t =
  match fg with
  | Documentation ->
      let pos = {Lexing.dummy_pos with pos_fname= input_name} in
      let ast = Docstring.parse_file pos source in
      { Parsed.ast
      ; comments= []
      ; prefix= ""
      ; source= Source.create ~text:source ~tokens:[] }
  | Mll_file ->
      let open Ocamlformat_mll_parser in
      let ast = Mll_parser.parse_string ~input_name source in
      let comments =
        List.map ast.Mll_ast.comments ~f:(fun (c : Mll_ast.ocaml_code) ->
            Cmt.create_comment (strip_comment_delimiters c.value) c.loc )
      in
      { Parsed.ast
      ; comments
      ; prefix= ""
      ; source= Source.create ~text:source ~tokens:[] }
  | fg ->
      parse_ocaml ?disable_w50 ?disable_deprecated fg conf ~input_name
        ~source

(** [is_repl_block x] returns whether [x] is a list of REPL phrases and
    outputs of the form:

    {v
    # let this is = some phrase;;
    this is some output
    v} *)
let is_repl_block x =
  String.length x >= 2 && Char.equal x.[0] '#' && Char.is_whitespace x.[1]

let parse_toplevel ?disable_w50 ?disable_deprecated (conf : Conf.t)
    ~input_name ~source =
  if is_repl_block source && conf.fmt_opts.parse_toplevel_phrases.v then
    Either.Second
      (parse ?disable_w50 ?disable_deprecated Repl_file conf ~input_name
         ~source )
  else
    First
      (parse ?disable_w50 ?disable_deprecated Use_file conf ~input_name
         ~source )

