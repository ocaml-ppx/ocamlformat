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

let equal_core_type : core_type -> core_type -> bool = Poly.equal

type use_file = toplevel_phrase list

type repl_file = repl_phrase list

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

let equal (type a) (_ : a t) : a -> a -> bool = Poly.equal

let map (type a) (x : a t) (m : Ast_mapper.mapper) : a -> a =
  match x with
  | Structure -> m.structure m
  | Signature -> m.signature m
  | Use_file -> List.map ~f:(m.toplevel_phrase m)
  | Core_type -> m.typ m
  | Module_type -> m.module_type m
  | Expression -> m.expr m
  | Pattern -> m.pat m
  | Repl_file -> List.map ~f:(m.repl_phrase m)
  | Documentation -> Fn.id

module Parse = struct
  let normalize_mapper ~ocaml_version ~preserve_beginend =
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
    let binding_op (m : Ast_mapper.mapper) b =
      let b' =
        let loc_start = b.pbop_op.loc.loc_start in
        let loc_end = b.pbop_exp.pexp_loc.loc_end in
        {b with pbop_loc= {b.pbop_loc with loc_start; loc_end}}
      in
      Ast_mapper.default_mapper.binding_op m b'
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
      (* [(module M) : (module T)] -> [(module M : T)] *)
      | { ppat_desc=
            Ppat_constraint
              ( {ppat_desc= Ppat_unpack (name, None); ppat_attributes= []; _}
              , {ptyp_desc= Ptyp_package pt; ptyp_attributes= []; _} )
        ; _ } as p ->
          {p with ppat_desc= Ppat_unpack (name, Some pt)}
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
      | {pexp_desc= Pexp_beginend e'; pexp_attributes= []; _}
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
      (* [(module M) : (module T)] -> [(module M : T)] *)
      | { pexp_desc=
            Pexp_constraint
              ( { pexp_desc= Pexp_pack (name, None)
                ; pexp_attributes= []
                ; pexp_loc
                ; _ }
              , {ptyp_desc= Ptyp_package pt; ptyp_attributes= []; ptyp_loc; _}
              )
        ; _ } as p
        when Migrate_ast.Location.compare_start ptyp_loc pexp_loc > 0 ->
          (* Match locations to differentiate between the two position for
             the constraint, we want to shorten the second: - [let _ :
             (module S) = (module M)] - [let _ = ((module M) : (module
             S))] *)
          {p with pexp_desc= Pexp_pack (name, Some pt)}
      | e -> Ast_mapper.default_mapper.expr m e
    in
    Ast_mapper.{default_mapper with expr; pat; binding_op}

  let ast (type a) (fg : a t) ~ocaml_version ~preserve_beginend ~input_name
      str : a =
    map fg (normalize_mapper ~ocaml_version ~preserve_beginend)
    @@
    let lexbuf = Lexing.from_string str in
    let ocaml_version =
      Some Ocaml_version.(major ocaml_version, minor ocaml_version)
    in
    Location.init_info lexbuf input_name ;
    match fg with
    | Structure -> Parse.implementation ~ocaml_version lexbuf
    | Signature -> Parse.interface ~ocaml_version lexbuf
    | Use_file -> Parse.use_file ~ocaml_version lexbuf
    | Core_type -> Parse.core_type ~ocaml_version lexbuf
    | Module_type -> Parse.module_type ~ocaml_version lexbuf
    | Expression -> Parse.expression ~ocaml_version lexbuf
    | Pattern -> Parse.pattern ~ocaml_version lexbuf
    | Repl_file -> Toplevel_lexer.repl_file ~ocaml_version lexbuf
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
    | Structure -> implementation
    | Signature -> interface
    | Use_file -> use_file
    | Core_type -> core_type
    | Module_type -> module_type
    | Expression -> expression
    | Pattern -> pattern
    | Repl_file -> repl_file
    | Documentation -> Docstring.dump
end

module Asttypes = struct
  include Asttypes

  let is_override = function Override -> true | Fresh -> false

  let is_recursive = function Recursive -> true | Nonrecursive -> false
end
