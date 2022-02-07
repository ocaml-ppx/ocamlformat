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

open Parser_extended
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
  | Repl_file : repl_file t
  | Documentation : Odoc_parser.Ast.t t

let equal (type a) (_ : a t) : a -> a -> bool = Poly.equal

let map (type a) (x : a t) (m : Ast_mapper.mapper) : a -> a =
  match x with
  | Structure -> m.structure m
  | Signature -> m.signature m
  | Use_file -> List.map ~f:(m.toplevel_phrase m)
  | Core_type -> m.typ m
  | Module_type -> m.module_type m
  | Expression -> m.expr m
  | Repl_file -> List.map ~f:(m.repl_phrase m)
  | Documentation -> Fn.id

module Parse = struct
  let fix_letop_locs =
    let binding_op (m : Ast_mapper.mapper) b =
      let b' =
        let loc_start = b.pbop_op.loc.loc_start in
        let loc_end = b.pbop_exp.pexp_loc.loc_end in
        {b with pbop_loc= {b.pbop_loc with loc_start; loc_end}}
      in
      Ast_mapper.default_mapper.binding_op m b'
    in
    Ast_mapper.{default_mapper with binding_op}

  let list_pat pat =
    match pat.ppat_desc with
    (* Do not normalize (x :: []) *)
    | Ppat_cons (_ :: _ :: _ :: _ as l) -> (
      match List.last_exn l with
      (* Empty lists are always represented as Lident [] *)
      | { ppat_desc= Ppat_construct ({txt= Lident "[]"; loc= _}, None)
        ; ppat_attributes= []
        ; _ } ->
          Some List.(rev (tl_exn (rev l)))
      | _ -> None )
    | _ -> None

  let list_exp exp =
    match exp.pexp_desc with
    (* Do not normalize (x :: []) *)
    | Pexp_cons (_ :: _ :: _ :: _ as l) -> (
      match List.last_exn l with
      (* Empty lists are always represented as Lident [] *)
      | { pexp_desc= Pexp_construct ({txt= Lident "[]"; loc= _}, None)
        ; pexp_attributes= []
        ; _ } ->
          Some List.(rev (tl_exn (rev l)))
      | _ -> None )
    | _ -> None

  let normalize_lists =
    let expr (m : Ast_mapper.mapper) e =
      let e' =
        match list_exp e with
        | Some exprs -> {e with pexp_desc= Pexp_list exprs}
        | None -> e
      in
      Ast_mapper.default_mapper.expr m e'
    in
    let pat (m : Ast_mapper.mapper) p =
      let p' =
        match list_pat p with
        | Some pats -> {p with ppat_desc= Ppat_list pats}
        | None -> p
      in
      Ast_mapper.default_mapper.pat m p'
    in
    Ast_mapper.{default_mapper with expr; pat}

  let remove_beginend_nodes =
    let expr (m : Ast_mapper.mapper) e =
      let e' =
        match e with
        | {pexp_desc= Pexp_beginend e'; pexp_attributes= []; _} -> e'
        | _ -> e
      in
      Ast_mapper.default_mapper.expr m e'
    in
    Ast_mapper.{default_mapper with expr}

  let normalize fg ~preserve_beginend x =
    map fg fix_letop_locs @@ map fg normalize_lists
    @@ (if preserve_beginend then Fn.id else map fg remove_beginend_nodes)
    @@ x

  let ast (type a) (fg : a t) ~preserve_beginend ~input_name str : a =
    normalize fg ~preserve_beginend
    @@
    let lexbuf = Lexing.from_string str in
    Location.init lexbuf input_name ;
    match fg with
    | Structure -> Parse.implementation lexbuf
    | Signature -> Parse.interface lexbuf
    | Use_file -> Parse.use_file lexbuf
    | Core_type -> Parse.core_type lexbuf
    | Module_type -> Parse.module_type lexbuf
    | Expression -> Parse.expression lexbuf
    | Repl_file -> Toplevel_lexer.repl_file lexbuf
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
    | Repl_file -> repl_file
    | Documentation -> Docstring.dump
end

module Asttypes = struct
  include Asttypes

  let is_override = function Override -> true | Fresh -> false

  let is_recursive = function Recursive -> true | Nonrecursive -> false
end
