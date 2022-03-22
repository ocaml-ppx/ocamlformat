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

open Ocaml_413_extended
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
    let rec list_pat_ pat acc =
      match pat.ppat_desc with
      | Ppat_construct ({txt= Lident "[]"; loc= _}, None) -> (
        (* Empty lists are always represented as Lident [] *)
        match acc with [] -> None | _ -> Some (List.rev acc) )
      | Ppat_construct
          ( {txt= Lident "::"; loc= _}
          , Some
              ( []
              , { ppat_desc= Ppat_tuple [hd; ({ppat_attributes= []; _} as tl)]
                ; ppat_attributes= []
                ; _ } ) ) ->
          list_pat_ tl (hd :: acc)
      | _ -> None
    in
    list_pat_ pat []

  let list_exp exp =
    let rec list_exp_ exp acc =
      match exp.pexp_desc with
      | Pexp_construct ({txt= Lident "[]"; loc= _}, None) -> (
        (* Empty lists are always represented as Lident [] *)
        match acc with [] -> None | _ -> Some (List.rev acc) )
      | Pexp_construct
          ( {txt= Lident "::"; loc= _}
          , Some
              { pexp_desc= Pexp_tuple [hd; ({pexp_attributes= []; _} as tl)]
              ; pexp_attributes= []
              ; _ } ) ->
          list_exp_ tl (hd :: acc)
      | _ -> None
    in
    list_exp_ exp []

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

  let ast (type a) (fg : a t) ~preserve_beginend lexbuf : a =
    normalize fg ~preserve_beginend
    @@
    match fg with
    | Structure -> Parse.implementation lexbuf
    | Signature -> Parse.interface lexbuf
    | Use_file -> Parse.use_file lexbuf
    | Core_type -> Parse.core_type lexbuf
    | Module_type -> Parse.module_type lexbuf
    | Expression -> Parse.expression lexbuf
    | Repl_file -> Toplevel_lexer.repl_file lexbuf
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
end

module Asttypes = struct
  include Asttypes

  let is_private = function Private -> true | Public -> false

  let is_open : closed_flag -> bool = function
    | Open -> true
    | Closed -> false

  let is_override = function Override -> true | Fresh -> false

  let is_mutable = function Mutable -> true | Immutable -> false

  let is_recursive = function Recursive -> true | Nonrecursive -> false
end
