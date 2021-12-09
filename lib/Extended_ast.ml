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

type 'a t =
  | Structure : structure t
  | Signature : signature t
  | Use_file : use_file t
  | Core_type : core_type t
  | Module_type : module_type t
  | Expression : expression t

let map (type a) (x : a t) (m : Ast_mapper.mapper) : a -> a =
  match x with
  | Structure -> m.structure m
  | Signature -> m.signature m
  | Use_file -> List.map ~f:(m.toplevel_phrase m)
  | Core_type -> m.typ m
  | Module_type -> m.module_type m
  | Expression -> m.expr m

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

  let normalize fg x = map fg fix_letop_locs @@ map fg normalize_lists @@ x

  let ast (type a) (fg : a t) lexbuf : a =
    normalize fg
    @@
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

module Printast = struct
  include Printast

  let use_file = Format.pp_print_list top_phrase

  let ast (type a) : a t -> _ -> a -> _ = function
    | Structure -> implementation
    | Signature -> interface
    | Use_file -> use_file
    | Core_type -> core_type 0
    | Module_type -> module_type 0
    | Expression -> expression 0
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
                              Pexp_constant
                                {pconst_desc= Pconst_string (doc, _, None); _}
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
end
