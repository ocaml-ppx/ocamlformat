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

(* Missing from ocaml_migrate_parsetree *)
let use_file (mapper : Ast_mapper.mapper) use_file =
  let open Parsetree in
  List.map use_file ~f:(fun toplevel_phrase ->
      match (toplevel_phrase : toplevel_phrase) with
      | Ptop_def structure -> Ptop_def (mapper.structure mapper structure)
      | Ptop_dir {pdir_name; pdir_arg; pdir_loc} ->
          let pdir_arg =
            match pdir_arg with
            | None -> None
            | Some a ->
                Some {a with pdira_loc= mapper.location mapper a.pdira_loc}
          in
          Ptop_dir
            { pdir_name=
                {pdir_name with loc= mapper.location mapper pdir_name.loc}
            ; pdir_arg
            ; pdir_loc= mapper.location mapper pdir_loc } )

let map (type a) (x : a t) (m : Ast_mapper.mapper) : a -> a =
  match x with
  | Structure -> m.structure m
  | Signature -> m.signature m
  | Use_file -> use_file m
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

  let normalize fg = map fg fix_letop_locs

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
