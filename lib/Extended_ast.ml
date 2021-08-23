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
  let implementation = Parse.implementation

  let interface = Parse.interface

  let use_file = Parse.use_file

  let core_type = Parse.core_type

  let module_type (lx : Lexing.lexbuf) =
    let pre = "module X : " in
    let lex_buffer_len = lx.lex_buffer_len + String.length pre in
    let input = Bytes.to_string lx.lex_buffer in
    let lex_buffer = Bytes.of_string (pre ^ input) in
    let lx = {lx with lex_buffer; lex_buffer_len} in
    match interface lx with
    | [{psig_desc= Psig_module {pmd_type; _}; _}] -> pmd_type
    | _ ->
        failwith
          (Format.sprintf "Syntax error: %s is not a module type" input)

  let expression = Parse.expression

  let ast (type a) (fg : a t) lexbuf : a =
    match fg with
    | Structure -> implementation lexbuf
    | Signature -> interface lexbuf
    | Use_file -> use_file lexbuf
    | Core_type -> core_type lexbuf
    | Module_type -> module_type lexbuf
    | Expression -> expression lexbuf
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
