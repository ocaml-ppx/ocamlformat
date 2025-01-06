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

open Ocamlformat_parser_standard
include Parsetree

type use_file = toplevel_phrase list

type 'a t =
  | Structure : structure t
  | Signature : signature t
  | Use_file : use_file t
  | Core_type : core_type t
  | Module_type : module_type t
  | Expression : expression t
  | Pattern : pattern t
  (* not implemented *)
  | Repl_file : unit t
  | Documentation : unit t

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
  | Use_file ->
      List.filter_map ~f:(fun x ->
          match m.toplevel_phrase m x with
          | Ptop_def [] -> None
          | Ptop_def _ as x -> Some x
          | Ptop_dir _ as x -> Some x )
  | Core_type -> m.typ m
  | Module_type -> m.module_type m
  | Expression -> m.expr m
  | Pattern -> m.pat m
  | Repl_file -> Fn.id
  | Documentation -> Fn.id

module Parse = struct
  let ast (type a) (fg : a t) ~ocaml_version ~input_name str : a =
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
    | Repl_file -> ()
    | Documentation -> ()
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
    | Pattern -> pattern 0
    | Repl_file -> fun _ _ -> ()
    | Documentation -> fun _ _ -> ()
end
