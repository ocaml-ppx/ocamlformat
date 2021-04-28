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

module Ast0 = struct
  include Ppxlib.Parsetree

  let equal_core_type : core_type -> core_type -> bool = Poly.equal

  type use_file = toplevel_phrase list

  type 'a t =
    | Structure : structure t
    | Signature : signature t
    | Use_file : use_file t
    | Core_type : core_type t
    | Module_type : module_type t
    | Expression : expression t

  let equal (type a) (_ : a t) : a -> a -> bool = Poly.equal

  class map =
    object
      inherit Ppxlib.Ast_traverse.map
    end

  let map (type a) (x : a t) (m : Ppxlib.Ast_traverse.map) : a -> a =
    match x with
    | Structure -> m#structure
    | Signature -> m#signature
    | Use_file -> m#list m#toplevel_phrase
    | Core_type -> m#core_type
    | Module_type -> m#module_type
    | Expression -> m#expression

  let iter (type a) (fg : a t) (i : Ppxlib.Ast_traverse.iter) : a -> unit =
    match fg with
    | Structure -> i#structure
    | Signature -> i#signature
    | Use_file -> i#list i#toplevel_phrase
    | Core_type -> i#core_type
    | Module_type -> i#module_type
    | Expression -> i#expression

  let fold (type a) (fg : a t) (f : _ Ppxlib.Ast_traverse.fold) : a -> _ =
    match fg with
    | Structure -> f#structure
    | Signature -> f#signature
    | Use_file -> f#list f#toplevel_phrase
    | Core_type -> f#core_type
    | Module_type -> f#module_type
    | Expression -> f#expression

  module Parse = struct
    let implementation = Ppxlib_ast.Parse.implementation

    let interface = Ppxlib_ast.Parse.interface

    let use_file lexbuf =
      List.filter (Ppxlib_ast.Parse.use_file lexbuf)
        ~f:(fun (p : toplevel_phrase) ->
          match p with
          | Ptop_def [] -> false
          | Ptop_def (_ :: _) | Ptop_dir _ -> true )

    let core_type = Ppxlib_ast.Parse.core_type

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

    let expression = Ppxlib_ast.Parse.expression

    let ast (type a) (fg : a t) lexbuf : a =
      match fg with
      | Structure -> implementation lexbuf
      | Signature -> interface lexbuf
      | Use_file -> use_file lexbuf
      | Core_type -> core_type lexbuf
      | Module_type -> module_type lexbuf
      | Expression -> expression lexbuf

    let parser_version = Ocaml_version.sys_version
  end
end

module Ast_final = struct
  include Ast0

  module Printast = struct
    let pp_sexp ppf sexp =
      Format.fprintf ppf "%a" (Sexp.pp_hum_indent 2) sexp

    let sexp_of = Ppxlib.Ast_traverse.sexp_of

    let implementation ppf x = pp_sexp ppf (sexp_of#structure x)

    let interface ppf x = pp_sexp ppf (sexp_of#signature x)

    let expression ppf x = pp_sexp ppf (sexp_of#expression x)

    let payload ppf x = pp_sexp ppf (sexp_of#payload x)

    let use_file ppf x =
      pp_sexp ppf (List.sexp_of_t sexp_of#toplevel_phrase x)

    let core_type ppf x = pp_sexp ppf (sexp_of#core_type x)

    let module_type ppf x = pp_sexp ppf (sexp_of#module_type x)

    let ast (type a) : a t -> _ -> a -> _ = function
      | Structure -> implementation
      | Signature -> interface
      | Use_file -> use_file
      | Core_type -> core_type
      | Module_type -> module_type
      | Expression -> expression
  end

  module Pprintast = Ppxlib.Pprintast

  let mk (type a b) (fg0 : a Ast0.t) (fg_final : b t) : a -> b =
    match (fg0, fg_final) with
    | Structure, Structure -> Fn.id
    | Signature, Signature -> Fn.id
    | Use_file, Use_file -> Fn.id
    | Core_type, Core_type -> Fn.id
    | Module_type, Module_type -> Fn.id
    | Expression, Expression -> Fn.id
    | _ -> failwith "Ast_final.mk: missing transformations (impossible)"
end

let run = Ast_final.mk
