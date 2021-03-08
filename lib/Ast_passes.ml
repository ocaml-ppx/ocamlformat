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

  let iter (type a) (fg : a t) (i : Ppxlib.Ast_traverse.iter) : a -> unit =
    match fg with
    | Structure -> i#structure
    | Signature -> i#signature
    | Use_file -> i#list i#toplevel_phrase

  let fold (type a) (fg : a t) (f : _ Ppxlib.Ast_traverse.fold) : a -> _ =
    match fg with
    | Structure -> f#structure
    | Signature -> f#signature
    | Use_file -> f#list f#toplevel_phrase

  module Parse = struct
    let implementation = Ppxlib_ast.Parse.implementation

    let interface = Ppxlib_ast.Parse.interface

    let use_file lexbuf =
      List.filter (Ppxlib_ast.Parse.use_file lexbuf)
        ~f:(fun (p : toplevel_phrase) ->
          match p with
          | Ptop_def [] -> false
          | Ptop_def (_ :: _) | Ptop_dir _ -> true )

    let ast (type a) (fg : a t) lexbuf : a =
      match fg with
      | Structure -> implementation lexbuf
      | Signature -> interface lexbuf
      | Use_file -> use_file lexbuf

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

    let ast (type a) : a t -> _ -> a -> _ = function
      | Structure -> implementation
      | Signature -> interface
      | Use_file -> use_file
  end

  module Pprintast = Ppxlib.Pprintast

  let mk (type a b) (fg0 : a Ast0.t) (fg_final : b t) : a -> b =
    match (fg0, fg_final) with
    | Structure, Structure -> Fn.id
    | Signature, Signature -> Fn.id
    | Use_file, Use_file -> Fn.id
    | _ -> failwith "impossible"
end

let run = Ast_final.mk
