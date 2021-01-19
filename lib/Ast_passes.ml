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

  type t =
    | Past_str of structure
    | Past_sig of signature
    | Past_usf of use_file

  class map =
    object (self)
      inherit Ppxlib.Ast_traverse.map

      method use_file x = List.map x ~f:self#toplevel_phrase

      method ast =
        function
        | Past_str x -> Past_str (self#structure x)
        | Past_sig x -> Past_sig (self#signature x)
        | Past_usf x -> Past_usf (self#use_file x)
    end

  let equal : t -> t -> bool = Poly.equal

  let map (m : map) : t -> t = m#ast

  let iter (i : Ppxlib.Ast_traverse.iter) : t -> unit = function
    | Past_str x -> i#structure x
    | Past_sig x -> i#signature x
    | Past_usf x -> i#list i#toplevel_phrase x

  let fold (f : 'a Ppxlib.Ast_traverse.fold) : t -> 'a -> 'a =
   fun x acc ->
    match x with
    | Past_str x -> f#structure x acc
    | Past_sig x -> f#signature x acc
    | Past_usf x -> f#list f#toplevel_phrase x acc

  module Parse = struct
    let implementation = Ppxlib_ast.Parse.implementation

    let interface = Ppxlib_ast.Parse.interface

    let use_file lexbuf =
      List.filter (Ppxlib_ast.Parse.use_file lexbuf)
        ~f:(fun (p : Parsetree.toplevel_phrase) ->
          match p with
          | Ptop_def [] -> false
          | Ptop_def (_ :: _) | Ptop_dir _ -> true )

    let ast ~(kind : Syntax.t) lexbuf : t =
      match kind with
      | Structure -> Past_str (implementation lexbuf)
      | Signature -> Past_sig (interface lexbuf)
      | Use_file -> Past_usf (use_file lexbuf)

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

    let ast ppf = function
      | Past_str x -> implementation ppf x
      | Past_sig x -> interface ppf x
      | Past_usf x -> use_file ppf x
  end

  module Pprintast = Ppxlib.Pprintast
end

let run = Fn.id
