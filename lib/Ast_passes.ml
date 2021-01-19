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
end

module Ast_final = Ast0

let run = Fn.id
