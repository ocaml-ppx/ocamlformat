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

type 'a item =
  | Structure : Extended_ast.structure item
  | Signature : Extended_ast.signature item
  | Use_file : Extended_ast.use_file item

type 'a t =
  { attr_loc: Location.t
  ; chunk_loc: Location.t
  ; state: [`Enable | `Disable]
  ; items: 'a list }

val split :
  state:[`Enable | `Disable] -> 'a list item -> 'a list -> 'a t list
