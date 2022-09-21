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

type state = Enable | Disable of Location.t

type 'a t =
  | Structure : Extended_ast.structure t
  | Signature : Extended_ast.signature t
  | Use_file : Extended_ast.use_file t

val split : 'a list t -> Conf.t -> 'a list -> (state * 'a list) list
