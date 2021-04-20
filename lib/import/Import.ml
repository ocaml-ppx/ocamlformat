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

(** Opened in each source module to establish global namespace *)

include Base
include Stdio
include Ocamlformat_stdlib
module Format = Caml.Format
module Filename = Caml.Filename

let ( >> ) f g x = g (f x)

let impossible msg = failwith msg
