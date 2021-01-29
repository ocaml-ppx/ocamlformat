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

val split :
     range:int * int
  -> split_on_semisemi:bool
  -> Cmt_lexer.token list
  -> string * (int * int)
(** Exposed for tests. *)

val fragment :
  'a Extended_ast.t -> range:int * int -> string -> string * (int * int)
(** [fragment fg ~range x] slices [x] into a smaller string, filtered
    according to the range of lines [range], and containing the relevant
    context so we can infer the indentation to apply to this range of lines. *)
