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

val parse :
     loc:Warnings.loc
  -> string
  -> ( Ocamlformat_odoc_parser.Ast.t
     , Ocamlformat_odoc_parser.Warning.t list )
     Result.t

val parse_file : Lexing.position -> string -> Ocamlformat_odoc_parser.Ast.t
(** Used for parsing [.mld] files. Exceptions are caught in
    [Translation_unit]. *)

val warn : Format.formatter -> Ocamlformat_odoc_parser.Warning.t -> unit

val is_tag_only : Ocamlformat_odoc_parser.Ast.t -> bool
(** [true] if the documentation only contains tags *)

val normalize :
     parse_docstrings:bool
  -> normalize_code:(string -> string)
  -> string
  -> string

val normalize_text : string -> string

val dump : Format.formatter -> Ocamlformat_odoc_parser.Ast.t -> unit
