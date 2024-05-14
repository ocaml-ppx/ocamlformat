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

(** [offset] is the column at which the content of the comment begins. It is
    used to adjust the margin. *)
type fmt_code =
     Conf.t
  -> offset:int
  -> set_margin:bool
  -> string
  -> (Fmt.t, [`Msg of string]) Result.t

val fmt_ast :
  Conf.t -> fmt_code:fmt_code -> Ocamlformat_odoc_parser.Ast.t -> Fmt.t

val fmt_parsed :
     Conf.t
  -> ?trailing_space:Fmt.t
  -> fmt_code:fmt_code
  -> input:string
  -> offset:int
  -> ( Ocamlformat_odoc_parser.Ast.t
     , Ocamlformat_odoc_parser.Warning.t list )
     Result.t
  -> Fmt.t
(** [source] is the global source in which the locations in the AST make
    sense. [input] is the content of the doc-comment. *)
