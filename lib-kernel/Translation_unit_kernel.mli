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

exception
  Internal_error of
    [ `Cannot_parse of exn
    | `Ast_changed
    | `Doc_comment of Normalize.docstring_error list
    | `Comment
    | `Comment_dropped of Cmt.t list
    | `Warning50 of (Location.t * Warnings.t) list ]
    * (string * Sexp.t) list

module Error : sig
  type t =
    | Invalid_source of {exn: exn; input_name: string}
    | Unstable of
        {iteration: int; prev: string; next: string; input_name: string}
    | Ocamlformat_bug of {exn: exn; input_name: string}
    | User_error of string

  val equal : t -> t -> bool
end

val parse_and_format :
     Syntax.t
  -> ?output_file:string
  -> input_name:string
  -> source:string
  -> Conf.t
  -> Conf.opts
  -> (string, Error.t) Result.t
(** [parse_and_format kind conf ?output_file ~input_name ~source] parses and
    formats [source] as a list of fragments. *)
