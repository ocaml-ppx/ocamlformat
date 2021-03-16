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

module Error : sig
  type t

  val equal : t -> t -> bool

  val print : ?debug:bool -> ?quiet:bool -> Format.formatter -> t -> unit
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
