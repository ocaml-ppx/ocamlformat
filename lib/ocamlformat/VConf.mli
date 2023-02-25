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

(** Versionned configuration.  *)

module type Ocamlformat_interface = sig
  val parse_and_format :
       Syntax.t
    -> ?output_file:string
    -> input_name:string
    -> source:string
    -> Conf.t
    -> (string, Translation_unit_error.t) Result.t
  (** [parse_and_format kind ?output_file ~input_name ~source conf opts]
      parses and formats [source] as a list of fragments. *)

  val numeric :
       Syntax.t
    -> input_name:string
    -> source:string
    -> range:Range.t
    -> Conf.t
    -> int list
  (** [numeric ~input_name ~source ~range conf opts] returns the indentation
      of the range of lines [range] (line numbers ranging from 1 to number of
      lines), where the line numbers are relative to [source] and the
      indentation is relative to the formatted output. *)

  val print_error :
       ?debug:bool
    -> ?quiet:bool
    -> Format.formatter
    -> Translation_unit_error.t
    -> unit
end


type ocamlformat_module = (module Ocamlformat_interface)

type t = {version_number: Version.t; version_module: ocamlformat_module; conf: Conf.t}


val latest : ocamlformat_module

val oldest : ocamlformat_module

val module_of_version : Version.t -> (ocamlformat_module, Translation_unit_error.t) Result.t

val module_of_string : string -> ocamlformat_module option

val default : t

val parse_line :
     t
  -> ?version_check:bool
  -> ?disable_conf_attrs:bool
  -> from:[< `Attribute of Warnings.loc | `File of Warnings.loc > `File]
  -> string
  -> (t, Conf.Error.t) Result.t

val update : ?quiet:bool -> t -> Parsetree.attribute -> t
(** [update ?quiet c a] updates configuration [c] after reading attribute
    [a]. [quiet] is false by default. *)

val term : (t -> t) Cmdliner.Term.t

val map_conf : f:(Conf.t -> Conf.t) -> t -> t