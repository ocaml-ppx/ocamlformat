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

module VConf : module type of VConf

module For_external_use : sig
(** This module exposes functions that take an optionnal [?version] argument.
    They are meant for use in ocamlformat-as-a-library, and are not used by the 
    various executables provided by the ocamlformat package. *)

  val parse_and_format :
       ?version:Version.t
    -> Syntax.t
    -> ?output_file:string
    -> input_name:string
    -> source:string
    -> Conf.t
    -> (string, Translation_unit_error.t) Result.t

  val numeric :
       ?version:Version.t
    -> Syntax.t
    -> input_name:string
    -> source:string
    -> Conf.t
    -> range:Range.t
    -> Export.int list

  val print_error :
       ?version:Version.t
    -> ?debug:Export.bool
    -> ?quiet:Export.bool
    -> Format.formatter
    -> Translation_unit_error.t
    -> unit
end

val parse_and_format :
     Syntax.t
  -> ?output_file:string
  -> input_name:string
  -> source:string
  -> VConf.t
  -> (string, Translation_unit_error.t) Result.t

val numeric :
     Syntax.t
  -> input_name:string
  -> source:string
  -> VConf.t
  -> range:Range.t
  -> Export.int list

val print_error :
     ?debug:Export.bool
  -> ?quiet:Export.bool
  -> VConf.t
  -> Format.formatter
  -> Translation_unit_error.t
  -> unit
