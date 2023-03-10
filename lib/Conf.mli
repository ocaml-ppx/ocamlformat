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

include module type of Conf_t

val default_profile : from -> fmt_opts

val default : t

val update : ?quiet:bool -> t -> Parsetree.attribute -> t
(** [update ?quiet c a] updates configuration [c] after reading attribute
    [a]. [quiet] is false by default. *)

val update_value : t -> name:string -> value:string -> (t, Error.t) Result.t

val update_state : t -> [`Enable | `Disable] -> t

val parse_state_attr : Parsetree.attribute -> [`Enable | `Disable] option

val parse_line :
     t
  -> ?version_check:bool
  -> ?disable_conf_attrs:bool
  -> from:[< `Attribute of Warnings.loc | `File of Warnings.loc]
  -> string
  -> (t, Error.t) Result.t

val print_config : t -> unit

val collect_warnings : (unit -> t) -> t * (unit -> unit)

val warn :
  loc:Warnings.loc -> ('a, Format.formatter, unit, unit) format4 -> 'a

module UI : sig
  val profile : t Conf_decl.UI.t

  val fmt_opts : t Conf_decl.UI.t list

  val opr_opts : t Conf_decl.UI.t list
end

module Operational : sig
  val update : f:(opr_opts -> opr_opts) -> t -> t
end

val term : (t -> t) Cmdliner.Term.t

val options : Conf_decl.Store.t
