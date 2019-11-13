(**********************************************************************
 *                                                                    *
 *                            OCamlFormat                             *
 *                                                                    *
 *  Copyright (c) 2019-present, Facebook, Inc.  All rights reserved.  *
 *                                                                    *
 *  This source code is licensed under the MIT license found in the   *
 *  LICENSE file in the root directory of this source tree.           *
 *                                                                    *
 **********************************************************************)

module type CONFIG = sig
  type config

  val profile_option_names : string list

  val warn : config -> ('a, Format.formatter, unit, unit) format4 -> 'a
end

module Make (C : CONFIG) : sig
  type config = C.config

  type 'a t

  type parsed_from = [`File of Fpath.t * int | `Attribute]

  type updated_from = [`Env | `Commandline | `Parsed of parsed_from]

  type 'a option_decl =
       names:string list
    -> doc:string
    -> section:[`Formatting | `Operational]
    -> ?allow_inline:bool
    -> ?deprecated:bool
    -> (config -> 'a -> config)
    -> (config -> 'a)
    -> 'a t

  val section_name : [`Formatting | `Operational] -> string

  type removed_value
  (** Indicate that a configuration value has been removed in an ocamlformat
      release. A message indicating how to migrate will be displayed. *)

  val removed_value :
    name:string -> version:string -> msg:string -> removed_value
  (** [name] is the configuration value that was removed in version
      [version]. [msg] explains how to get the former behaviour. *)

  val removed_values :
    names:string list -> version:string -> msg:string -> removed_value list
  (** Shorthand for [removed_value] when [version] and [msg] are shared. This
      can be used when multiple values are removed at the same time. *)

  val choice :
       all:(string * 'a * string) list
    -> ?removed_values:removed_value list
    -> 'a option_decl

  val flag : default:bool -> bool option_decl

  val any :
    'a Cmdliner.Arg.conv -> default:'a -> docv:string -> 'a option_decl

  val default : 'a t -> 'a

  val update_using_cmdline : config -> config

  val update :
       config:config
    -> from:updated_from
    -> name:string
    -> value:string
    -> inline:bool
    -> ( config
       , [ `Unknown of string * string
         | `Bad_value of string * string
         | `Malformed of string
         | `Misplaced of string * string ] )
       Result.t

  val print_config : config -> unit
end
