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

module type CONFIG = sig
  type config

  val profile_option_names : string list

  val warn : config -> ('a, Format.formatter, unit, unit) format4 -> 'a
end

module Make (C : CONFIG) : sig
  type config = C.config

  type 'a t

  type kind = Formatting | Operational

  type parsed_from = [`File of Fpath.t * int | `Attribute]

  type updated_from = [`Env | `Commandline | `Parsed of parsed_from]

  type deprecated

  type removed

  type status = [`Valid | `Deprecated of deprecated | `Removed of removed]

  type 'a option_decl =
       names:string list
    -> doc:string
    -> kind:kind
    -> ?allow_inline:bool
    -> ?status:[`Valid | `Deprecated of deprecated]
    -> (config -> 'a -> config)
    -> (config -> 'a)
    -> 'a t

  val section_name : kind -> status -> string

  val deprecated : since_version:string -> string -> deprecated

  val removed : since_version:string -> string -> removed

  module Value : sig
    module Ok : sig
      type 'a t

      val valid : name:string -> doc:string -> 'a -> 'a t

      val deprecated :
        name:string -> doc:string -> deprecated:deprecated -> 'a -> 'a t
    end

    module Removed : sig
      (** Indicate that a configuration value has been removed in an
          ocamlformat release. A message indicating how to migrate will be
          displayed. *)
      type t

      val mk : name:string -> version:string -> msg:string -> t
      (** [name] is the configuration value that was removed in version
          [version]. [msg] explains how to get the former behaviour. *)

      val mk_list :
        names:string list -> version:string -> msg:string -> t list
      (** Shorthand for [mk] when [version] and [msg] are shared. This can be
          used when multiple values are removed at the same time. *)
    end
  end

  val choice :
       all:'a Value.Ok.t list
    -> ?removed_values:Value.Removed.t list
    -> 'a option_decl

  val flag : default:bool -> bool option_decl

  val any :
       ?default_doc:string
    -> 'a Cmdliner.Arg.conv
    -> default:'a
    -> docv:string
    -> 'a option_decl

  val removed_option :
    names:string list -> version:string -> msg:string -> unit
  (** Declare an option as removed. Using such an option will result in an
      helpful error including [msg] and [version]. *)

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
