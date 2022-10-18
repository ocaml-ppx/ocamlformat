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

(** This module contains ways to declare command-line and config-files
    options for ocamlformat. To look at declared options, see {!Conf.mli}. *)

module Error : sig
  type t =
    | Bad_value of string * string
    | Malformed of string
    | Misplaced of string * string
    | Unknown of string * [`Msg of string] option
    | Version_mismatch of {read: string; installed: string}

  val to_string : t -> string
end

type typ = Int | Bool | Range | Ocaml_version | Choice of string list

module UI : sig
  type 'config t =
    { names: string list
    ; values: typ
    ; doc: string
    ; update: 'config -> string -> Conf_t.updated_from -> 'config }
end

type 'a t

type kind = Formatting | Operational

type deprecated

type removed

type status = [`Valid | `Deprecated of deprecated | `Removed of removed]

type 'a option_decl =
     names:string list
  -> doc:string
  -> kind:kind
  -> ?allow_inline:bool
  -> ?status:[`Valid | `Deprecated of deprecated]
  -> (Conf_t.t -> 'a Conf_t.elt -> Conf_t.t)
  -> (Conf_t.t -> 'a Conf_t.elt)
  -> 'a t

val section_name : kind -> status -> string

val deprecated : since:Version.t -> string -> deprecated

val removed : since:Version.t -> string -> removed

module Value : sig
  (** Values of multiple-choice options *)

  type 'a t

  val make : ?deprecated:deprecated -> name:string -> 'a -> string -> 'a t
end

module Value_removed : sig
  (** Indicate that a configuration value has been removed in an ocamlformat
      release. A message indicating how to migrate will be displayed. *)
  type t

  val make : name:string -> since:Version.t -> msg:string -> t
  (** [name] is the configuration value that was removed in version [since].
      [msg] explains how to get the former behaviour. *)

  val make_list :
    names:string list -> since:Version.t -> msg:string -> t list
  (** Shorthand for [mk] when [since] and [msg] are shared. This can be used
      when multiple values are removed at the same time. *)
end

val choice :
     all:'a Value.t list
  -> ?removed_values:Value_removed.t list
  -> 'a option_decl

val flag : default:bool -> bool option_decl

val int : default:int -> docv:string -> int option_decl

val range :
     default:(string -> Range.t)
  -> docv:string
  -> (string -> Range.t) option_decl

val ocaml_version : default:Ocaml_version.t -> Ocaml_version.t option_decl

val any :
     'a Cmdliner.Arg.conv
  -> values:typ
  -> default:'a
  -> docv:string
  -> 'a option_decl

val removed_option :
  names:string list -> since:Version.t -> msg:string -> unit
(** Declare an option as removed. Using such an option will result in an
    helpful error including [msg] and [since]. *)

val default : 'a t -> 'a

val update_using_cmdline : Conf_t.t -> Conf_t.t

val update :
     config:Conf_t.t
  -> from:Conf_t.updated_from
  -> name:string
  -> value:string
  -> inline:bool
  -> (Conf_t.t, Error.t) Result.t

val to_ui : 'a t -> Conf_t.t UI.t

val print_config : Conf_t.t -> unit
