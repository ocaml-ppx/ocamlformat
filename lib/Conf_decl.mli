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
    options for ocamlformat. To look at declared options, see {!Conf.mli}. To
    declare a new option :

    - Add a field to one of the records in [Conf_t]
    - Declare the option in [Conf], using functions from this module. *)

type typ = Int | Bool | Ocaml_version | Choice of string list

module UI : sig
  type 'config t =
    { names: string list
    ; values: typ
    ; doc: string
    ; update: 'config -> string -> Conf_t.updated_from -> 'config }
end

(** The type of an option declaration *)
type 'a t

type kind = Formatting | Operational

type deprecated

type removed

type status = [`Valid | `Deprecated of deprecated | `Removed of removed]

(** The type of option declarators, that is functions returning a declaration *)
type 'a declarator =
     names:string list
  -> default:Conf_t.t
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

module Store : sig
  type elt

  type store = elt list

  val add : store -> 'a t -> store

  val elt : 'a t -> elt

  type t = store

  val empty : t

  val merge : t -> t -> t

  val to_ui : t -> Conf_t.t UI.t list

  val to_term : t -> (Conf_t.t -> Conf_t.t) Cmdliner.Term.t
end

val choice :
     all:'a Value.t list
  -> ?removed_values:Value_removed.t list
  -> 'a declarator

val flag : bool declarator

val int : docv:string -> int declarator

val ocaml_version : Ocaml_version.t declarator

val any : 'a Cmdliner.Arg.conv -> values:typ -> docv:string -> 'a declarator

val removed_option :
  names:string list -> since:Version.t -> msg:string -> unit t
(** Declare an option as removed. Using such an option will result in an
    helpful error including [msg] and [since]. *)

val default : 'a t -> 'a

val update :
     Store.t
  -> config:Conf_t.t
  -> from:Conf_t.updated_from
  -> name:string
  -> value:string
  -> inline:bool
  -> (Conf_t.t, Conf_t.Error.t) Result.t

val to_ui : 'a t -> Conf_t.t UI.t

val print_config : Store.t -> Conf_t.t -> unit
