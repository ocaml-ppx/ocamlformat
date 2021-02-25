(** Findlib database *)

open! Stdune
open Dune_engine
open Dune_engine.Import

(** Findlib database *)
type t

val meta_fn : string

val create : paths:Path.t list -> lib_config:Lib_config.t -> t

(** The search path for this DB *)
val paths : t -> Path.t list

(** The builtins packages *)
val builtins : t -> Meta.Simplified.t Package.Name.Map.t

val findlib_predicates_set_by_dune : Variant.Set.t

module Unavailable_reason : sig
  type t =
    | Not_found
        (** The package is hidden because it contains an unsatisfied 'exist_if'
            clause *)
    | Invalid_dune_package of exn

  val to_dyn : t -> Dyn.t
end

(** Lookup a whole package, including sub-packages, in the given database.
    [root_name] must be a library name without dots. *)
val find_root_package :
  t -> Package.Name.t -> (Dune_package.t, Unavailable_reason.t) result

val find :
  t -> Lib_name.t -> (Dune_package.Entry.t, Unavailable_reason.t) result

val available : t -> Lib_name.t -> bool

(** List all the packages available in this Database *)
val all_packages : t -> Dune_package.Entry.t list

(** List all the packages that have broken [dune-package] files *)
val all_broken_packages : t -> (Package.Name.t * exn) list

(** A dummy package. This is used to implement [external-lib-deps] *)
val dummy_lib : t -> name:Lib_name.t -> Dune_package.Lib.t

module Config : sig
  type t

  val to_dyn : t -> Dyn.t

  val load : Path.t -> toolchain:string -> context:string -> t

  val get : t -> string -> string option

  val env : t -> Env.t
end
