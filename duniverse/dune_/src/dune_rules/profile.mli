(** Defines build profile for dune. Only one profile is active per context. Some
    profiles are treated specially by dune. *)
open! Import

type t =
  | Dev
  | Release
  | User_defined of string

val equal : t -> t -> bool

val is_dev : t -> bool

val is_release : t -> bool

val is_inline_test : t -> bool

include Stringlike_intf.S with type t := t

val default : t
