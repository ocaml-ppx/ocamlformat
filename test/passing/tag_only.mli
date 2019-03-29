(** @deprecated  *)
open Module

(** abc
    @deprecated  *)
open Module

(** @inline *)
include sig
  type t
end

(** @inline *)
include Type

(** @inline *)
include module type of Module

(** @deprecated  *)
module A : B

(** @deprecated  *)
module A : sig
  type t
end

(** @open *)
module type A = B

(** @open *)
module type A = sig
  type t
end

(** @deprecated  *)
type t = T

type t = t
(** @deprecated  *)

val a : b
(** @deprecated  *)

[@@@ocamlformat "doc-comments-tag-only=fit"]

open Module (** @deprecated  *)

(** abc
    @deprecated  *)
open Module

(** @inline *)
include sig
  type t
end

include Type (** @inline *)

include module type of Module (** @inline *)

module A : B (** @deprecated  *)

(** @deprecated  *)
module A : sig
  type t
end

module type A = B (** @open *)

(** @open *)
module type A = sig
  type t
end

(** @deprecated  *)
type t = T

type t = t
(** @deprecated  *)

val a : b
(** @deprecated  *)
