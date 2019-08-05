open Module
(** @deprecated *)

open Module
(** abc

    @deprecated *)

(** @inline *)
include sig
  type t
end

include Type
(** @inline *)

include module type of Module
(** @inline *)

module A : B
(** @deprecated *)

(** @deprecated *)
module A : sig
  type t
end

module type A = B
(** @open *)

(** @open *)
module type A = sig
  type t
end

(** @deprecated *)
type t = T

type t = {a: int}
(** @deprecated *)

type t = ..
(** @deprecated *)

type t
(** @deprecated *)

type t = t
(** @deprecated *)

val a : b
(** @deprecated *)

[@@@ocamlformat "doc-comments-tag-only=fit"]

open Module  (** @deprecated *)

open Module
(** abc

    @deprecated *)

(** @inline *)
include sig
  type t
end

include Type  (** @inline *)

include module type of Module  (** @inline *)

module A : B  (** @deprecated *)

(** @deprecated *)
module A : sig
  type t
end

module type A = B  (** @open *)

(** @open *)
module type A = sig
  type t
end

(** @deprecated *)
type t = T

type t = {a: int}
(** @deprecated *)

type t = ..  (** @deprecated *)

type t  (** @deprecated *)

type t = t  (** @deprecated *)

val a : b
(** @deprecated *)
