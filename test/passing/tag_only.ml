(** @deprecated  *)
open Module

(** abc
    @deprecated  *)
open Module

(** @author A *)
open Module

(** @inline *)
open Module

(** @inline *)
include Abc

(** @inline *)
include struct
  type t
end

(** @inline *)
include (Module : Type)

(** @inline *)
module A = B

(** @inline *)
module A : sig
  type t
end = struct
  type t
end

[@@@ocamlformat "doc-tagonly-fit"]

open Module (** @deprecated  *)

(** abc
    @deprecated  *)
open Module

open Module (** @author A *)

open Module (** @inline *)

include Abc (** @inline *)

include struct
  type t
end (** @inline *)

include (Module : Type) (** @inline *)

module A = B (** @inline *)

module A : sig
  type t
end = struct
  type t
end (** @inline *)
