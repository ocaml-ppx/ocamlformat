(** @deprecated *)
open Module

(** abc

    @deprecated *)
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

(** @inline *)
module rec A : sig
  type t
end = struct
  type t
end

(** @author B *)
and B : sig
  type t
end = struct
  type t
end

(** @deprecated abc *)
module type A = B

(** @deprecated abc *)
module type A = sig
  type t
end

(** @open *)
module A : sig
  type t
end =
  B

(** @deprecated *)
open Module.With_veryyyyyy_loooooooooooooooooooooooong_naaaaaaaaaaaaaaaaame

(** @deprecated *)
include Module.With_very_loooooooooooooooooooooooong_naaaaaaaaaaaaaaaaame

(** @deprecated *)
module A = Module.With_very_loooooooooooooooooooooooong_naaaaaaaaaaaaaaaaame

(** @deprecated *)
type t = T

(** @deprecated *)
type t = t

(** @deprecated *)
let a = b

(** @deprecated *)
type t = t
(** @deprecated *)

class b =
  object
    (** @deprecated *)
    method f = 0

    (** @deprecated *)
    inherit a

    (** @deprecated *)
    val x = 1

    (** @deprecated *)
    constraint 'a = [> ]

    (** @deprecated *)
    initializer do_init ()
  end

[@@@ocamlformat "doc-comments-tag-only=fit"]

open Module  (** @deprecated *)

(** abc

    @deprecated *)
open Module

open Module  (** @author A *)

open Module  (** @inline *)

include Abc  (** @inline *)

(** @inline *)
include struct
  type t
end

include (Module : Type)  (** @inline *)

module A = B  (** @inline *)

(** @inline *)
module A : sig
  type t
end = struct
  type t
end

(** @inline *)
module rec A : sig
  type t
end = struct
  type t
end

(** @author B *)
and B : sig
  type t
end = struct
  type t
end

module type A = B  (** @deprecated abc *)

(** @deprecated abc *)
module type A = sig
  type t
end

(** @open *)
module A : sig
  type t
end =
  B

open Module.With_veryyyyyy_loooooooooooooooooooooooong_naaaaaaaaaaaaaaaaame
(** @deprecated *)

include Module.With_very_loooooooooooooooooooooooong_naaaaaaaaaaaaaaaaame
(** @deprecated *)

module A = Module.With_very_loooooooooooooooooooooooong_naaaaaaaaaaaaaaaaame
(** @deprecated *)

(** @deprecated *)
type t = T

type t = t  (** @deprecated *)

(** @deprecated *)
let a = b

(** @deprecated *)
type t = t
(** @deprecated *)

class b =
  object
    method f = 0  (** @deprecated *)

    inherit a  (** @deprecated *)

    val x = 1  (** @deprecated *)

    constraint 'a = [> ]  (** @deprecated *)

    initializer do_init ()  (** @deprecated *)
  end
