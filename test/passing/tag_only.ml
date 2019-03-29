open Module
(** @deprecated  *)

open Module
(** abc
    @deprecated  *)

open Module
(** @author A *)

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
(** @inline *)

and B : sig
  type t
end = struct
  type t
end
(** @author B *)

module type A = B
(** @deprecated abc *)

module type A = sig
  type t
end
(** @deprecated abc *)

module A : sig
  type t
end =
  B
(** @open *)

open Module.With_veryyyyyy_loooooooooooooooooooooooong_naaaaaaaaaaaaaaaaame
(** @deprecated  *)

include Module.With_very_loooooooooooooooooooooooong_naaaaaaaaaaaaaaaaame
(** @deprecated  *)

module A = Module.With_very_loooooooooooooooooooooooong_naaaaaaaaaaaaaaaaame
(** @deprecated  *)

(** @deprecated  *)
type t = T

type t = t
(** @deprecated  *)

(** @deprecated  *)
let a = b

[@@@ocamlformat "doc-comments-tag-only=fit"]

open Module
(** @deprecated  *)

open Module
(** abc
    @deprecated  *)

open Module
(** @author A *)

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
(** @inline *)

and B : sig
  type t
end = struct
  type t
end
(** @author B *)

module type A = B
(** @deprecated abc *)

module type A = sig
  type t
end
(** @deprecated abc *)

module A : sig
  type t
end =
  B
(** @open *)

open Module.With_veryyyyyy_loooooooooooooooooooooooong_naaaaaaaaaaaaaaaaame
(** @deprecated  *)

include Module.With_very_loooooooooooooooooooooooong_naaaaaaaaaaaaaaaaame
(** @deprecated  *)

module A = Module.With_very_loooooooooooooooooooooooong_naaaaaaaaaaaaaaaaame
(** @deprecated  *)

(** @deprecated  *)
type t = T

type t = t
(** @deprecated  *)

(** @deprecated  *)
let a = b
