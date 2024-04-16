(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Age property. *)

(** {1:ageprop Age property} *)

type t = [ `Unassigned | `Version of int * int ]
(** The type for character age. *)

val compare : t -> t -> int
(** [compare a a'] is [Stdlib.compare a a'] *)

val pp : Format.formatter -> t -> unit
(** [pp ppf a] prints an unspecified representation of [a] on [ppf]. *)

val age : Uchar.t -> t
(** [age u] is [u]'s
      {{:http://www.unicode.org/reports/tr44/#Age}Age} property. *)
