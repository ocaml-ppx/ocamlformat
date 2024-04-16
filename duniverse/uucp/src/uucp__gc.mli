(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** General category property. *)

(** {1:gcprop General category property} *)

type t =
  [ `Cc | `Cf | `Cn | `Co | `Cs | `Ll | `Lm | `Lo | `Lt | `Lu | `Mc
  | `Me | `Mn | `Nd | `Nl | `No | `Pc | `Pd | `Pe | `Pf | `Pi | `Po
  | `Ps | `Sc | `Sk | `Sm | `So | `Zl | `Zp | `Zs ]
(** The type for general categories. *)

val compare : t -> t -> int
(** [compare c c'] is [Stdlib.compare s s']. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf c] prints an unspecified representation of [c] on [ppf]. *)

val general_category : Uchar.t -> t
(** [general_category u] is [u]'s
    {{:http://www.unicode.org/reports/tr44/#General_Category}
    General_Category} property. *)
