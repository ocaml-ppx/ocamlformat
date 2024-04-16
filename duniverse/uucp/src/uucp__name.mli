(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Name and name alias properties.

    {b References.}
    {ul
    {- {{:http://unicode.org/faq/casemap_charprop.html#nameprop}
       The Unicode names FAQ}.}
    {- The Unicode consortium.
       {{:https://unicode.org/charts/nameslist/index.html}
       The Unicode names charts}}} *)

(** {1:nameprop Names} *)

val name : Uchar.t -> string
(** [name u] is [u]'s
    {{:http://www.unicode.org/reports/tr44/#Name}Name} property. *)

(** {1:namealiasprop Name aliases} *)

type alias_tag =
  [ `Abbreviation | `Alternate | `Control | `Correction | `Figment ]

val pp_alias_tag : Format.formatter -> alias_tag -> unit
(** [pp_alias_tag t] prints an unspecified representation of [t]
    on [ppf]. *)

val name_alias : Uchar.t -> (alias_tag * string) list
(** [name_alias u] is [u]'s
    {{:http://www.unicode.org/reports/tr44/#Name_Alias}Name_Alias}
    property. *)
