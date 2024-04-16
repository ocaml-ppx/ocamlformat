(*---------------------------------------------------------------------------
   Copyright (c) 2013 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Case mappings. *)

(** {1:casemaps Case mappings} *)

val to_lower : Uchar.t -> [ `Self | `Uchars of Uchar.t list ]
(** [to_lower u] is [u]'s
    {{:http://www.unicode.org/reports/tr44/#Lowercase_Mapping}
    Lowercase_Mapping} property. *)

val to_upper : Uchar.t -> [ `Self | `Uchars of Uchar.t list ]
(** [to_upper u] is [u]'s
    {{:http://www.unicode.org/reports/tr44/#Uppercase_Mapping}
    Uppercase_Mapping} property. *)

val to_title : Uchar.t -> [ `Self | `Uchars of Uchar.t list ]
(** [to_title u] is [u]'s
    {{:http://www.unicode.org/reports/tr44/#Titlecase_Mapping}
    Titlecase_Mapping} property. *)
