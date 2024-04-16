(*---------------------------------------------------------------------------
   Copyright (c) 2013 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Case folding. *)

(** {1:casefolding Case folding} *)

val fold : Uchar.t -> [ `Self | `Uchars of Uchar.t list ]
(** [fold u] is [u]'s
    {{:http://www.unicode.org/reports/tr44/#Case_Folding}Case_Folding}
    property. *)
