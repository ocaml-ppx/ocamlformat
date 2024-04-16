(*---------------------------------------------------------------------------
   Copyright (c) 2013 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** NFKC case folding. *)

(** {1:nfkcfold NFKC Case folding} *)

val fold : Uchar.t -> [ `Self | `Uchars of Uchar.t list ]
(** [fold u] is [u]'s
    {{:http://www.unicode.org/reports/tr44/#NFKC_Casefold}NFKC_Casefold}
    property. *)
