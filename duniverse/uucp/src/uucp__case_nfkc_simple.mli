(*---------------------------------------------------------------------------
   Copyright (c) 2013 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** NFKC simple case folding. *)

(** {1:nfkcfold NFKC simple case folding} *)

val fold : Uchar.t -> [ `Self | `Uchars of Uchar.t list ]
(** [fold u] is [u]'s
    {{:http://www.unicode.org/reports/tr44/#NFKC_Simple_Casefold}
    NFKC_Simple_Casefold}property. *)
