(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Alphabetic property. *)

(** {1:alphaprop Alphabetic property} *)

val is_alphabetic : Uchar.t -> bool
(** [is_alphabetic u] is [true] if [u] has the
    {{:http://www.unicode.org/reports/tr44/#Alphabetic}Alphabetic}
    property. *)
