(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** White space property. *)

(** {1:whiteprop White space property} *)

val is_white_space : Uchar.t -> bool
(** [is_white_space u] is [true] if [u] has the
    {{:http://www.unicode.org/reports/tr44/#White_Space}White_Space}
    property. *)
