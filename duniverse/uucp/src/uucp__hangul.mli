(*---------------------------------------------------------------------------
   Copyright (c) 2018 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Hangul properties. *)

(** {1:hangul_syllable_type Hangul syllable type property} *)

type syllable_type = [ `L | `V | `T | `LV | `LVT | `NA ]
(** The type for hangul syllable types. *)

val pp_syllable_type : Format.formatter -> syllable_type -> unit
(** [pp_syllable_type ppf s] prints an unspecified representation of
    [s] on [ppf]. *)

val syllable_type : Uchar.t -> syllable_type
(** [syllable_type u] is [u]'s
    {{:http://www.unicode.org/reports/tr44/#Hangul_Syllable_Type}
    Hangul_Syllable_type} property. *)
