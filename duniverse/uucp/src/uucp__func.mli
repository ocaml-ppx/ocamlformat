(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Function and graphics properties. *)

(** {1:funcprops Function and graphics properties} *)

val is_dash : Uchar.t -> bool
(** [is_dash u] is [true] if [u] has the
    {{:http://www.unicode.org/reports/tr44/#Dash}Dash}
    property. *)

val is_diacritic : Uchar.t -> bool
(** [is_diacritic u] is [true] if [u] has the
    {{:http://www.unicode.org/reports/tr44/#Diacritic}Diacritic}
    property. *)

val is_extender : Uchar.t -> bool
(** [is_extender u] is [true] if [u] has the
    {{:http://www.unicode.org/reports/tr44/#Extender}Extender}
    property. *)

val is_grapheme_base : Uchar.t -> bool
(** [is_grapheme_base u] is [true] if [u] has the
    {{:http://www.unicode.org/reports/tr44/#Grapheme_Base}Grapheme_Base}
    property. *)

val is_grapheme_extend : Uchar.t -> bool
(** [is_grapheme_extend u] is [true] if [u] has the
    {{:http://www.unicode.org/reports/tr44/#Grapheme_Extend}Grapheme_Extend}
    property. *)

val is_math : Uchar.t -> bool
(** [is_math u] is [true] if [u] has the
    {{:http://www.unicode.org/reports/tr44/#Math}Math}
    property. *)

val is_quotation_mark : Uchar.t -> bool
(** [is_quotation_mark u] is [true] if [u] has the
    {{:http://www.unicode.org/reports/tr44/#Quotation_Mark}Quotation_Mark}
    property. *)

val is_soft_dotted : Uchar.t -> bool
(** [is_soft_dotted u] is [true] if [u] has the
    {{:http://www.unicode.org/reports/tr44/#Soft_Dotted}Soft_Dotted}
    property. *)

val is_terminal_punctuation : Uchar.t -> bool
(** [is_terminal_punctuation u] is [true] if [u] has the
    {{:http://www.unicode.org/reports/tr44/#Terminal_Punctuation}
    Terminal_Punctuation} property. *)

val is_regional_indicator : Uchar.t -> bool
(** [is_regional_indicator u] is [true] if [u] has the
    {{:http://www.unicode.org/reports/tr44/#Regional_Indicator}
    Regional_indicator} property. *)

val is_join_control : Uchar.t -> bool
(** [is_join_control u] is [true] if [u] has the
    {{:http://www.unicode.org/reports/tr44/#Join_Control}Join_Control}
    property. *)
