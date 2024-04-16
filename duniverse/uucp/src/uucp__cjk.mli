(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** CJK properties.

    {b References.}
    {ul
    {- {{:http://www.unicode.org/faq/han_cjk.html}
        The Unicode Chinese and Japanese FAQ.}}
    {- {{:http://www.unicode.org/faq/korean.html}
        The Unicode Korean FAQ.}}} *)

(**  {1:cjkprops CJK properties} *)

val is_ideographic : Uchar.t -> bool
(** [is_ideographic u] is [true] if [u] has the
    {{:http://www.unicode.org/reports/tr44/#Ideographic}Ideographic}
    property. *)

val is_ids_unary_operator : Uchar.t -> bool
(** [is_ids_unary_operator u] is [true] if [u] has the
    {{:http://www.unicode.org/reports/tr44/#IDS_Unary_Operator}
    IDS_Binary_Operator} property. *)

val is_ids_binary_operator : Uchar.t -> bool
(** [is_ids_binary_operator u] is [true] if [u] has the
    {{:http://www.unicode.org/reports/tr44/#IDS_Binary_Operator}
    IDS_Binary_Operator} property. *)

val is_ids_trinary_operator : Uchar.t -> bool
(** [is_ids_trinary_operator u] is [true] if [u] has the
    {{:http://www.unicode.org/reports/tr44/#IDS_Trinary_Operator}
    IDS_Trinary_Operator} property. *)

val is_radical : Uchar.t -> bool
(** [is_radical u] is [true] if [u] has the
    {{:http://www.unicode.org/reports/tr44/#Radical}Radical}
    property. *)

val is_unified_ideograph : Uchar.t -> bool
(** [is_unified_ideograph u] is [true] if [u] has the
    {{:http://www.unicode.org/reports/tr44/#Unified_Ideograph}
    Unified_Ideograph} property. *)
