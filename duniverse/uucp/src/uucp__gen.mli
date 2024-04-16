(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** General properties. *)

(** {1:genprops General properties} *)

val is_default_ignorable : Uchar.t -> bool
(** [is_default_ignorable u] is [true] if [u] has the
    {{:http://www.unicode.org/reports/tr44/#Default_Ignorable_Code_Point}
    Default_Ignorable_Code_Point} property. *)

val is_deprecated : Uchar.t -> bool
(** [is_deprecated u] is [true] if [u] has the
    {{:http://www.unicode.org/reports/tr44/#Deprecated}
    Deprecated} property. *)

val is_logical_order_exception : Uchar.t -> bool
(** [is_logical_order_exception u] is [true] if [u] has the
    {{:http://www.unicode.org/reports/tr44/#Logical_Order_Exception}
    Logical_Order_Exception} property. *)

val is_non_character : Uchar.t -> bool
(** [is_non_character u] is [true] if [u] has the
    {{:http://www.unicode.org/reports/tr44/#Noncharacter_Code_Point}
    Noncharacter_Code_Point} property. *)

val is_variation_selector : Uchar.t -> bool
(** [is_variation_selector u] is [true] if [u] has the
    {{:http://www.unicode.org/reports/tr44/#Variation_Selector}
    Variation_Selector} property. See the
    {{:http://www.unicode.org/faq/vs.html}Variation Sequences FAQ}. *)
