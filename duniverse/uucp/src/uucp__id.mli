(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Identifier properties.

    {b References.}
    {ul
    {- Mark Davis.
    {e {{:http://www.unicode.org/reports/tr31/}UAX #31
       Unicode Identifier and Pattern Syntax}}. (latest version)}} *)

(** {1:idprops Identifier properties} *)

val is_id_start : Uchar.t -> bool
(** [is_id_start u] is [true] if [u] has the
    {{:http://www.unicode.org/reports/tr44/#ID_Start}ID_Start}
    property. *)

val is_id_continue : Uchar.t -> bool
(** [is_id_continue u] is [true] if [u] has the
    {{:http://www.unicode.org/reports/tr44/#ID_Continue}ID_Continue}
    property. *)

val is_xid_start : Uchar.t -> bool
(** [is_xid_start u] is [true] if [u] has the
    {{:http://www.unicode.org/reports/tr44/#XID_Start}XID_Start}
    property. *)

val is_xid_continue : Uchar.t -> bool
(** [is_xid_continue u] is [true] if [u] has the
    {{:http://www.unicode.org/reports/tr44/#XID_Continue}XID_Continue}
    property. *)

(** {1:mathprops Mathematical compatibility notation profile} *)

val is_id_compat_math_start : Uchar.t -> bool
(** [is_id_compat_math_start u] is [true] if [u] has then
    {{:https://www.unicode.org/reports/tr44/#ID_Compat_Math_Start}
    ID_Compat_Math_Start} property. *)

val is_id_compat_math_continue : Uchar.t -> bool
(** [is_id_compat_math_continue u] is [true] if [u] has then
    {{:https://www.unicode.org/reports/tr44/#ID_Compat_Math_Continue}
    ID_Compat_Math_Continue} property. *)

(** {1:patprops Pattern syntax properties} *)

val is_pattern_syntax : Uchar.t -> bool
(** [is_pattern_syntax u] is [true] if [u] has the
    {{:http://www.unicode.org/reports/tr44/#Pattern_Syntax}Pattern_Syntax}
    property. *)

val is_pattern_white_space : Uchar.t -> bool
(** [is_pattern_white_space u] is [true] if [u] has the
    {{:http://www.unicode.org/reports/tr44/#Pattern_White_Space}
    Pattern_White_Space} property. *)
