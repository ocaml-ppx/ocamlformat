(*---------------------------------------------------------------------------
   Copyright (c) 2020 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Emoji properties. *)

val is_emoji : Uchar.t -> bool
(** [is_emoji u] is [true] if [u] has the
    {{:http://www.unicode.org/reports/tr44/#Emoji}Emoji} property. *)

val is_emoji_presentation : Uchar.t -> bool
(** [is_emoji_presentation u] is [true] if [u] has the
    {{:http://www.unicode.org/reports/tr44/#Emoji_Presentation}
    Emoji_Presentation} property. *)

val is_emoji_modifier : Uchar.t -> bool
(** [is_emoji_modifier u] is [true] if [u] has the
    {{:http://www.unicode.org/reports/tr44/#Emoji_Modifier}
    Emoji_Modifier} property. *)

val is_emoji_modifier_base : Uchar.t -> bool
(** [is_emoji_modifier u] is [true] if [u] has the
    {{:http://www.unicode.org/reports/tr44/#Emoji_Modifier_Base}
    Emoji_Modifier_Base} property. *)

val is_emoji_component : Uchar.t -> bool
(** [is_emoji_component u] is [true] if [u] has the
    {{:http://www.unicode.org/reports/tr44/#Emoji_Component}
    Emoji_Component} property. *)

val is_extended_pictographic : Uchar.t -> bool
(** [is_extended_pictographic u] is [true] if [u] has the
    {{:http://www.unicode.org/reports/tr44/#Extended_Pictographic}
    Extended_Pictographic} property. *)
