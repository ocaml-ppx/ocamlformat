(*---------------------------------------------------------------------------
   Copyright (c) 2020 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let is_emoji u =
  Uucp_tmapbool.get Uucp_emoji_data.emoji_map (Uchar.to_int u)

let is_emoji_presentation u =
  Uucp_tmapbool.get Uucp_emoji_data.emoji_presentation_map (Uchar.to_int u)

let is_emoji_modifier u =
  Uucp_tmapbool.get Uucp_emoji_data.emoji_modifier_map (Uchar.to_int u)

let is_emoji_modifier_base u =
  Uucp_tmapbool.get Uucp_emoji_data.emoji_modifier_base_map (Uchar.to_int u)

let is_emoji_component u =
  Uucp_tmapbool.get Uucp_emoji_data.emoji_component_map (Uchar.to_int u)

let is_extended_pictographic u =
  Uucp_tmapbool.get Uucp_emoji_data.extended_pictographic_map (Uchar.to_int u)
