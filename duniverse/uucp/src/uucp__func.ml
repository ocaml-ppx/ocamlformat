(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let is_dash u =
  Uucp_tmapbool.get Uucp_func_data.dash_map (Uchar.to_int u)

let is_diacritic u =
  Uucp_tmapbool.get Uucp_func_data.diacritic_map (Uchar.to_int u)

let is_extender u =
  Uucp_tmapbool.get Uucp_func_data.extender_map (Uchar.to_int u)

let is_grapheme_base u =
  Uucp_tmapbool.get Uucp_func_data.grapheme_base_map (Uchar.to_int u)

let is_grapheme_extend u =
  Uucp_tmapbool.get Uucp_func_data.grapheme_extend_map (Uchar.to_int u)

let is_math u =
  Uucp_tmapbool.get Uucp_func_data.math_map (Uchar.to_int u)

let is_quotation_mark u =
  Uucp_tmapbool.get Uucp_func_data.quotation_mark_map (Uchar.to_int u)

let is_soft_dotted u =
  Uucp_tmapbool.get Uucp_func_data.soft_dotted_map (Uchar.to_int u)

let is_terminal_punctuation u =
  Uucp_tmapbool.get Uucp_func_data.terminal_punctuation_map (Uchar.to_int u)

let is_regional_indicator u =
  Uucp_tmapbool.get Uucp_func_data.regional_indicator_map (Uchar.to_int u)

let is_join_control u =
  Uucp_tmapbool.get Uucp_func_data.join_control_map (Uchar.to_int u)
