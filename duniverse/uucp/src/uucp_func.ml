(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
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

(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
