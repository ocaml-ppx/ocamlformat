(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let is_ideographic u =
  Uucp_tmapbool.get Uucp_cjk_data.ideographic_map (Uchar.to_int u)

let is_ids_unary_operator u =
  Uucp_tmapbool.get Uucp_cjk_data.ids_unary_operator_map (Uchar.to_int u)

let is_ids_binary_operator u =
  Uucp_tmapbool.get Uucp_cjk_data.ids_binary_operator_map (Uchar.to_int u)

let is_ids_trinary_operator u =
  Uucp_tmapbool.get Uucp_cjk_data.ids_trinary_operator_map (Uchar.to_int u)

let is_radical u =
  Uucp_tmapbool.get Uucp_cjk_data.radical_map (Uchar.to_int u)

let is_unified_ideograph u =
  Uucp_tmapbool.get Uucp_cjk_data.unified_ideograph_map (Uchar.to_int u)
