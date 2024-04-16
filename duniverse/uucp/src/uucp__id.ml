(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let is_id_start u =
  Uucp_tmapbool.get Uucp_id_data.id_start_map (Uchar.to_int u)

let is_id_continue u =
  Uucp_tmapbool.get Uucp_id_data.id_continue_map (Uchar.to_int u)

let is_xid_start u =
  Uucp_tmapbool.get Uucp_id_data.xid_start_map (Uchar.to_int u)

let is_xid_continue u =
  Uucp_tmapbool.get Uucp_id_data.xid_continue_map (Uchar.to_int u)

let is_id_compat_math_start u =
  Uucp_tmapbool.get Uucp_id_data.id_compat_math_start_map (Uchar.to_int u)

let is_id_compat_math_continue u =
  Uucp_tmapbool.get Uucp_id_data.id_compat_math_continue_map (Uchar.to_int u)

let is_pattern_syntax u =
  Uucp_tmapbool.get Uucp_id_data.pattern_syntax_map (Uchar.to_int u)

let is_pattern_white_space u =
  Uucp_tmapbool.get Uucp_id_data.pattern_white_space_map (Uchar.to_int u)
