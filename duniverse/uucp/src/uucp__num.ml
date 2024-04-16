(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

include Uucp_num_base

let is_ascii_hex_digit u =
  Uucp_tmapbool.get Uucp_num_data.ascii_hex_digit_map (Uchar.to_int u)

let is_hex_digit u =
  Uucp_tmapbool.get Uucp_num_data.hex_digit_map (Uchar.to_int u)

let numeric_type u =
  Uucp_rmap.get Uucp_num_data.numeric_type_map (Uchar.to_int u)

let numeric_value u =
  Uucp_cmap.get Uucp_num_data.numeric_value_map (Uchar.to_int u)
