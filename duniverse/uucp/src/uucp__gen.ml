(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* General properties *)

let is_default_ignorable u =
  Uucp_tmapbool.get Uucp_gen_data.default_ignorable_map (Uchar.to_int u)

let is_deprecated u =
  Uucp_tmapbool.get Uucp_gen_data.deprecated_map (Uchar.to_int u)

let is_logical_order_exception u =
  Uucp_tmapbool.get Uucp_gen_data.logical_order_exception_map (Uchar.to_int u)

let is_non_character u =
  Uucp_tmapbool.get Uucp_gen_data.non_character_map (Uchar.to_int u)

let is_variation_selector u =
  Uucp_tmapbool.get Uucp_gen_data.variation_selector_map (Uchar.to_int u)
