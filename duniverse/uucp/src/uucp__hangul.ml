(*---------------------------------------------------------------------------
   Copyright (c) 2018 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

include Uucp_hangul_base

let syllable_type u =
  Uucp_rmap.get Uucp_hangul_data.syllable_type_map (Uchar.to_int u)
