(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let is_alphabetic u =
  Uucp_tmapbool.get Uucp_alpha_data.alphabetic_map (Uchar.to_int u)
