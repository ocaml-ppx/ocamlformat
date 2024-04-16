(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let is_white_space u =
  Uucp_tmapbool.get Uucp_white_data.white_space_map (Uchar.to_int u)
