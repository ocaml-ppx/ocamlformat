(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

include Uucp_gc_base

let compare = compare
let general_category u =
  Uucp_rmap.get Uucp_gc_data.general_category_map (Uchar.to_int u)
