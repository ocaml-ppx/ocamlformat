(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

include Uucp_block_base

let compare = compare
let blocks = Uucp_block_data.block_list
let block u = Uucp_rmap.get Uucp_block_data.block_map (Uchar.to_int u)
