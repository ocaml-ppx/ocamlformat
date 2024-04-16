(*---------------------------------------------------------------------------
   Copyright (c) 2013 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let fold u =
  Uucp_tmap.get
    Uucp_case_nfkc_simple_data.nfkc_simple_fold_map_map (Uchar.to_int u)
