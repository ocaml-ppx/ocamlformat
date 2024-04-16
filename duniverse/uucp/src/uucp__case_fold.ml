(*---------------------------------------------------------------------------
   Copyright (c) 2013 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let untagged_get m u = match Uucp_tmap.get m u with
| [] -> `Self | us -> `Uchars us

let fold u = untagged_get Uucp_case_fold_data.fold_map_map (Uchar.to_int u)
