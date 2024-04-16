(*---------------------------------------------------------------------------
   Copyright (c) 2013 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let untagged_get m u = match Uucp_tmap.get m u with
| [] -> `Self | us -> `Uchars us

let to_upper u = untagged_get Uucp_case_map_data.upper_map_map (Uchar.to_int u)
let to_lower u = untagged_get Uucp_case_map_data.lower_map_map (Uchar.to_int u)
let to_title u = untagged_get Uucp_case_map_data.title_map_map (Uchar.to_int u)
