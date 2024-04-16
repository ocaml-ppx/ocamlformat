(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = [ `Unassigned | `Version of int * int ]

let compare = compare
let pp ppf = function
| `Version (maj,min) -> Format.fprintf ppf "%d.%d" maj min
| `Unassigned -> Format.fprintf ppf "unassigned"

let age u = Uucp_rmap.get Uucp_age_data.age_map (Uchar.to_int u)
