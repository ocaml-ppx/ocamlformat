(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

include Uucp_script_base

let compare = compare
let script u = Uucp_rmap.get Uucp_script_data.script_map (Uchar.to_int u)
let script_extensions u =
  match Uucp_rmap.get Uucp_script_data.script_extensions_map (Uchar.to_int u)
  with
  | [] -> [ script u ]
  | scripts -> scripts
