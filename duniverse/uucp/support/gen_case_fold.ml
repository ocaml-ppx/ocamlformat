(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let pp_props ppf ucd =
  let map prop pname = Gen_case_map.pp_map_untagged_prop prop pname ppf ucd in
  map Uucd.case_folding "fold_map";
  ()

let pp_mod ppf ucd = Gen.pp_mod pp_props ppf ucd
