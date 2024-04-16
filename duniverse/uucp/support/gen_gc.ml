(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let pp_general_category ppf ucd =
  let size v = 0 in
  let pp_gc ppf v = Gen.pp ppf "`%a" Uucp_gc_base.pp v in
  Gen.pp_prop_rmap_ucd ~share:false ppf ucd
    Uucd.general_category "general_category" "Uucp_gc_base.t"
    pp_gc ~default:`Cn size

let pp_props ppf ucd =
  pp_general_category ppf ucd;
  ()

let pp_mod ppf ucd = Gen.pp_mod pp_props ppf ucd
