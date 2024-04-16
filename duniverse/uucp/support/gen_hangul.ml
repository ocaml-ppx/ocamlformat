(*---------------------------------------------------------------------------
   Copyright (c) 2018 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let pp_syllable_type ppf ucd =
  let size v = 0 in
  let pp_stype ppf t = Gen.pp ppf "`%a" Uucp_hangul_base.pp_syllable_type t in
  Gen.pp_prop_rmap_ucd ~share:false ppf ucd Uucd.hangul_syllable_type
    "syllable_type" "Uucp_hangul_base.syllable_type" pp_stype
    ~default:`NA size

let pp_props ppf ucd =
  pp_syllable_type ppf ucd;
  ()

let pp_mod ppf ucd = Gen.pp_mod pp_props ppf ucd
