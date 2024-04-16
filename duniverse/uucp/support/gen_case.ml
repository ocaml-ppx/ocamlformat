(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let pp_props ppf ucd =
  let prop = Gen.pp_prop_tmapbool_ucd ppf ucd in
  prop Uucd.uppercase "upper";
  prop Uucd.lowercase "lower";
  prop Uucd.cased "cased";
  prop Uucd.case_ignorable "case_ignorable";
  ()

let pp_mod ppf ucd = Gen.pp_mod pp_props ppf ucd
