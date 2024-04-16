(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let pp_props ppf ucd =
  Gen.pp_prop_tmapbool_ucd ppf ucd Uucd.alphabetic "alphabetic";
  ()

let pp_mod ppf ucd = Gen.pp_mod pp_props ppf ucd
