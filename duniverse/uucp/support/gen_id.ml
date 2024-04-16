(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let pp_props ppf ucd =
  let prop = Gen.pp_prop_tmapbool_ucd ppf ucd in
  prop Uucd.id_start "id_start";
  prop Uucd.id_continue "id_continue";
  prop Uucd.xid_start "xid_start";
  prop Uucd.xid_continue "xid_continue";
  prop Uucd.id_compat_math_start "id_compat_math_start";
  prop Uucd.id_compat_math_continue "id_compat_math_continue";
  prop Uucd.pattern_syntax "pattern_syntax";
  prop Uucd.pattern_white_space "pattern_white_space";
  ()

let pp_mod ppf ucd = Gen.pp_mod pp_props ppf ucd
