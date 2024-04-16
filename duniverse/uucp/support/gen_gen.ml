(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let pp_props ppf ucd =
  let prop = Gen.pp_prop_tmapbool_ucd ppf ucd in
  prop Uucd.default_ignorable_code_point "default_ignorable";
  prop Uucd.deprecated "deprecated";
  prop Uucd.logical_order_exception "logical_order_exception";
  prop Uucd.noncharacter_code_point "non_character";
  prop Uucd.variation_selector "variation_selector";
  ()

let pp_mod ppf ucd = Gen.pp_mod pp_props ppf ucd
