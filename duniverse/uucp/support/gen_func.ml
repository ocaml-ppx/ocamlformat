(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let pp_props ppf ucd =
  let prop = Gen.pp_prop_tmapbool_ucd ppf ucd in
  prop Uucd.dash "dash";
  prop Uucd.diacritic "diacritic";
  prop Uucd.extender "extender";
  prop Uucd.grapheme_base "grapheme_base";
  prop Uucd.grapheme_extend "grapheme_extend";
  prop Uucd.math "math";
  prop Uucd.quotation_mark "quotation_mark";
  prop Uucd.soft_dotted "soft_dotted";
  prop Uucd.terminal_punctuation "terminal_punctuation";
  prop Uucd.regional_indicator "regional_indicator";
  prop Uucd.join_control "join_control";
  ()

let pp_mod ppf ucd = Gen.pp_mod pp_props ppf ucd
