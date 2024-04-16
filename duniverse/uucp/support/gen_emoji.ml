(*---------------------------------------------------------------------------
   Copyright (c) 2020 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let pp_props ppf ucd =
  let prop = Gen.pp_prop_tmapbool_ucd ppf ucd in
  prop Uucd.emoji "emoji";
  prop Uucd.emoji_presentation "emoji_presentation";
  prop Uucd.emoji_modifier "emoji_modifier";
  prop Uucd.emoji_modifier_base "emoji_modifier_base";
  prop Uucd.emoji_component "emoji_component";
  prop Uucd.extended_pictographic "extended_pictographic";
  ()

let pp_mod ppf ucd = Gen.pp_mod pp_props ppf ucd
