(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let pp_script_variant ppf s = Gen.pp ppf "`%a" Uucp_script_base.pp s

let pp_script_prop ppf ucd =
  let size v = 0 in
  Gen.pp_prop_rmap_ucd ~share:false ppf ucd
    Uucd.script "script" "Uucp_script_base.t" pp_script_variant
    ~default:`Zzzz size

let pp_script_extensions_prop ppf ucd =
  let size v = 3 * (List.length v) in
  let pp_script_list = Gen.pp_list pp_script_variant in
  let prop u =
    let script = Gen.ucd_get ucd u Uucd.script in
    let es = Gen.ucd_get ucd u Uucd.script_extensions in
    match es with
    | [] -> assert false
    | [script'] when script = script' -> []
    | es -> es
  in
  Gen.pp_prop_rmap ppf
    prop "script_extensions" "Uucp_script_base.t list" pp_script_list
    ~default:[] size

let pp_props ppf ucd =
  pp_script_prop ppf ucd;
  pp_script_extensions_prop ppf ucd;
  ()

let pp_mod ppf ucd = Gen.pp_mod pp_props ppf ucd
