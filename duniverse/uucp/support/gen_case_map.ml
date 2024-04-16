(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let pp_map_untagged_prop prop pname ppf ucd =
  let size us = 3 * (List.length us) in
  let pp_uchars = Gen.pp_list Gen.pp_uchar in
  let prop cp = match Gen.ucd_get ucd cp prop with
  | `Self -> []
  | `Cps [] -> assert false
  | `Cps us -> us
  in
  Gen.pp_prop_tmap ppf prop pname "Uchar.t list" pp_uchars ~default:[] size

let pp_props ppf ucd =
  let map prop pname = pp_map_untagged_prop prop pname ppf ucd in
  map Uucd.uppercase_mapping "upper_map";
  map Uucd.lowercase_mapping "lower_map";
  map Uucd.titlecase_mapping "title_map";
  ()

let pp_mod ppf ucd = Gen.pp_mod pp_props ppf ucd
