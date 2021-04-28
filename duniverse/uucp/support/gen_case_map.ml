(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
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

(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
