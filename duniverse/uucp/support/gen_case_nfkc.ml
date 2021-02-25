(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

let pp_map_prop prop pname ppf ucd =
  let pp_map_value ppf = function
  | `Self -> Format.fprintf ppf "`Self"
  | `Uchars us -> Format.fprintf ppf "@[<1>(`Uchars@ %a)@]"
                    (Gen.pp_list Gen.pp_uchar) us
  in
  let size = function
  | `Self -> 0
  | `Uchars us -> 2 + 3 * (List.length us)
  in
  let prop cp = match Gen.ucd_get ucd cp prop with
  | `Self -> `Self
  | `Cps us -> `Uchars us
  in
  Gen.pp_prop_tmap ppf prop pname "[ `Self | `Uchars of Uchar.t list ]"
    pp_map_value ~default:`Self size

let pp_props ppf ucd =
  pp_map_prop Uucd.nfkc_casefold "nfkc_fold_map" ppf ucd;
  let prop = Gen.pp_prop_tmapbool_ucd ppf ucd in
  prop Uucd.changes_when_nfkc_casefolded "changes_when_casefolded";
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
