(*---------------------------------------------------------------------------
   Copyright (c) 2023 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
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
  pp_map_prop Uucd.nfkc_simple_casefold "nfkc_simple_fold_map" ppf ucd;
  ()

let pp_mod ppf ucd = Gen.pp_mod pp_props ppf ucd
