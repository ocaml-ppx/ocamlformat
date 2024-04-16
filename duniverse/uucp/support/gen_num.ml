(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let pp_numeric_type ppf ucd =
  let size v = 0 in
  let pp_ntype ppf t = Gen.pp ppf "`%a" Uucp_num_base.pp_numeric_type t in
  Gen.pp_prop_rmap_ucd ~share:false ppf ucd Uucd.numeric_type
    "numeric_type" "Uucp_num_base.numeric_type" pp_ntype
    ~default:`None size

let pp_numeric_value ppf ucd =
  let num_size = function
  | `Frac _ -> 1 + 2 + 2 (* cons *)
  | `Num _ -> 1 + (64 / Sys.word_size) + 2 (* cons *)
  in
  let size = function
  | `NaN -> 0
  | `Nums nums -> List.fold_left (fun acc num -> acc + num_size num) 0 nums
  in
  let pp_numeric_value ppf = function
  | `NaN -> Format.fprintf ppf "`NaN"
  | `Nums nums ->
      let pp_num ppf = function
      | `Frac (a, b) -> Format.fprintf ppf "`Frac(%d,%d)" a b
      | `Num n -> Format.fprintf ppf "`Num(%LdL)" n
      in
      let pp_sep ppf () = Format.fprintf ppf ";@," in
      Format.fprintf ppf "`Nums[%a]" (Format.pp_print_list ~pp_sep pp_num) nums
  in
  let pp_nvalue ppf v = Gen.pp ppf "(%a)" pp_numeric_value v in
  Gen.pp_prop_cmap_ucd ppf ucd Uucd.numeric_value
    "numeric_value" "Uucp_num_base.numeric_value" pp_nvalue
    ~default:`NaN size

let pp_props ppf ucd =
  let prop = Gen.pp_prop_tmapbool_ucd ppf ucd in
  prop Uucd.ascii_hex_digit "ascii_hex_digit";
  prop Uucd.hex_digit "hex_digit";
  pp_numeric_type ppf ucd;
  pp_numeric_value ppf ucd;
  ()

let pp_mod ppf ucd = Gen.pp_mod pp_props ppf ucd
