(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type numeric_type = [ `De | `Di | `None | `Nu ]
type numeric_value =
  [ `NaN | `Nums of [`Frac of int * int | `Num of int64 ] list ]

let pp_numeric_type ppf v = Format.fprintf ppf "%s" begin match v with
  | `De -> "De"
  | `Di -> "Di"
  | `None -> "None"
  | `Nu -> "Nu"
  end

let pp_num ppf = function
| `Frac (a, b) -> Format.fprintf ppf "Frac(%d,%d)" a b
| `Num n -> Format.fprintf ppf "Num(%LdL)" n

let pp_numeric_value ppf = function
| `NaN -> Format.fprintf ppf "NaN"
| `Nums nums ->
    let pp_sep ppf () = Format.fprintf ppf " " in
    Format.fprintf ppf "@[%a@]" (Format.pp_print_list ~pp_sep pp_num) nums
