(*---------------------------------------------------------------------------
   Copyright (c) 2018 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type syllable_type = [ `L | `V | `T | `LV | `LVT | `NA ]

let pp_syllable_type ppf v = Format.fprintf ppf "%s" begin match v with
  | `NA -> "NA"
  | `L -> "L"
  | `V -> "V"
  | `T -> "T"
  | `LV -> "LV"
  | `LVT -> "LVT"
  end
