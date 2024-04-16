(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)


type alias_tag =
  [ `Abbreviation | `Alternate | `Control | `Correction | `Figment ]

let pp_alias_tag ppf t = Format.fprintf ppf "%s" begin match t with
  | `Abbreviation -> "Abbreviation"
  | `Alternate -> "Alternate"
  | `Control -> "Control"
  | `Correction -> "Correction"
  | `Figment -> "Figment"
  end
