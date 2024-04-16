(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t =
  [ `Cc
  | `Cf
  | `Cn
  | `Co
  | `Cs
  | `Ll
  | `Lm
  | `Lo
  | `Lt
  | `Lu
  | `Mc
  | `Me
  | `Mn
  | `Nd
  | `Nl
  | `No
  | `Pc
  | `Pd
  | `Pe
  | `Pf
  | `Pi
  | `Po
  | `Ps
  | `Sc
  | `Sk
  | `Sm
  | `So
  | `Zl
  | `Zp
  | `Zs ]

let pp ppf c = Format.fprintf ppf "%s" begin match c with
  | `Cc -> "Cc"
  | `Cf -> "Cf"
  | `Cn -> "Cn"
  | `Co -> "Co"
  | `Cs -> "Cs"
  | `Ll -> "Ll"
  | `Lm -> "Lm"
  | `Lo -> "Lo"
  | `Lt -> "Lt"
  | `Lu -> "Lu"
  | `Mc -> "Mc"
  | `Me -> "Me"
  | `Mn -> "Mn"
  | `Nd -> "Nd"
  | `Nl -> "Nl"
  | `No -> "No"
  | `Pc -> "Pc"
  | `Pd -> "Pd"
  | `Pe -> "Pe"
  | `Pf -> "Pf"
  | `Pi -> "Pi"
  | `Po -> "Po"
  | `Ps -> "Ps"
  | `Sc -> "Sc"
  | `Sk -> "Sk"
  | `Sm -> "Sm"
  | `So -> "So"
  | `Zl -> "Zl"
  | `Zp -> "Zp"
  | `Zs -> "Zs"
  end
