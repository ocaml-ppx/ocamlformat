(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
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
