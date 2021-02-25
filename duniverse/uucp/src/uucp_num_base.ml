(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

type numeric_type = [ `De | `Di | `None | `Nu ]
type numeric_value = [ `Frac of int * int | `NaN | `Num of int64 ]

let pp_numeric_type ppf v = Format.fprintf ppf "%s" begin match v with
  | `De -> "De"
  | `Di -> "Di"
  | `None -> "None"
  | `Nu -> "Nu"
  end

let pp_numeric_value ppf = function
| `NaN -> Format.fprintf ppf "NaN"
| `Frac (a, b) -> Format.fprintf ppf "Frac(%d,%d)" a b
| `Num n -> Format.fprintf ppf "Num(%LdL)" n


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
