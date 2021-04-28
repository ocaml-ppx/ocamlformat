(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uuseg programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

type ret = [ `Await | `Boundary | `End | `Uchar of Uchar.t ]

let pp_ret ppf v = match (v :> ret) with
| `Await -> Format.fprintf ppf "`Await"
| `Boundary -> Format.fprintf ppf "`Boundary"
| `End -> Format.fprintf ppf "`End"
| `Uchar u -> Format.fprintf ppf "`Uchar U+%04X" (Uchar.to_int u)

let err_exp_await add =
  invalid_arg (Format.asprintf "can't add %a, expected `Await" pp_ret add)

let err_ended add =
  invalid_arg (Format.asprintf "can't add %a, `End already added" pp_ret add)

(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uuseg programmers

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
