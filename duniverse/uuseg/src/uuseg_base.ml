(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uuseg programmers. All rights reserved.
   SPDX-License-Identifier: ISC
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
