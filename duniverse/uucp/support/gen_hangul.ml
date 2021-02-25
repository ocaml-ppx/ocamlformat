(*---------------------------------------------------------------------------
   Copyright (c) 2018 The uucp programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

let pp_syllable_type ppf ucd =
  let size v = 0 in
  let pp_stype ppf t = Gen.pp ppf "`%a" Uucp_hangul_base.pp_syllable_type t in
  Gen.pp_prop_rmap_ucd ~share:false ppf ucd Uucd.hangul_syllable_type
    "syllable_type" "Uucp_hangul_base.syllable_type" pp_stype
    ~default:`NA size

let pp_props ppf ucd =
  pp_syllable_type ppf ucd;
  ()

let pp_mod ppf ucd = Gen.pp_mod pp_props ppf ucd

(*---------------------------------------------------------------------------
   Copyright (c) 2018 The uucp programmers

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
