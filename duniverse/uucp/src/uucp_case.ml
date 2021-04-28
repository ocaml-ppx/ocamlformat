(*---------------------------------------------------------------------------
   Copyright (c) 2013 The uucp programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(* Case properties *)

let is_upper u = Uucp_tmapbool.get Uucp_case_data.upper_map (Uchar.to_int u)
let is_lower u = Uucp_tmapbool.get Uucp_case_data.lower_map (Uchar.to_int u)
let is_cased u = Uucp_tmapbool.get Uucp_case_data.cased_map (Uchar.to_int u)
let is_case_ignorable u =
  Uucp_tmapbool.get Uucp_case_data.case_ignorable_map (Uchar.to_int u)

(* Case mappings *)

module Map = Uucp_case_map
module Fold = Uucp_case_fold
module Nfkc_fold = Uucp_case_nfkc

(*---------------------------------------------------------------------------
   Copyright (c) 2013 The uucp programmers

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
