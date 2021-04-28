(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(* General properties *)

let is_default_ignorable u =
  Uucp_tmapbool.get Uucp_gen_data.default_ignorable_map (Uchar.to_int u)

let is_deprecated u =
  Uucp_tmapbool.get Uucp_gen_data.deprecated_map (Uchar.to_int u)

let is_logical_order_exception u =
  Uucp_tmapbool.get Uucp_gen_data.logical_order_exception_map (Uchar.to_int u)

let is_non_character u =
  Uucp_tmapbool.get Uucp_gen_data.non_character_map (Uchar.to_int u)

let is_variation_selector u =
  Uucp_tmapbool.get Uucp_gen_data.variation_selector_map (Uchar.to_int u)

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
