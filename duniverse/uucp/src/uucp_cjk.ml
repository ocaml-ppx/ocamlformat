(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

let is_ideographic u =
  Uucp_tmapbool.get Uucp_cjk_data.ideographic_map (Uchar.to_int u)

let is_ids_bin_op u =
  Uucp_tmapbool.get Uucp_cjk_data.ids_bin_op_map (Uchar.to_int u)

let is_ids_tri_op u =
  Uucp_tmapbool.get Uucp_cjk_data.ids_tri_op_map (Uchar.to_int u)

let is_radical u =
  Uucp_tmapbool.get Uucp_cjk_data.radical_map (Uchar.to_int u)

let is_unified_ideograph u =
  Uucp_tmapbool.get Uucp_cjk_data.unified_ideograph_map (Uchar.to_int u)

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
