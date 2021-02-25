(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

include Uucp_num_base

let is_ascii_hex_digit u =
  Uucp_tmapbool.get Uucp_num_data.ascii_hex_digit_map (Uchar.to_int u)

let is_hex_digit u =
  Uucp_tmapbool.get Uucp_num_data.hex_digit_map (Uchar.to_int u)

let numeric_type u =
  Uucp_rmap.get Uucp_num_data.numeric_type_map (Uchar.to_int u)

let numeric_value u =
  Uucp_cmap.get Uucp_num_data.numeric_value_map (Uchar.to_int u)

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
