(*---------------------------------------------------------------------------
   Copyright (c) 2020 The uucp programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

let is_emoji u =
  Uucp_tmapbool.get Uucp_emoji_data.emoji_map (Uchar.to_int u)

let is_emoji_presentation u =
  Uucp_tmapbool.get Uucp_emoji_data.emoji_presentation_map (Uchar.to_int u)

let is_emoji_modifier u =
  Uucp_tmapbool.get Uucp_emoji_data.emoji_modifier_map (Uchar.to_int u)

let is_emoji_modifier_base u =
  Uucp_tmapbool.get Uucp_emoji_data.emoji_modifier_base_map (Uchar.to_int u)

let is_emoji_component u =
  Uucp_tmapbool.get Uucp_emoji_data.emoji_component_map (Uchar.to_int u)

let is_extended_pictographic u =
  Uucp_tmapbool.get Uucp_emoji_data.extended_pictographic_map (Uchar.to_int u)

(*---------------------------------------------------------------------------
   Copyright (c) 2020 The uucp programmers

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
