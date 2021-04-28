(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

include Uucp_break_base

module Low = struct
  let line u =
    Uucp_tmapbyte.get Uucp_break_data.line_break_map (Uchar.to_int u)

  let line_max = line_max
  let line_of_int = line_of_byte

  let grapheme_cluster u =
    Uucp_tmapbyte.get Uucp_break_data.grapheme_cluster_break_map
      (Uchar.to_int u)

  let grapheme_cluster_max = grapheme_cluster_max
  let grapheme_cluster_of_int = grapheme_cluster_of_byte

  let word u = Uucp_tmapbyte.get Uucp_break_data.word_break_map (Uchar.to_int u)
  let word_max = word_max
  let word_of_int = word_of_byte

  let sentence u =
    Uucp_tmapbyte.get Uucp_break_data.sentence_break_map (Uchar.to_int u)

  let sentence_max = sentence_max
  let sentence_of_int = sentence_of_byte
end

let line u = Array.unsafe_get Low.line_of_int (Low.line u)
let grapheme_cluster u = Array.unsafe_get Low.grapheme_cluster_of_int
    (Low.grapheme_cluster u)

let word u = Array.unsafe_get Low.word_of_int (Low.word u)
let sentence u = Array.unsafe_get Low.sentence_of_int (Low.sentence u)

let east_asian_width u =
  Uucp_rmap.get Uucp_break_data.east_asian_width_map (Uchar.to_int u)

let tty_width_hint =
  let gc i = Uucp_gc.general_category (Uchar.unsafe_of_int i) in
  let eaw i = east_asian_width (Uchar.unsafe_of_int i) in
  fun u -> match Uchar.to_int u with
  (* C0 (without U+0000) or DELETE and C1 is non-sensical. *)
  |u when 0 < u && u <= 0x001F || 0x007F <= u && u <= 0x009F -> -1
  (* U+0000 is actually safe to (non-)render. *)
  | 0 -> 0
  (* Soft Hyphen. *)
  | 0x00AD -> 1
  (* Line/Paragraph Separator. 1 seems more frequent than 0 and we
     never saw -1, i.e. correct handling. *)
  | 0x2028 | 0x2029 -> 1
  (* Euro-centric fast path: does not intersect branches below. *)
  | u when u <= 0x02FF -> 1
  (* Wide east-asian. *)
  | u when (let w = eaw u in w = `W || w = `F) -> 2
  (* Non-spacing, unless stated otherwise. *)
  | u when (let c = gc u in c = `Mn || c = `Me || c = `Cf) -> 0
  (* or else. *)
  | _ -> 1

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
