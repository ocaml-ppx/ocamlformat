(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
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

  let indic_conjunct_break u =
    Uucp_tmapbyte.get Uucp_break_data.indic_conjunct_break_map (Uchar.to_int u)

  let indic_conjunct_break_max = indic_conjunct_break_max
  let indic_conjunct_break_of_int = indic_conjunct_break_of_byte
end

let line u = Array.unsafe_get Low.line_of_int (Low.line u)
let grapheme_cluster u = Array.unsafe_get Low.grapheme_cluster_of_int
    (Low.grapheme_cluster u)

let word u = Array.unsafe_get Low.word_of_int (Low.word u)
let sentence u = Array.unsafe_get Low.sentence_of_int (Low.sentence u)
let indic_conjunct_break u =
  Array.unsafe_get Low.indic_conjunct_break_of_int
    (Low.indic_conjunct_break u)

let east_asian_width u =
  Uucp_rmap.get Uucp_break_data.east_asian_width_map (Uchar.to_int u)

let tty_width_hint =
  let gc i = Uucp__gc.general_category (Uchar.unsafe_of_int i) in
  let eaw i = east_asian_width (Uchar.unsafe_of_int i) in
  fun u -> match Uchar.to_int u with
  (* U+0000 is actually safe to (non-)render. *)
  | 0 -> 0
  (* C0 or DELETE and C1 (general category Cc) is non-sensical. *)
  |u when u <= 0x001F || 0x007F <= u && u <= 0x009F -> -1
  (* Euro-centric fast path (blocks ASCII - Modifier Letters).
     Notably includes one Cf character, U+00AD (Soft hyphen). *)
  | u when u <= 0x02FF -> 1
  (* Non-spacing. *)
  | u when (let c = gc u in c = `Mn || c = `Me || c = `Cf) -> 0
  (* Wide east-asian; intersects non-spacing. *)
  | u when (let w = eaw u in w = `W || w = `F) -> 2
  (* or else. Notably includes Zl (U+2028) and Zp (U+2029). *)
  | _ -> 1
