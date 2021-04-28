(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

let pp_line_break ppf ucd =
  Gen.pp_code_prop_tmapbyte_ucd ppf ucd
    Uucp_break_base.line_to_byte Uucd.line_break "line_break" ~default:`XX
    Uucp_break_base.pp_line

let pp_grapheme_cluster_break ppf ucd =
  Gen.pp_code_prop_tmapbyte_ucd ppf ucd
    Uucp_break_base.grapheme_cluster_to_byte Uucd.grapheme_cluster_break
    "grapheme_cluster_break" ~default:`XX Uucp_break_base.pp_grapheme_cluster

let pp_word_break ppf ucd =
  Gen.pp_code_prop_tmapbyte_ucd ppf ucd
    Uucp_break_base.word_to_byte Uucd.word_break
    "word_break" ~default:`XX Uucp_break_base.pp_word

let pp_sentence_break ppf ucd =
  Gen.pp_code_prop_tmapbyte_ucd ppf ucd
    Uucp_break_base.sentence_to_byte Uucd.sentence_break
    "sentence_break" ~default:`XX Uucp_break_base.pp_sentence

let pp_east_asian_width ppf ucd =
  let size _ = 0 in
  let pp ppf w = Gen.pp ppf "`%a" Uucp_break_base.pp_east_asian_width w in
  Gen.pp_prop_rmap_ucd ~share:false ppf ucd
    Uucd.east_asian_width "east_asian_width" "Uucp_break_base.east_asian_width"
    pp ~default:`N size

let pp_props ppf ucd =
  pp_line_break ppf ucd;
  pp_grapheme_cluster_break ppf ucd;
  pp_word_break ppf ucd;
  pp_sentence_break ppf ucd;
  pp_east_asian_width ppf ucd;
  ()

let pp_mod ppf ucd = Gen.pp_mod pp_props ppf ucd

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
