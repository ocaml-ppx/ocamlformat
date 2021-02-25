(*---------------------------------------------------------------------------
   Copyright (c) 2020 The uucp programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

let pp_props ppf ucd =
  let prop = Gen.pp_prop_tmapbool_ucd ppf ucd in
  prop Uucd.emoji "emoji";
  prop Uucd.emoji_presentation "emoji_presentation";
  prop Uucd.emoji_modifier "emoji_modifier";
  prop Uucd.emoji_modifier_base "emoji_modifier_base";
  prop Uucd.emoji_component "emoji_component";
  prop Uucd.extended_pictographic "extended_pictographic";
  ()

let pp_mod ppf ucd = Gen.pp_mod pp_props ppf ucd

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
