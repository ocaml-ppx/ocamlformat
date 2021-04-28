(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

let pp_script_variant ppf s = Gen.pp ppf "`%a" Uucp_script_base.pp s

let pp_script_prop ppf ucd =
  let size v = 0 in
  Gen.pp_prop_rmap_ucd ~share:false ppf ucd
    Uucd.script "script" "Uucp_script_base.t" pp_script_variant
    ~default:`Zzzz size

let pp_script_extensions_prop ppf ucd =
  let size v = 3 * (List.length v) in
  let pp_script_list = Gen.pp_list pp_script_variant in
  let prop u =
    let script = Gen.ucd_get ucd u Uucd.script in
    let es = Gen.ucd_get ucd u Uucd.script_extensions in
    match es with
    | [] -> assert false
    | [script'] when script = script' -> []
    | es -> es
  in
  Gen.pp_prop_rmap ppf
    prop "script_extensions" "Uucp_script_base.t list" pp_script_list
    ~default:[] size

let pp_props ppf ucd =
  pp_script_prop ppf ucd;
  pp_script_extensions_prop ppf ucd;
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
