(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

include Uucp_name_base

let tok_len i =
  let rec loop size i =
    if String.unsafe_get Uucp_name_data.name_toks i = '\x00' then size else
    loop (size + 1) (i + 1)
  in
  loop 0 i

let get_tok i = String.sub Uucp_name_data.name_toks i (tok_len i)

let name u =
  let u = Uchar.to_int u in
  match Uucp_tmap5bytes.get_uint20_pair Uucp_name_data.name_map u with
  | 0, 0 -> ""
  | l, 0 -> get_tok l
  | 0, r -> Printf.sprintf "%s%04X" (get_tok r) u
  | l, r -> String.concat "" [get_tok l; get_tok r]

let name_alias u = Uucp_cmap.get Uucp_name_data.name_alias_map (Uchar.to_int u)

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
