(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
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
