(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uuseg programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type 'a folder = 'a -> string -> 'a

let fold dec_uchar enc_uchar seg f acc0 s =
  let flush_segment buf acc =
    let segment = Buffer.contents buf in
    Buffer.clear buf; if segment = "" then acc else f acc segment
  in
  let rec add buf acc segmenter v = match Uuseg.add segmenter v with
  | `Uchar u -> enc_uchar buf u; add buf acc segmenter `Await
  | `Boundary -> add buf (flush_segment buf acc) segmenter `Await
  | `Await | `End -> acc
  in
  let rec loop buf acc s i max segmenter =
    if i > max then flush_segment buf (add buf acc segmenter `End) else
    let dec = dec_uchar s i in
    let acc = add buf acc segmenter (`Uchar (Uchar.utf_decode_uchar dec)) in
    loop buf acc s (i + Uchar.utf_decode_length dec) max segmenter
  in
  let buf = Buffer.create 42 in
  let segmenter = Uuseg.create seg in
  loop buf acc0 s 0 (String.length s - 1) segmenter

let fold_utf_8 seg f acc0 s =
  fold String.get_utf_8_uchar Buffer.add_utf_8_uchar seg f acc0 s

let fold_utf_16be seg f acc0 s =
  fold String.get_utf_16be_uchar Buffer.add_utf_16be_uchar seg f acc0 s

let fold_utf_16le seg f acc0 s =
  fold String.get_utf_16le_uchar Buffer.add_utf_16le_uchar seg f acc0 s

let pp_utf_8 ppf s =
  let flush buf =
    let gc = Buffer.contents buf in
    if gc = "" then () else (Format.fprintf ppf "@<1>%s" gc; Buffer.clear buf)
  in
  let rec add buf segmenter v = match Uuseg.add segmenter v with
  | `Uchar u -> Buffer.add_utf_8_uchar buf u; add buf segmenter `Await
  | `Boundary -> flush buf; add buf segmenter `Await
  | `Await | `End -> ()
  in
  let rec loop buf s i max segmenter =
    if i > max then (add buf segmenter `End; flush buf) else
    let dec = String.get_utf_8_uchar s i in
    add buf segmenter (`Uchar (Uchar.utf_decode_uchar dec));
    loop buf s (i + Uchar.utf_decode_length dec) max segmenter
  in
  let buf = Buffer.create 10 in
  let segmenter = Uuseg.create `Grapheme_cluster in
  loop buf s 0 (String.length s - 1) segmenter

let pp_utf_8_text ~only_mandatory ppf s =
  let b = Buffer.create 10 in
  let buf_buf = ref None in (* buffer to handle CRLF and suppress white *)
  let buf_flush () =
    let gc = Buffer.contents b in
    if gc = "" then () else (Format.fprintf ppf "@<1>%s" gc; Buffer.clear b)
  in
  let buf_add u = match !buf_buf with
  | None -> buf_buf := Some u
  | Some last ->
      match Uchar.to_int last with
      | 0x000D when Uchar.to_int u = 0x000A -> buf_buf := Some u (* rem CR *)
      | _ -> Buffer.add_utf_8_uchar b last; buf_buf := Some u
  in
  let buf_cut mandatory =
    let bbuf = !buf_buf in
    buf_buf := None;
    match bbuf with
    | None -> buf_flush (); Format.pp_print_cut ppf ()
    | Some u when mandatory && Uucp.White.is_white_space u ->
        buf_flush (); Format.pp_force_newline ppf ()
    | Some u when mandatory -> (* should not happen *)
        Buffer.add_utf_8_uchar b u; buf_flush (); Format.pp_force_newline ppf ()
    | Some u when Uucp.White.is_white_space u ->
        buf_flush (); Format.pp_print_break ppf 1 0;
    | Some u ->
        Buffer.add_utf_8_uchar b u; buf_flush (); Format.pp_print_cut ppf ()
  in
  let gseg = Uuseg.create `Grapheme_cluster in
  let lseg = Uuseg.create `Line_break in
  let rec line_add a = match Uuseg.add lseg a with
  | `Uchar u -> buf_add u; line_add `Await
  | `Boundary ->
      let m = Uuseg.mandatory lseg in
      if (only_mandatory && m) || (not only_mandatory) then buf_cut m;
      line_add `Await
  | `Await | `End -> ()
  in
  let rec add a = match Uuseg.add gseg a with
  | `Uchar _ as a -> line_add a; add `Await
  | `Boundary -> buf_flush (); add `Await
  | `Await -> ()
  | `End -> line_add `End; ()
  in
  let rec loop s i max =
    if i > max then add `End else
    let dec = String.get_utf_8_uchar s i in
    add (`Uchar (Uchar.utf_decode_uchar dec));
    loop s (i + Uchar.utf_decode_length dec) max
  in
  loop s 0 (String.length s - 1)

let pp_utf_8_lines = pp_utf_8_text ~only_mandatory:true
let pp_utf_8_text = pp_utf_8_text ~only_mandatory:false
