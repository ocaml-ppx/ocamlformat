(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uuseg programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

let utf_8_segments seg s =
  let flush_segment buf acc =
    let segment = Buffer.contents buf in
    Buffer.clear buf; if segment = "" then acc else segment :: acc
  in
  let rec add buf acc segmenter v = match Uuseg.add segmenter v with
  | `Uchar u -> Buffer.add_utf_8_uchar buf u; add buf acc segmenter `Await
  | `Boundary -> add buf (flush_segment buf acc) segmenter `Await
  | `Await | `End -> acc
  in
  let rec loop buf acc s i max segmenter =
    if i > max then flush_segment buf (add buf acc segmenter `End) else
    let dec = String.get_utf_8_uchar s i in
    let acc = add buf acc segmenter (`Uchar (Uchar.utf_decode_uchar dec)) in
    loop buf acc s (i + Uchar.utf_decode_length dec) max segmenter
  in
  let buf = Buffer.create 42 in
  let segmenter = Uuseg.create seg in
  List.rev (loop buf [] s 0 (String.length s - 1) segmenter)
