(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uuseg programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

type 'a folder = 'a -> string -> 'a

let fold
    (fold : ?pos:int -> ?len:int -> 'a Uutf.String.folder -> 'a -> string -> 'a)
    enc seg f acc0 s
  =
  let b = Buffer.create 42 in
  let flush_segment acc =
    let segment = Buffer.contents b in
    Buffer.clear b; if segment = "" then acc else f acc segment
  in
  let seg = Uuseg.create (seg :> Uuseg.boundary) in
  let rec add acc v = match Uuseg.add seg v with
  | `Uchar u -> enc b u; add acc `Await
  | `Boundary -> add (flush_segment acc) `Await
  | `Await | `End -> acc
  in
  let rec uchar acc _ = function
  | `Uchar _ as u -> add acc u
  | `Malformed _ -> add acc (`Uchar Uutf.u_rep)
  in
  flush_segment (add (fold uchar acc0 s) `End)

let fold_utf_8 seg f acc0 s =
  fold Uutf.String.fold_utf_8 Uutf.Buffer.add_utf_8 seg f acc0 s

let fold_utf_16be seg f acc0 s =
  fold Uutf.String.fold_utf_16be Uutf.Buffer.add_utf_16be seg f acc0 s

let fold_utf_16le seg f acc0 s =
  fold Uutf.String.fold_utf_16le Uutf.Buffer.add_utf_16le seg f acc0 s

let pp_utf_8 ppf s =
  let b = Buffer.create 10 in
  let flush () =
    let gc = Buffer.contents b in
    if gc = "" then () else (Format.fprintf ppf "@<1>%s" gc; Buffer.clear b)
  in
  let seg = Uuseg.create `Grapheme_cluster in
  let rec add a = match Uuseg.add seg a with
  | `Uchar u -> Uutf.Buffer.add_utf_8 b u; add `Await
  | `Boundary -> flush (); add `Await
  | `Await | `End -> ()
  in
  let rec uchar () _ = function
  | `Uchar _ as u -> add u
  | `Malformed _ -> add (`Uchar Uutf.u_rep)
  in
  Uutf.String.fold_utf_8 uchar () s; add `End; flush ()

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
      | _ -> Uutf.Buffer.add_utf_8 b last; buf_buf := Some u
  in
  let buf_cut mandatory =
    let bbuf = !buf_buf in
    buf_buf := None;
    match bbuf with
    | None -> buf_flush (); Format.pp_print_cut ppf ()
    | Some u when mandatory && Uucp.White.is_white_space u ->
        buf_flush (); Format.pp_force_newline ppf ()
    | Some u when mandatory -> (* should not happen *)
        Uutf.Buffer.add_utf_8 b u; buf_flush (); Format.pp_force_newline ppf ()
    | Some u when Uucp.White.is_white_space u ->
        buf_flush (); Format.pp_print_break ppf 1 0;
    | Some u ->
        Uutf.Buffer.add_utf_8 b u; buf_flush (); Format.pp_print_cut ppf ()
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
  let rec uchar () _ = function
  | `Uchar _ as u -> add u
  | `Malformed _ -> add (`Uchar Uutf.u_rep)
  in
  Uutf.String.fold_utf_8 uchar () s; add `End

let pp_utf_8_lines = pp_utf_8_text ~only_mandatory:true
let pp_utf_8_text = pp_utf_8_text ~only_mandatory:false

(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uuseg programmers

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
