(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uuseg programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

let dummy_u =                            (* to initalize arrays, never read. *)
  `Uchar (Uchar.unsafe_of_int 0x0000)

type t =
  { mutable buf : [`Uchar of Uchar.t] array;
    mutable first : int;
    mutable last : int; }

let create n = { buf = Array.make n dummy_u; first = 0; last = -1; }
let copy b = { b with buf = Array.copy b.buf; }
let empty b = b.last = -1
let len b = (b.last - b.first) + 1
let grow b =
  let len = Array.length b.buf in
  let newbuf = Array.make (2 * len) dummy_u in
  Array.blit b.buf 0 newbuf 0 len; b.buf <- newbuf

let add b add =
  let last = b.last + 1 in
  if last = Array.length b.buf then grow b;
  b.buf.(last) <- add; b.last <- last

let flush b =
  let `Uchar _ as add = b.buf.(b.first) in
  b.first <- b.first + 1;
  if b.first > b.last then (b.first <- 0; b.last <- -1);
  add

let get_first b = let `Uchar u = b.buf.(b.first) in u

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
