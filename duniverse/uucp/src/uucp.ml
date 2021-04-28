(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(* Unicode version *)

let unicode_version = "%%UNICODE_VERSION%%"

(* Properties *)

module Age = Uucp_age
module Alpha = Uucp_alpha
module Break = Uucp_break
module Block = Uucp_block
module Case = Uucp_case
module Cjk = Uucp_cjk
module Emoji = Uucp_emoji
module Func = Uucp_func
module Gc = Uucp_gc
module Gen = Uucp_gen
module Hangul = Uucp_hangul
module Id = Uucp_id
module Name = Uucp_name
module Num = Uucp_num
module Script = Uucp_script
module White = Uucp_white

(* Maps. Not part of the public API. *)

module Cmap = Uucp_cmap
module Rmap = Uucp_rmap
module Tmap = Uucp_tmap
module Tmapbool = Uucp_tmapbool
module Tmapbyte = Uucp_tmapbyte

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
