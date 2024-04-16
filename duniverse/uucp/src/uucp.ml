(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Unicode version *)

let unicode_version = Uucp_version_data.unicode_version

(* Properties *)

module Age = Uucp__age
module Alpha = Uucp__alpha
module Break = Uucp__break
module Block = Uucp__block
module Case = Uucp__case
module Cjk = Uucp__cjk
module Emoji = Uucp__emoji
module Func = Uucp__func
module Gc = Uucp__gc
module Gen = Uucp__gen
module Hangul = Uucp__hangul
module Id = Uucp__id
module Name = Uucp__name
module Num = Uucp__num
module Script = Uucp__script
module White = Uucp__white

(* Maps. Not part of the public API. *)

module Cmap = Uucp_cmap
module Rmap = Uucp_rmap
module Tmap = Uucp_tmap
module Tmapbool = Uucp_tmapbool
module Tmapbyte = Uucp_tmapbyte
