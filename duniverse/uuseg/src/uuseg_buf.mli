(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uuseg programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** [`Uchar] buffers *)

(** {1 [`Uchar] buffers} *)

type t
(** The type for [`Uchar] buffers. *)

val create : int -> t
(** [create n] is a buffer of initial size [n]. *)

val copy : t -> t
(** [copy b] is a copy of [b]. *)

val empty : t -> bool
(** [empty b] is [true] iff [b] is empty. *)

val len : t -> int
(** [len b] is [b]'s length. *)

val add : t -> [`Uchar of Uchar.t] -> unit
(** [add b u] adds [u] at the end of [b]. *)

val flush : t -> [> `Uchar of Uchar.t ]
(** [flush b] is the first [`Uchar] of [b] and removes it from [b]. *)

val get_first : t -> Uchar.t
(** [get_first b] is the first [`Uchar] of [b], if any. *)

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
