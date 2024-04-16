(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uuseg programmers. All rights reserved.
   SPDX-License-Identifier: ISC
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
