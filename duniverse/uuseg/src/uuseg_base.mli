(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uuseg programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Segmenter commonalities. *)

(** {1 Common} *)

type ret = [ `Await | `Boundary | `End | `Uchar of Uchar.t ]
(** See {!Uuseg.ret}. *)

val pp_ret : Format.formatter -> [< ret ] -> unit
(** See {!Uuseg.pp_ret}. *)

val err_exp_await : [< ret] -> 'a
(** See {!Uuseg.err_exp_await}. *)

val err_ended : [< ret] -> 'a
(** See {!Uuseg.err_ended}. *)
