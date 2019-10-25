(**********************************************************************
 *                                                                    *
 *                            OCamlFormat                             *
 *                                                                    *
 *  Copyright (c) 2019-present, Facebook, Inc.  All rights reserved.  *
 *                                                                    *
 *  This source code is licensed under the MIT license found in the   *
 *  LICENSE file in the root directory of this source tree.           *
 *                                                                    *
 **********************************************************************)

val with_warning_filter :
  filter:(Location.t -> Warnings.t -> bool) -> f:(unit -> 'a) -> 'a

val print_warning : Location.t -> Warnings.t -> unit

val make_printf :
     ('b -> ('b, 'c) CamlinternalFormat.acc -> 'd)
  -> 'b
  -> ('b, 'c) CamlinternalFormat.acc
  -> ('a, 'b, 'c, 'c, 'c, 'd) CamlinternalFormatBasics.fmt
  -> 'a

module Stack : sig
  type 'a t

  val create : unit -> 'a t

  val push : 'a -> 'a t -> unit

  val clear : 'a t -> unit

  val top_opt : 'a t -> 'a option

  val pop_opt : 'a t -> 'a option

  val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
end

module Queue : sig
  type 'a t

  val create : unit -> 'a t

  val add : 'a -> 'a t -> unit

  val clear : 'a t -> unit

  val peek_opt : 'a t -> 'a option

  val take_opt : 'a t -> 'a option

  val take : 'a t -> 'a
end

module Int : sig
  val to_string : int -> string
end
