(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

module Make (X : Order.S) : sig

  (** A type of FIFO queues that detects and eliminates duplicate elements. *)
  type t

  (**[create()] creates a fresh empty queue. *)
  val create: unit -> t

  (**[add x q] inserts the element [x] into the queue [q], unless this element
     has already been inserted into [q] in the past, in which case it is
     ignored. The Boolean result indicates whether [x] was inserted. *)
  val add: X.t -> t -> bool

  (**[repeatedly q f] repeatedly extracts an element [x] out of the queue [q]
     and invokes [f x], until the queue [q] becomes empty. The function [f]
     may insert new elements into the queue. *)
  val repeatedly: t -> (X.t -> unit) -> unit

  (**[count q] returns a count of the (unique) elements that have been
     inserted into the queue [q] so far. *)
  val count: t -> int

end
