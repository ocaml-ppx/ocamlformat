(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

module Make (X : Order.S) = struct

  module XSet =
    Set.Make(X)

  type t = {
    (* A FIFO queue. *)
    queue: X.t Queue.t;
    (* A set of all elements ever inserted into the queue. *)
    mutable inserted: XSet.t;
    (* The cardinality of this set. *)
    mutable count: int;
  }

  let create () =
    let queue = Queue.create()
    and inserted = XSet.empty
    and count = 0 in
    { queue; inserted; count }

  let add x t =
    not (XSet.mem x t.inserted) && begin
      Queue.add x t.queue;
      t.inserted <- XSet.add x t.inserted;
      t.count <- t.count + 1;
      true
    end

  let repeatedly t f =
    try
      while true do
        f (Queue.take t.queue)
      done
    with Queue.Empty ->
      ()

  let count t =
    t.count

end
