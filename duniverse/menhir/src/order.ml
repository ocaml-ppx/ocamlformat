(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

module type S = sig
  type t
  val compare : t -> t -> int
end

module Option (X : S) = struct
  type t = X.t option
  let compare t1 t2 =
    Option.compare X.compare t1 t2
end

module Pair (X : S) (Y : S) = struct
  type t = X.t * Y.t
  let compare (x1, y1) (x2, y2) =
    let c = X.compare x1 x2 in
    if c <> 0 then c else Y.compare y1 y2
end
