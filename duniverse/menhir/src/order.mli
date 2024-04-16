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

module Option (X : S) : S with type t = X.t option

module Pair (X : S) (Y : S) : S with type t = X.t * Y.t
