(**************************************************************************)
(*                                                                        *)
(*                              OCamlFormat                               *)
(*                                                                        *)
(*            Copyright (c) Facebook, Inc. and its affiliates.            *)
(*                                                                        *)
(*      This source code is licensed under the MIT license found in       *)
(*      the LICENSE file in the root directory of this source tree.       *)
(*                                                                        *)
(**************************************************************************)

(** The [IO] module defines the blocking interface for reading and writing to
    Cohttp streams *)

module type S = sig
  (** ['a t] represents a blocking monad state *)
  type 'a t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  (** [a >>= b] will pass the result of [a] to the [b] function. This is a
      monadic [bind]. *)

  val return : 'a -> 'a t
  (** [return a] will construct a constant IO value. *)

  (** [ic] represents an input channel *)
  type ic

  (** [oc] represents an output channel *)
  type oc

  val read : ic -> Csexp.t option t

  val write : oc -> Csexp.t list -> unit t
end
