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

(** Opened in each source module to establish global namespace *)

include module type of Base

include module type of Stdio

include module type of Compat

module Format = Caml.Format
module Filename = Caml.Filename

val ( >> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
(** Composition of functions: [(f >> g) x] is exactly equivalent to
    [g (f (x))]. Left associative. *)

val impossible : string -> _
(** Indicate why the call is expected to be impossible. *)

module Fpath : sig
  include module type of Fpath

  val cwd : unit -> t
  (** Current working directory, relying on [Unix]. *)

  val exists : t -> bool
  (** [exists p] returns whether the given path [p] exists. *)

  val to_absolute : t -> t
  (** [to_absolute p] returns [cwd]/[p] if the [p] is relative, otherwise
      returns [p]. *)

  val to_string : ?relativize:bool -> t -> string
  (** If [relativize] is set to [true] (it is set to [false] by default), the
      path is relativized according to the [cwd]. *)

  val pp : Format.formatter -> t -> unit
end
