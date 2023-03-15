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

open Migrate_ast

type t = private {txt: string; loc: Location.t}

val create : string -> Location.t -> t

val loc : t -> Location.t

val txt : t -> string

val dummy : t

val is_dummy : t -> bool

include Comparator.S with type t := t

type pos = Before | Within | After

type loc = t

module Comparator_no_loc : sig
  type t = loc

  include Comparator.S with type t := t
end
