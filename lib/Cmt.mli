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

include Comparator.S with type t := t

type error =
  { kind: [`Added of t | `Modified of t * t | `Dropped of t]
  ; cmt_kind: [`Comment | `Doc_comment] }

val pp_error : Format.formatter -> error -> unit

type pos = Before | Within | After

type loc = t

module Comparator_no_loc : sig
  type t = loc

  include Comparator.S with type t := t
end

type decoded_kind =
  | Verbatim of string  (** Original content. *)
  | Doc of string  (** Original content. *)
  | Normal of string  (** Original content with whitespaces trimmed. *)
  | Code of string list
      (** Source code is line splitted with indentation removed. *)
  | Asterisk_prefixed of string list
      (** Line splitted with asterisks removed. *)

type decoded =
  { prefix: string  (** Just after the opening. *)
  ; suffix: string  (** Just before the closing. *)
  ; kind: decoded_kind }

val decode : parse_comments_as_doc:bool -> t -> decoded
