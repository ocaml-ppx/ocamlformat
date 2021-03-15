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

module Ast_helper = Ppxlib.Ast_helper

module Asttypes : sig
  include module type of Ppxlib.Asttypes

  val is_private : private_flag -> bool

  val is_open : closed_flag -> bool

  val is_override : override_flag -> bool

  val is_mutable : mutable_flag -> bool
end

module Position : sig
  type t = Lexing.position

  include Comparator.S with type t := t

  val column : t -> int

  val distance : t -> t -> int

  val compare : t -> t -> int
end

module Location : sig
  include module type of Ppxlib.Location

  type comparator_witness

  val comparator : (t, comparator_witness) Comparator.t

  val contains : t -> t -> bool

  val sexp_of_t : t -> Sexp.t

  val compare_width_decreasing : t -> t -> int
  (** Compare, in order:

      - start
      - end (in reverse order)
      - ghostness

      Locs (start and end) are compared using [Position.compare]. *)

  val compare : t -> t -> int

  val compare_start : t -> t -> int

  val compare_start_col : t -> t -> int

  val compare_end : t -> t -> int

  val compare_end_col : t -> t -> int

  val line_difference : t -> t -> int
  (** [line_difference x y] returns the difference between the line at the
      start of [y] and at the end of [x]. [x] must precede [y], undefined
      behavior otherwise, or if one location includes the other. *)

  val fmt : Format.formatter -> t -> unit

  val smallest : t -> t list -> t

  val width : t -> int

  val is_single_line : t -> int -> bool

  val to_span : t -> Odoc_model.Location_.span
end

module Longident : sig
  type t = Longident.t =
    | Lident of string
    | Ldot of t * string
    | Lapply of t * t

  val flatten : t -> string list

  val last : t -> string

  val lident : string -> t
  (** Make a Lident from a dotless string *)
end

module Parser = Token_latest

module Lexer : sig
  val token_with_comments : Lexing.lexbuf -> Parser.token

  type error

  exception Error of error * Location.t
end
