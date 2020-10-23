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

module Parsetree : sig
  include module type of Ppxlib.Parsetree

  val equal_core_type : core_type -> core_type -> bool

  val equal_structure : structure -> structure -> bool

  val equal_signature : signature -> signature -> bool

  val equal_toplevel_phrase : toplevel_phrase -> toplevel_phrase -> bool
end

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

  val fmt : Format.formatter -> t -> unit

  val smallest : t -> t list -> t

  val width : t -> int

  val is_single_line : t -> int -> bool

  type location = t

  module Set : sig
    type t

    val empty : t

    val add : location -> t -> t

    val remove : location -> t -> t

    val to_list : t -> location list
  end

  module Multimap : sig
    type 'a t

    val empty : 'a t

    val add_list : 'a t -> location -> 'a list -> 'a t

    val update_multi :
         'a t
      -> src:location
      -> dst:location
      -> f:('a list -> 'a list -> 'a list)
      -> 'a t

    val find : 'a t -> location -> 'a list option

    val remove : 'a t -> location -> 'a t

    val filter : 'a t -> f:('a -> bool) -> 'a t

    val mem : 'a t -> location -> bool

    val to_list : 'a t -> 'a list

    val find_multi : 'a t -> location -> 'a list
  end
end

module Mapper : sig
  type 'a fragment =
    | Structure : Parsetree.structure fragment
    | Signature : Parsetree.signature fragment
    | Use_file : Parsetree.toplevel_phrase list fragment

  val equal : 'a fragment -> 'a -> 'a -> bool

  val map_ast : 'a fragment -> Ppxlib.Ast_traverse.map -> 'a -> 'a
end

module Parse : sig
  val fragment : 'a Mapper.fragment -> Lexing.lexbuf -> 'a
end

module Printast : sig
  val implementation : Format.formatter -> Parsetree.structure -> unit

  val interface : Format.formatter -> Parsetree.signature -> unit

  val payload : Format.formatter -> Parsetree.payload -> unit

  val expression : Format.formatter -> Parsetree.expression -> unit

  val use_file : Format.formatter -> Parsetree.toplevel_phrase list -> unit

  val fragment : 'a Mapper.fragment -> Format.formatter -> 'a -> unit
end

module Pprintast = Ppxlib.Pprintast

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
  val token : Lexing.lexbuf -> Parser.token

  type error

  exception Error of error * Location.t
end
