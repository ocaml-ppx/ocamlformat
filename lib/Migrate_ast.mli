val selected_version :
  Migrate_parsetree.Versions.OCaml_408.types
  Migrate_parsetree.Versions.ocaml_version

module Selected_version = Ast_408
module Ast_mapper = Selected_version.Ast_mapper
module Ast_helper = Selected_version.Ast_helper

module Parsetree : sig
  include module type of Selected_version.Parsetree

  val equal_core_type : core_type -> core_type -> bool

  val equal_structure : structure -> structure -> bool

  val equal_signature : signature -> signature -> bool

  val equal_toplevel_phrase : toplevel_phrase -> toplevel_phrase -> bool
end

module Asttypes : sig
  include module type of Selected_version.Asttypes

  val is_private : private_flag -> bool

  val is_open : closed_flag -> bool

  val is_override : override_flag -> bool

  val is_mutable : mutable_flag -> bool
end

module Position : sig
  val column : Lexing.position -> int

  val compare : Lexing.position -> Lexing.position -> int

  val distance : Lexing.position -> Lexing.position -> int
end

module Location : sig
  include module type of Selected_version.Location

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

  val fmt : Format_.formatter -> t -> unit

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

module Parse : sig
  val implementation : Lexing.lexbuf -> Parsetree.structure

  val interface : Lexing.lexbuf -> Parsetree.signature

  val use_file : Lexing.lexbuf -> Parsetree.toplevel_phrase list
end

module Mapper : sig
  val structure :
    Ast_mapper.mapper -> Parsetree.structure -> Parsetree.structure

  val signature :
    Ast_mapper.mapper -> Parsetree.signature -> Parsetree.signature

  val use_file :
       Ast_mapper.mapper
    -> Parsetree.toplevel_phrase list
    -> Parsetree.toplevel_phrase list
end

module Printast : sig
  val implementation : Format.formatter -> Parsetree.structure -> unit

  val interface : Format.formatter -> Parsetree.signature -> unit

  val payload : int -> Format.formatter -> Parsetree.payload -> unit

  val expression : int -> Format.formatter -> Parsetree.expression -> unit

  val use_file : Format.formatter -> Parsetree.toplevel_phrase list -> unit
end

module Pprintast : sig
  val core_type : Format.formatter -> Parsetree.core_type -> unit

  val pattern : Format.formatter -> Parsetree.pattern -> unit

  val toplevel_phrase : Format.formatter -> Parsetree.toplevel_phrase -> unit

  val expression : Format.formatter -> Parsetree.expression -> unit

  val structure : Format.formatter -> Parsetree.structure -> unit

  val signature : Format.formatter -> Parsetree.signature -> unit
end
