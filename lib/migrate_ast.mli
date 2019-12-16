val selected_version :
  Migrate_parsetree.Versions.OCaml_408.types
  Migrate_parsetree.Versions.ocaml_version

module Selected_version = Migrate_parsetree.Ast_408
module Ast_mapper = Selected_version.Ast_mapper
module Ast_helper = Selected_version.Ast_helper
module Parsetree = Selected_version.Parsetree
module Asttypes = Selected_version.Asttypes

module Docstrings : sig
  include module type of Selected_version.Docstrings

  type let_binding = {
    lb_pattern : Parsetree.pattern;
    lb_expression : Parsetree.expression;
    lb_attributes : Parsetree.attributes;
    lb_docs : docs lazy_t;
    lb_text : text lazy_t;
    lb_loc : Location.t;
  }
  type let_bindings = {
    lbs_bindings : let_binding list;
    lbs_rec : Asttypes.rec_flag;
    lbs_extension : string Location.loc option;
    lbs_loc : Location.t;
  }
end

module Location = Selected_version.Location

module Parse : sig
  val implementation : Lexing.lexbuf -> Parsetree.structure

  val interface : Lexing.lexbuf -> Parsetree.signature

  val use_file : Lexing.lexbuf -> Parsetree.toplevel_phrase list
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
