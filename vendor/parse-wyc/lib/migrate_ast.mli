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

module Location : sig
  include module type of Selected_version.Location

  val compare : t -> t -> int

  val merge : t -> t -> t option
end

module Mapper : sig
  val structure :
    Ast_mapper.mapper -> Parsetree.structure -> Parsetree.structure

  val signature :
    Ast_mapper.mapper -> Parsetree.signature -> Parsetree.signature

  val use_file :
    Ast_mapper.mapper ->
    Parsetree.toplevel_phrase list ->
    Parsetree.toplevel_phrase list
end

module Parse : sig
  val implementation : Lexing.lexbuf -> Parsetree.structure

  val interface : Lexing.lexbuf -> Parsetree.signature

  val use_file : Lexing.lexbuf -> Parsetree.toplevel_phrase list
end
