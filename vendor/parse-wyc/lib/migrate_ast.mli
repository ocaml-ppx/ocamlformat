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
  type 'a fragment =
    | Structure : Parsetree.structure fragment
    | Signature : Parsetree.signature fragment
    | Use_file : Parsetree.toplevel_phrase list fragment

  val map_ast : 'a fragment -> Ast_mapper.mapper -> 'a -> 'a
end
