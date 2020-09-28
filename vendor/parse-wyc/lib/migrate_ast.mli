module Selected_version = Migrate_parsetree.Ast_408
module Ast_mapper = Selected_version.Ast_mapper
module Parsetree = Selected_version.Parsetree
module Asttypes = Selected_version.Asttypes

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
