module Selected_version = Migrate_parsetree.Ast_412
module Parsetree = Selected_version.Parsetree
module Asttypes = Selected_version.Asttypes

module Mapper : sig
  type ('omp, 'ppxlib) fragment =
    | Structure
        : ( Selected_version.Parsetree.structure,
            Ppxlib.Parsetree.structure )
          fragment
    | Signature
        : ( Selected_version.Parsetree.signature,
            Ppxlib.Parsetree.signature )
          fragment
    | Use_file
        : ( Selected_version.Parsetree.toplevel_phrase list,
            Ppxlib.Parsetree.toplevel_phrase list )
          fragment

  val fold_ast : (_, 'ppxlib) fragment -> 'a Ppxlib.Ast_traverse.fold -> 'a -> 'ppxlib -> 'a

  val to_ppxlib : ('omp, 'ppxlib) fragment -> 'omp -> 'ppxlib
end
