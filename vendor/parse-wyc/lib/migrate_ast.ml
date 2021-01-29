module Selected_version = Migrate_parsetree.Ast_412
module Parsetree = Selected_version.Parsetree
module Asttypes = Selected_version.Asttypes

module Mapper = struct
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

  let fold_ast (type o p) (fragment:(o, p) fragment)
        (m:_ Ppxlib.Ast_traverse.fold) init (x:p) =
    match fragment with
    | Structure -> m#structure x init
    | Signature -> m#signature x init
    | Use_file -> List.fold_left (fun acc tlp -> m#toplevel_phrase tlp acc) init x

  let to_ppxlib (type o p) (f:(o, p) fragment) : o -> p =
    let module Conv = Ppxlib_ast.Select_ast (Ppxlib_ast__.Versions.OCaml_412) in
    let module To_ppxlib = Conv.Of_ocaml in
    match f with
    | Structure -> To_ppxlib.copy_structure
    | Signature -> To_ppxlib.copy_signature
    | Use_file -> List.map To_ppxlib.copy_toplevel_phrase
end
