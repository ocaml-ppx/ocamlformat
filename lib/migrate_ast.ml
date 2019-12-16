let selected_version = Migrate_parsetree.Versions.ocaml_408

module Selected_version = Migrate_parsetree.Ast_408
module Ast_mapper = Selected_version.Ast_mapper
module Ast_helper = Selected_version.Ast_helper
module Parsetree = Selected_version.Parsetree

module Asttypes = Selected_version.Asttypes

module Docstrings = struct
  include Selected_version.Docstrings
  open Parsetree
  open Asttypes

  type let_binding =
    { lb_pattern: pattern
    ; lb_expression: expression
    ; lb_attributes: attributes
    ; lb_docs: docs Lazy.t
    ; lb_text: text Lazy.t
    ; lb_loc: Location.t }

  type let_bindings =
    { lbs_bindings: let_binding list
    ; lbs_rec: rec_flag
    ; lbs_extension: string Asttypes.loc option
    ; lbs_loc: Location.t }
end

module Parse = struct
  open Migrate_parsetree

  let implementation = Parse.implementation selected_version

  let interface = Parse.interface selected_version

  let use_file lexbuf =
    List.filter
      (fun (p : Parsetree.toplevel_phrase) ->
        match p with
        | Ptop_def [] -> false
        | Ptop_def (_ :: _) | Ptop_dir _ -> true)
      (Parse.use_file selected_version lexbuf)
end

let to_current =
  Migrate_parsetree.Versions.(migrate selected_version ocaml_current)

module Printast = struct
  open Printast

  let implementation f x = implementation f (to_current.copy_structure x)

  let interface f x = interface f (to_current.copy_signature x)

  let expression n f x = expression n f (to_current.copy_expression x)

  let payload n f (x : Parsetree.payload) =
    payload n f
      ( match x with
      | PStr x -> PStr (to_current.copy_structure x)
      | PSig x -> PSig (to_current.copy_signature x)
      | PTyp x -> PTyp (to_current.copy_core_type x)
      | PPat (x, y) ->
          PPat
            ( to_current.copy_pattern x
            , Option.map to_current.copy_expression y ) )

  let use_file f (x : Parsetree.toplevel_phrase list) =
    List.iter (fun (p : Parsetree.toplevel_phrase) ->
        top_phrase f (to_current.copy_toplevel_phrase p)) x
end

module Pprintast = struct
  open Pprintast

  let structure f x = structure f (to_current.copy_structure x)

  let signature f x = signature f (to_current.copy_signature x)

  let core_type f x = core_type f (to_current.copy_core_type x)

  let expression f x = expression f (to_current.copy_expression x)

  let pattern f x = pattern f (to_current.copy_pattern x)

  let toplevel_phrase f x =
    toplevel_phrase f (to_current.copy_toplevel_phrase x)
end

module Location = Selected_version.Location
