let selected_version = Migrate_parsetree.Versions.ocaml_408

module Selected_version = Migrate_parsetree.Ast_408
module Ast_mapper = Selected_version.Ast_mapper
module Ast_helper = Selected_version.Ast_helper
module Parsetree = Selected_version.Parsetree
module Asttypes = Selected_version.Asttypes

module Mapper = struct
  let structure = Selected_version.map_structure

  let signature = Selected_version.map_signature

  (* Missing from ocaml_migrate_parsetree *)
  let use_file (mapper : Ast_mapper.mapper) use_file =
    let open Parsetree in
    List.map
      (fun toplevel_phrase ->
        match (toplevel_phrase : toplevel_phrase) with
        | Ptop_def structure ->
            Ptop_def (mapper.Ast_mapper.structure mapper structure)
        | Ptop_dir { pdir_name; pdir_arg; pdir_loc } ->
            let pdir_arg =
              match pdir_arg with
              | None -> None
              | Some a ->
                  Some { a with pdira_loc = mapper.location mapper a.pdira_loc }
            in
            Ptop_dir
              {
                pdir_name =
                  { pdir_name with loc = mapper.location mapper pdir_name.loc };
                pdir_arg;
                pdir_loc = mapper.location mapper pdir_loc;
              } )
      use_file
end

module Docstrings = struct
  include Selected_version.Docstrings
  open Parsetree
  open Asttypes

  type let_binding = {
    lb_pattern : pattern;
    lb_expression : expression;
    lb_attributes : attributes;
    lb_docs : docs Lazy.t;
    lb_text : text Lazy.t;
    lb_loc : Location.t;
  }

  type let_bindings = {
    lbs_bindings : let_binding list;
    lbs_rec : rec_flag;
    lbs_extension : string Asttypes.loc option;
    lbs_loc : Location.t;
  }
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
        | Ptop_def (_ :: _) | Ptop_dir _ -> true )
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
      | PPat (x, Some y) ->
          PPat (to_current.copy_pattern x, Some (to_current.copy_expression y))
      | PPat (x, None) -> PPat (to_current.copy_pattern x, None) )

  let use_file f (x : Parsetree.toplevel_phrase list) =
    List.iter
      (fun (p : Parsetree.toplevel_phrase) ->
        top_phrase f (to_current.copy_toplevel_phrase p) )
      x
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

module Int = struct
  let compare x y = if x < y then -1 else if x > y then 1 else 0
end

module Position = struct
  open Lexing

  let compare p1 p2 = Int.compare p1.pos_cnum p2.pos_cnum
end

module Location = struct
  include Selected_version.Location

  let compare_start x y = Position.compare x.loc_start y.loc_start

  let compare_end x y = Position.compare x.loc_end y.loc_end

  let compare x y =
    let st = compare_start x y in
    if st = 0 then compare_end x y else st

  let merge x y =
    if Position.compare x.loc_end y.loc_start >= 0 then
      Some { x with loc_end = y.loc_end }
    else None
end
