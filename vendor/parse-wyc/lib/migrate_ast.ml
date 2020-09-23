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
              })
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
