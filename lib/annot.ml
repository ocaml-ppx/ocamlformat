open Migrate_ast.Ast_helper
open Migrate_ast.Parsetree

module Exp = struct
  let mk () =
    let loc = !default_loc in
    let id = Location.mkloc "merlin.hole" loc in
    Exp.mk ~loc (Pexp_extension (id, PStr []))

  let is_generated e =
    match e.pexp_desc with
    | Pexp_extension ({ txt = "merlin.hole"; _ }, PStr []) -> true
    | _ -> false
end

module Attr = struct
  let mk () = Attr.mk { txt = "merlin.hole.gen"; loc = Location.none } (PStr [])

  let is_generated a =
    match (a.attr_name.txt, a.attr_payload) with
    | "merlin.hole.gen", PStr [] -> true
    | _ -> false
end
