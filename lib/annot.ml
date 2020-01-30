open Migrate_ast.Ast_helper
open Migrate_ast.Parsetree

module type Annotated = sig
  type t

  val mk : unit -> t

  val is_generated : t -> bool
end

module Exp = struct
  type t = expression

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
  type t = attribute

  let mk () = Attr.mk { txt = "merlin.hole.gen"; loc = Location.none } (PStr [])

  let is_generated a =
    match (a.attr_name.txt, a.attr_payload) with
    | "merlin.hole.gen", PStr [] -> true
    | _ -> false
end

module Class_exp = struct
  type t = class_expr

  let mk () =
    let loc = !default_loc in
    let id = Location.mkloc "merlin.hole" loc in
    Cl.mk ~loc (Pcl_extension (id, PStr []))

  let is_generated e =
    match e.pcl_desc with
    | Pcl_extension ({ txt = "merlin.hole"; _ }, PStr []) -> true
    | _ -> false
end
