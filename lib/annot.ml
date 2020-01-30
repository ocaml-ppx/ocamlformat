open Migrate_ast.Ast_helper
open Migrate_ast.Parsetree

module type Annotated = sig
  type t

  val mk : unit -> t

  val is_generated : t -> bool
end

module Ext = struct
  let mk () = (Location.mkloc "merlin.hole" !default_loc, PStr [])

  let is_generated = function
    | (({ txt = "merlin.hole"; _ }, PStr []) : extension) -> true
    | _ -> false
end

module Exp = struct
  type t = expression

  let mk () = Exp.extension (Ext.mk ())

  let is_generated e =
    match e.pexp_desc with
    | Pexp_extension ext when Ext.is_generated ext -> true
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

  let mk () = Cl.extension (Ext.mk ())

  let is_generated e =
    match e.pcl_desc with
    | Pcl_extension ext when Ext.is_generated ext -> true
    | _ -> false
end
