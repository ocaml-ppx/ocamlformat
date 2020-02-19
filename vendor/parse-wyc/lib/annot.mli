module type Annotated = sig
  type t

  val mk : unit -> t

  val is_generated : t -> bool
end

open Migrate_ast.Parsetree

module Exp : Annotated with type t = expression

module Attr : Annotated with type t = attribute

module Class_exp : Annotated with type t = class_expr
