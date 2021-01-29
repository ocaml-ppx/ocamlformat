module Exp : sig
  val mk : unit -> Migrate_ast.Parsetree.expression
end

module Cl : sig
  val mk : unit -> Migrate_ast.Parsetree.class_expr
end

module Cty : sig
  val mk : unit -> Migrate_ast.Parsetree.class_type
end

module Pat : sig
  val mk : unit -> Migrate_ast.Parsetree.pattern
end

module Mty : sig
  val mk : unit -> Migrate_ast.Parsetree.module_type
end

module Mod : sig
  val mk : unit -> Migrate_ast.Parsetree.module_expr
end
