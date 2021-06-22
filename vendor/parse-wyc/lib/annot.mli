module Exp : sig
  val mk : unit -> Parsetree.expression
end

module Cl : sig
  val mk : unit -> Parsetree.class_expr
end

module Cty : sig
  val mk : unit -> Parsetree.class_type
end

module Pat : sig
  val mk : unit -> Parsetree.pattern
end

module Mty : sig
  val mk : unit -> Parsetree.module_type
end

module Mod : sig
  val mk : unit -> Parsetree.module_expr
end
