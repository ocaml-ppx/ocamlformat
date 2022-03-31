open Ast_helper
open Parsetree

module Ext = struct
  let mk () = (Location.mkloc "merlin.hole" !default_loc, PStr [])
end

module Exp = struct
  let mk () = Exp.extension (Ext.mk ())
end

module Cl = struct
  let mk () = Cl.extension (Ext.mk ())
end

module Cty = struct
  let mk () = Cty.extension (Ext.mk ())
end

module Pat = struct
  let mk () = Pat.extension (Ext.mk ())
end

module Mty = struct
  let mk () = Mty.extension (Ext.mk ())
end

module Mod = struct
  let mk () = Mod.extension (Ext.mk ())
end
