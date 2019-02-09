(**********************************************************************
 *                                                                    *
 *                            OCamlFormat                             *
 *                                                                    *
 *  Copyright (c) 2019-present, Facebook, Inc.  All rights reserved.  *
 *                                                                    *
 *  This source code is licensed under the MIT license found in the   *
 *  LICENSE file in the root directory of this source tree.           *
 *                                                                    *
 **********************************************************************)

module type Formattable = sig
  type t

  val format :
       Conf.t
    -> ?dump_ast:(suffix:string -> (Formatter.t -> unit) -> unit)
    -> ?dump_formatted:(suffix:string -> string -> string option)
    -> input_name:string
    -> source:string
    -> parsed:t Translation_unit.with_comments
    -> unit
    -> (string, Translation_unit.error) Result.t
end

module Impl : Formattable with type t = Migrate_ast.Parsetree.structure

module Intf : Formattable with type t = Migrate_ast.Parsetree.signature
