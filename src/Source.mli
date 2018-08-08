(**********************************************************************
 *                                                                    *
 *                            OCamlFormat                             *
 *                                                                    *
 *  Copyright (c) 2017-present, Facebook, Inc.  All rights reserved.  *
 *                                                                    *
 *  This source code is licensed under the MIT license found in the   *
 *  LICENSE file in the root directory of this source tree.           *
 *                                                                    *
 **********************************************************************)

open Migrate_ast

type t

val create : string -> t

val string_between : t -> Location.t -> Location.t -> string option

val string_literal :
  t -> [`Normalize_nl | `Preserve] -> Location.t -> string

val char_literal : t -> Location.t -> string

val tokens_at :
     t
  -> ?filter:(Parser.token -> bool)
  -> Location.t
  -> (Parser.token * Location.t) list

val begins_line : t -> Location.t -> bool

val ends_line : t -> Location.t -> bool

val extension_using_sugar :
  name:string Location.loc -> payload:Parsetree.expression -> bool
