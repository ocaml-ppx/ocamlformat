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

val init : string -> unit

val string_between : Location.t -> Location.t -> string option

val string_literal : [`Normalize_nl | `Preserve] -> Location.t -> string

val char_literal : Location.t -> string

val string_at : Location.t -> string

val begins_line : Location.t -> bool

val ends_line : Location.t -> bool

val sub : pos:int -> len:int -> string
