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

val string_at : t -> Location.t -> string

val string_literal :
  t -> [`Normalize_nl | `Preserve] -> Location.t -> string

val char_literal : t -> Location.t -> string

val tokens_at :
     t
  -> ?filter:(Parser.token -> bool)
  -> Location.t
  -> (Parser.token * Location.t) list

val loc_between : from:Location.t -> upto:Location.t -> Location.t
(** [loc_between ~from ~upto] returns a location starting from [from] and
    ending before [upto]. *)

val tokens_between :
     t
  -> ?filter:(Parser.token -> bool)
  -> from:Location.t
  -> upto:Location.t
  -> (Parser.token * Location.t) list
(** [tokens_between s ~filter ~from ~upto] returns the list of tokens
    starting from [from] and ending before [upto] and respecting the
    [filter] property. [from] must start before [upto]. *)

val contains_IN_token_between :
  t -> from:Location.t -> upto:Location.t -> bool
(** [contains_IN_token_between s ~from ~upto] returns [true] if and only if
    an IN token is contained in the tokens starting from [from] and ending
    before [upto]. [from] must start before [upto]. *)

val begins_line : t -> Location.t -> bool

val ends_line : t -> Location.t -> bool

val extension_using_sugar :
  name:string Location.loc -> payload:Parsetree.expression -> bool

val extend_loc_to_include_attributes :
  t -> Location.t -> Parsetree.attributes -> Location.t
