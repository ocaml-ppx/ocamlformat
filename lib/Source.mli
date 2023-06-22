(**************************************************************************)
(*                                                                        *)
(*                              OCamlFormat                               *)
(*                                                                        *)
(*            Copyright (c) Facebook, Inc. and its affiliates.            *)
(*                                                                        *)
(*      This source code is licensed under the MIT license found in       *)
(*      the LICENSE file in the root directory of this source tree.       *)
(*                                                                        *)
(**************************************************************************)

open Extended_ast

type t

val create : text:string -> tokens:(Parser.token * Location.t) list -> t

val empty_line_between : t -> Lexing.position -> Lexing.position -> bool
(** [empty_line_between t p1 p2] is [true] if there is an empty line between
    [p1] and [p2]. The lines containing [p1] and [p2] are not considered
    empty. *)

val empty_line_before : t -> Location.t -> bool

val empty_line_after : t -> Location.t -> bool

val tokens_between :
     t
  -> filter:(Parser.token -> bool)
  -> Lexing.position
  -> Lexing.position
  -> (Parser.token * Location.t) list

val string_at : t -> Location.t -> string

val find_token_after :
     t
  -> filter:(Parser.token -> bool)
  -> Lexing.position
  -> (Parser.token * Location.t) option

val find_token_before :
     t
  -> filter:(Parser.token -> bool)
  -> Lexing.position
  -> (Parser.token * Location.t) option

val string_literal : t -> [`Normalize | `Preserve] -> Location.t -> string

val begins_line : ?ignore_spaces:bool -> t -> Location.t -> bool

val ends_line : t -> Location.t -> bool

val extension_using_sugar :
  name:string Location.loc -> payload:Location.t -> bool

val extend_loc_to_include_attributes : Location.t -> attributes -> Location.t

val type_constraint_is_first : core_type -> Location.t -> bool

val is_quoted_string : t -> Location.t -> bool

val loc_of_first_token_at :
  t -> Location.t -> Parser.token -> Location.t option

val find_first_token_on_line : t -> int -> (Parser.token * Location.t) option
