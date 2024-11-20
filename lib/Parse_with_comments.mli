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

type 'a with_comments =
  {ast: 'a; comments: Cmt.t list; prefix: string; source: Source.t}

module W : sig
  type t

  val in_lexer : int list

  val disable : int -> t

  val enable : int -> t

  val to_string : t list -> string
end

exception Warning50 of (Location.t * Warnings.t) list

val parse :
     ?disable_w50:bool
  -> ?disable_deprecated:bool
  -> (   'b
      -> ocaml_version:Ocaml_version.t
      -> input_name:string
      -> string
      -> 'a )
  -> 'b
  -> Conf.t
  -> input_name:string
  -> source:string
  -> 'a with_comments
(** @raise [Warning50] on misplaced documentation comments. *)

val parse_toplevel :
     ?disable_w50:bool
  -> ?disable_deprecated:bool
  -> Conf.t
  -> input_name:string
  -> source:string
  -> ( Extended_ast.use_file with_comments
     , Extended_ast.repl_file with_comments )
     Either.t
(** Variant of {!parse} that uses {!Extended_ast.Parse.toplevel}. This
    function handles [conf.parse_toplevel_phrases]. *)

val parse_ast :
     Conf.t
  -> 'a Extended_ast.t
  -> ocaml_version:Ocaml_version.t
  -> input_name:string
  -> string
  -> 'a
(** Argument to {!parse}. *)
