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

exception Warning50 of (Location.t * Warnings.t) list

val parse :
     ?disable_w50:bool
  -> ?disable_deprecated:bool
  -> 'a Extended_ast.t
  -> Conf.t
  -> input_name:string
  -> source:string
  -> 'a with_comments
(** Parse source with warning handling, hash-bang detection, and token
    collection. For paired fragment types, also parses with the standard
    parser. *)

val parse_toplevel :
     ?disable_w50:bool
  -> ?disable_deprecated:bool
  -> Conf.t
  -> input_name:string
  -> source:string
  -> ( ( Extended_ast.use_file
       , Extended_ast.Std_parsetree.toplevel_phrase list )
       Extended_ast.paired
       with_comments
     , Extended_ast.repl_file with_comments )
     Either.t
(** Parse source as toplevel phrases or REPL phrases depending on content. *)
