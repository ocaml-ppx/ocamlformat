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

val ast : 'a Std_ast.t -> erase_jane_syntax:bool -> Conf.t -> 'a -> 'a
(** Normalize an AST fragment. If [erase_jane_syntax] is true, remove all
    [Jane_syntax] attributes signaling erasable syntax. *)

val equal :
     'a Std_ast.t
  -> ignore_doc_comments:bool
  -> erase_jane_syntax:bool
  -> Conf.t
  -> old:'a
  -> new_:'a
  -> bool
(** Compare fragments for equality up to normalization. If
    [erase_jane_syntax] is true, first removes all [Jane_syntax] attributes
    signaling erasable syntax from the [old] AST fragment; the [new_] AST
    fragment should already omit them. *)

val moved_docstrings :
     'a Std_ast.t
  -> erase_jane_syntax:bool
  -> Conf.t
  -> old:'a
  -> new_:'a
  -> Cmt.error list
