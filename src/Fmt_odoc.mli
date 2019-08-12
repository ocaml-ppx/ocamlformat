(**********************************************************************
 *                                                                    *
 *                            OCamlFormat                             *
 *                                                                    *
 *  Copyright (c) 2018-present, Facebook, Inc.  All rights reserved.  *
 *                                                                    *
 *  This source code is licensed under the MIT license found in the   *
 *  LICENSE file in the root directory of this source tree.           *
 *                                                                    *
 **********************************************************************)

type conf = {conf: Conf.t; fmt_code: string -> Fmt.t}
(** Internal state *)

val fmt : conf -> Odoc_parser.Ast.docs -> Fmt.t

val diff :
     Conf.t
  -> (string * Location.t) list
  -> (string * Location.t) list
  -> (string, string) Either.t Sequence.t
(** Difference between two lists of doc comments. *)

val is_tag_only : Odoc_parser.Ast.docs -> bool
(** [true] if the documentation only contains tags *)
