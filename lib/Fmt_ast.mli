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
module Format = Format_

(** Format OCaml Ast *)

open Migrate_ast
open Parsetree

val fmt_signature :
  debug:bool -> Source.t -> Cmts.t -> Conf.t -> signature -> Fmt.t
(** Format a signature. *)

val fmt_structure :
  debug:bool -> Source.t -> Cmts.t -> Conf.t -> structure -> Fmt.t
(** Format a structure. *)

val fmt_toplevel :
  debug:bool -> Source.t -> Cmts.t -> Conf.t -> toplevel_phrase list -> Fmt.t
(** Format a toplevel structure. *)
