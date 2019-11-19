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
  Source.t -> Cmts.t -> Conf.t * Conf.opts -> signature -> Fmt.t
(** Format a signature. *)

val fmt_structure :
  Source.t -> Cmts.t -> Conf.t * Conf.opts -> structure -> Fmt.t
(** Format a structure. *)

val fmt_toplevel :
  Source.t -> Cmts.t -> Conf.t * Conf.opts -> toplevel_phrase list -> Fmt.t
(** Format a toplevel structure. *)
