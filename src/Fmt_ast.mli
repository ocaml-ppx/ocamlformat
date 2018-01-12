(**********************************************************************)
(*                                                                    *)
(*                            OCamlFormat                             *)
(*                                                                    *)
(*  Copyright (c) 2017-present, Facebook, Inc.  All rights reserved.  *)
(*                                                                    *)
(*  This source code is licensed under the MIT license found in the   *)
(*  LICENSE file in the root directory of this source tree.           *)
(*                                                                    *)
(**********************************************************************)

module Format = Format_

(** Format OCaml Ast *)

open Migrate_ast
open Parsetree

exception Formatting_disabled
    (** Raise this exception to Indicate that formatting has been disabled for the
        current file *)

val fmt_signature : Conf.t -> signature -> Fmt.t
(** Format a signature. *)

val fmt_structure : Conf.t -> structure -> Fmt.t
(** Format a structure. *)
