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

(** Configuration options *)

type t =
  { margin: int  (** Format code to fit within [margin] columns. *)
  ; sparse: bool  (** Generate more sparsely formatted code if true. *)
  ; max_iters: int
        (** Fail if output of formatting does not stabilize within
      [max_iters] iterations. *)
  ; escape_chars: [`Hexadecimal | `Minimal | `Octal]
        (** How to escape characters (literals and within strings). *)
  ; break_string_literals: [`Never | `New_lines]
        (** How to potentially break string literals into new lines. *) }

type 'a input = {kind: 'a; name: string; file: string; conf: t}

type action =
  | In_out of [`Impl | `Intf] input * string option
      (** Format input file of given kind to output file, or stdout if
          None. *)
  | Inplace of [`Impl | `Intf] input list
      (** Format in-place, overwriting input file(s). *)

val action : action
(** Formatting action: input type and source, and output destination. *)

val debug : bool
(** Generate debugging output if true. *)

val warn_error : bool
(** Treat warnings detected by the parser as errors. *)
