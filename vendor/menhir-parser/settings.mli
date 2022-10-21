(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(** This module parses the command line. *)

(** The file name extension that is expected for sources files. *)
val extension: string

(** Whether tracing instructions should be generated. *)
val trace: bool

type infer_mode =
  (* Perform no type inference. This is the default mode. *)
  | IMNone
  (* Perform type inference by invoking ocamlc directly. *)
  | IMInfer                (** --infer *)
  | IMDependRaw            (** --raw-depend *)
  | IMDependPostprocess    (** --depend *)
  (* Perform type inference by writing a mock .ml file and reading the
     corresponding inferred .mli file. *)
  | IMWriteQuery of string (** --infer-write-query <filename> *)
  | IMReadReply of string  (** --infer-read-reply <filename> *)

(** Whether and how OCaml type inference (for semantic actions and nonterminal
    symbols) should be performed. See the manual for details. *)
val infer: infer_mode

(** This undocumented flag suppresses prefixing of identifiers with an
    unlikely prefix in the generated code. This increases the code's
    readability, but can cause identifiers in semantic actions to be
    captured. *)
val noprefix: bool

(** How verbose we should be. *)

val logG: int (** diagnostics on the grammar *)

val logA: int (** diagnostics on the automaton *)

val logC: int (** diagnostics on the generated code *)

(** The filename of the standard library. *)
val stdlib_filename : string

(** Whether unresolved LR(1) conflicts, useless precedence declarations,
   productions that are never reduced, etc. should be treated as errors. *)
val strict: bool

(** This flag tells whether [$i] notation in semantic actions is allowed. *)
type dollars =
  | DollarsDisallowed
  | DollarsAllowed

(** This flag tells whether [$i] notation in semantic actions is allowed. *)
val dollars: dollars
