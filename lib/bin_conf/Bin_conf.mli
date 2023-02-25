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

open Ocamlformat_lib

val build_config :
     enable_outside_detected_project:bool
  -> root:Fpath.t option
  -> file:string
  -> is_stdin:bool
  -> (VConf.t, string) Result.t

type file = Stdin | File of string

type input =
  { kind: Syntax.t
  ; name: string
  ; file: file
  ; conf: VConf.t }

(** Formatting action: input type and source, and output destination. *)
type action =
  | In_out of input * string option
      (** Format input file (or [-] for stdin) of given kind to output file,
          or stdout if None. *)
  | Inplace of input list  (** Format in-place, overwriting input file(s). *)
  | Check of input list
      (** Check whether the input files already are formatted. *)
  | Print_config of VConf.t
      (** Print the configuration and exit. *)
  | Numeric of input

val action :
  unit -> (action Cmdliner.Cmd.eval_ok, Cmdliner.Cmd.eval_error) Result.t
