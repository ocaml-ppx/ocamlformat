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

val build_config :
     enable_outside_detected_project:bool
  -> root:Fpath.t option
  -> file:string
  -> is_stdin:bool
  -> (Ocamlformat_lib.Conf.t, string) Result.t

type file = Stdin | File of string

type input =
  { kind: Ocamlformat_lib.Syntax.t
  ; name: string
  ; file: file
  ; conf: Ocamlformat_lib.Conf.t }

(** Formatting action: input type and source, and output destination. *)
type action =
  | In_out of input * string option
      (** Format input file (or [-] for stdin) of given kind to output file,
          or stdout if None. *)
  | Inplace of input list  (** Format in-place, overwriting input file(s). *)
  | Check of input list
      (** Check whether the input files already are formatted. *)
  | Print_config of Ocamlformat_lib.Conf.t
      (** Print the configuration and exit. *)

val action :
  unit -> (action Cmdliner.Cmd.eval_ok, Cmdliner.Cmd.eval_error) Result.t

open Ocamlformat_lib

type t =
  { lib_conf: Conf.t
  ; enable_outside_detected_project: bool
  ; inplace: bool
  ; check: bool
  ; kind: Syntax.t option
  ; inputs: file list
  ; name: string option
  ; output: string option
  ; print_config: bool
  ; root: string option
  ; disable_conf_files: bool
  ; ignore_invalid_options: bool
  ; ocp_indent_config: bool
  ; config: (string * string) list }

val global_conf : t ref
