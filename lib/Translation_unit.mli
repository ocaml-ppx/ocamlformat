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

type error =
  | Invalid_source of {exn: exn}
  | Unstable of {iteration: int; prev: string; next: string}
  | Ocamlformat_bug of {exn: exn}
  | User_error of string

val parse_and_format :
     _ list Migrate_ast.Traverse.fragment
  -> ?output_file:string
  -> input_name:string
  -> source:string
  -> Conf.t
  -> Conf.opts
  -> (string, error) Result.t
(** [parse_and_format conf ?output_file ~input_name ~source] parses and
    formats [source] as a list of fragments. *)

val indentation :
     _ list Migrate_ast.Traverse.fragment
  -> input_name:string
  -> source:string
  -> range:int * int
  -> Conf.t
  -> Conf.opts
  -> (int list, error) Result.t
(** [indentation ~input_name ~source ~range conf opts] returns the
    indentation of the range of lines [range] (line numbers ranging from 1 to
    number of lines), where the line numbers are relative to [source] and the
    indentation is relative to the formatted output. *)

val print_error :
     fmt:Format.formatter
  -> exe:string
  -> debug:bool
  -> quiet:bool
  -> input_name:string
  -> error
  -> unit
(** [print_error conf ?fmt ~input_name e] prints the error message
    corresponding to error [e] on the [fmt] formatter (stderr by default). *)
