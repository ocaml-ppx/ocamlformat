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

type error

val parse_and_format_impl :
     ?output_file:string
  -> input_name:string
  -> source:string
  -> Conf.t
  -> Conf.opts
  -> (string, error) Result.t
(** [parse_and_format_impl conf ?output_file ~input_name ~source] parses and
    formats [source] as a list of toplevel phrases. *)

val parse_and_format_intf :
     ?output_file:string
  -> input_name:string
  -> source:string
  -> Conf.t
  -> Conf.opts
  -> (string, error) Result.t
(** [parse_and_format_intf conf ?output_file ~input_name ~source] parses and
    formats [source] as a list of signature items. *)

val print_error :
     ?fmt:Format.formatter
  -> debug:bool
  -> quiet:bool
  -> input_name:string
  -> error
  -> unit
(** [print_error conf ?fmt ~input_name e] prints the error message
    corresponding to error [e] on the [fmt] formatter (stderr by default). *)
