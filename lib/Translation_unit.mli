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

(** Operations on translation units. *)
type 'a t

type error

val parse_and_format :
     'a t
  -> ?output_file:string
  -> input_name:string
  -> source:string
  -> Conf.t
  -> Conf.opts
  -> (string, error) Result.t
(** [parse_and_format xunit conf ?output_file ~input_name ~source] is similar
    to [format] but parses the source according to [xunit]. *)

val print_error :
     ?fmt:Format.formatter
  -> debug:bool
  -> quiet:bool
  -> input_name:string
  -> error
  -> unit
(** [print_error conf ?fmt ~input_name e] prints the error message
    corresponding to error [e] on the [fmt] formatter (stderr by default). *)

val impl : Migrate_ast.Parsetree.toplevel_phrase list t

val intf : Migrate_ast.Parsetree.signature t
