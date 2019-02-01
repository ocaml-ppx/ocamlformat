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

type 'a with_comments =
  {ast: 'a; comments: (string * Location.t) list; prefix: string}

(** Operations on translation units. *)
type 'a t =
  { input: Conf.t -> In_channel.t -> 'a with_comments
  ; init_cmts:
      Source.t -> Conf.t -> 'a -> (string * Location.t) list -> Cmts.t
  ; fmt: Source.t -> Cmts.t -> Conf.t -> 'a -> Fmt.t
  ; parse: Lexing.lexbuf -> 'a
  ; equal:
         ignore_doc_comments:bool
      -> Conf.t
      -> 'a with_comments
      -> 'a with_comments
      -> bool
  ; moved_docstrings:
         Conf.t
      -> 'a with_comments
      -> 'a with_comments
      -> Normalize.docstring_error list
  ; normalize: Conf.t -> 'a with_comments -> 'a
  ; printast: Caml.Format.formatter -> 'a -> unit }

(** Existential package of a type of translation unit and its operations. *)
type x = XUnit : 'a t -> x

exception Warning50 of (Location.t * Warnings.t) list

type result =
  | Ok
  | Invalid_source of exn
  | Unstable of int
  | Ocamlformat_bug of exn
  | User_error of string

val parse :
  (Lexing.lexbuf -> 'a) -> Conf.t -> In_channel.t -> 'a with_comments

val parse_print :
     x
  -> Conf.t
  -> input_name:string
  -> input_file:string
  -> In_channel.t
  -> string option
  -> result
(** [parse_print xunit conf input_name input_file input_channel output_file]
    parses the contents of [input_channel], using [input_name] for error
    messages, and referring to the contents of [input_file] to improve
    comment placement, and writes formatted code into [output_file]. *)
