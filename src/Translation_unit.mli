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
  { init_cmts:
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

type error =
  | Invalid_source of {exn: exn}
  | Unstable of {iteration: int; prev: string; next: string}
  | Ocamlformat_bug of {exn: exn}
  | User_error of string

val parse :
  (Lexing.lexbuf -> 'a) -> Conf.t -> source:string -> 'a with_comments

val format :
     'a t
  -> Conf.t
  -> ?output_file:string
  -> input_name:string
  -> source:string
  -> parsed:('a with_comments, exn) Result.t
  -> unit
  -> (string, error) Result.t
(** [format xunit conf ?output_file ~input_name ~source ~parsed ()] format
    [parsed], using [input_name] for error messages, and referring to
    [source] to improve comment placement. It returns the formatted string
    or an error that prevented formatting. *)

val parse_and_format :
     'a t
  -> Conf.t
  -> ?output_file:string
  -> input_name:string
  -> source:string
  -> unit
  -> (string, error) Result.t
(** [parse_and_format xunit conf ?output_file ~input_name ~source ()]
    Similar to [format] but parses the source according to [xunit]. *)
