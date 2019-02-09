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
  | Invalid_source of {input_name: string; exn: exn}
  | Unstable of
      { input_name: string
      ; iteration: int
      ; prev: string
      ; next: string }
  | Ocamlformat_bug of {input_name: string; exn: exn}
  | User_error of string

val parse : (Lexing.lexbuf -> 'a) -> Conf.t -> string -> 'a with_comments

val format :
     'a t
  -> Conf.t
  -> ?dump_ast:(suffix:string -> (Formatter.t -> unit) -> unit)
  -> ?dump_formatted:(suffix:string -> string -> string option)
  -> input_name:string
  -> source:string
  -> parsed:('a with_comments, exn) Result.t
  -> unit
  -> (string, error) Result.t

val print_error :
     ?quiet_unstable:bool
  -> ?quiet_comments:bool
  -> ?quiet_doc_comments:bool
  -> Conf.t
  -> Formatter.t
  -> error
  -> unit
