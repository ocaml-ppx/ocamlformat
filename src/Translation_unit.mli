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
type 'a t =
  { input: string -> In_channel.t -> 'a * (string * Location.t) list
  ; init_cmts: string -> 'a -> (string * Location.t) list -> unit
  ; fmt: Conf.t -> 'a -> Fmt.t
  ; parse:
      ?warn:bool -> string -> string -> In_channel.t
      -> 'a * (string * Location.t) list
  ; equal:
      'a * (string * Location.t) list -> 'a * (string * Location.t) list
      -> bool
  ; normalize: 'a * (string * Location.t) list -> 'a
  ; printast: Caml.Format.formatter -> 'a -> unit }

(** Existential package of a type of translation unit and its operations. *)
type x = XUnit: 'a t -> x

val parse :
  (Lexing.lexbuf -> 'a) -> ?warn:bool -> string -> string -> In_channel.t
  -> 'a * (string * Location.t) list
(** [parse parse_ast ~warn input_name input_file input_channel] parses the
    contents of [input_channel] assuming it corresponds to [input_name] for
    the purposes of error reporting and to the contents of [input_file] for
    the purposes of comment placement. Actual parsing is done by
    [parse_ast]. If [warn] is set, then parse-time warnings are fatal,
    otherwise only enabled. *)

val parse_print :
  x -> Conf.t -> string -> string -> In_channel.t -> string option -> unit
(** [parse_print xunit conf input_name input_file input_channel output_file]
    parses the contents of [input_channel], using [input_name] for error
    messages, and referring to the contents of [input_file] to improve
    comment placement, and writes formatted code into [output_file]. *)
