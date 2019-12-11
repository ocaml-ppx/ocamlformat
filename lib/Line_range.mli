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

type t

val all : t

val only_between : low:int -> high:int -> t

val pp_hum : Format.formatter -> t -> unit

val apply :
     t
  -> f:(string -> (string, 'e) Result.t)
  -> on_invalid:'e
  -> string
  -> (string, 'e) Result.t
