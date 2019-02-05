(**********************************************************************
 *                                                                    *
 *                            OCamlFormat                             *
 *                                                                    *
 *  Copyright (c) 2018-present, Facebook, Inc.  All rights reserved.  *
 *                                                                    *
 *  This source code is licensed under the MIT license found in the   *
 *  LICENSE file in the root directory of this source tree.           *
 *                                                                    *
 **********************************************************************)

val fmt : Octavius.Types.t -> Fmt.t

val diff :
     Conf.t
  -> (string * Location.t) list
  -> (string * Location.t) list
  -> (string, string) Either.t Sequence.t
(** Difference between two lists of doc comments. *)
