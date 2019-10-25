(**********************************************************************
 *                                                                    *
 *                            OCamlFormat                             *
 *                                                                    *
 *  Copyright (c) 2019-present, Facebook, Inc.  All rights reserved.  *
 *                                                                    *
 *  This source code is licensed under the MIT license found in the   *
 *  LICENSE file in the root directory of this source tree.           *
 *                                                                    *
 **********************************************************************)

val with_warning_filter :
  filter:(Location.t -> Warnings.t -> bool) -> f:(unit -> 'a) -> 'a

val print_warning : Location.t -> Warnings.t -> unit

val make_printf :
     ('b -> ('b, 'c) CamlinternalFormat.acc -> 'd)
  -> 'b
  -> ('b, 'c) CamlinternalFormat.acc
  -> ('a, 'b, 'c, 'c, 'c, 'd) CamlinternalFormatBasics.fmt
  -> 'a
