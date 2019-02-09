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

val format :
     Conf.t
  -> kind:[`Impl | `Intf | `Use_file]
  -> ?dump_ast:(suffix:string -> (Formatter.t -> unit) -> unit)
  -> ?dump_formatted:(suffix:string -> string -> string option)
  -> input_name:string
  -> source:string
  -> unit
  -> (string, Translation_unit.error) Result.t
