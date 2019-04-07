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

let setup_warning_filter f =
  let warning_printer = !Location.warning_printer in
  (Location.warning_printer :=
     fun loc fmt warn ->
       if f loc warn then warning_printer loc fmt warn else ()) ;
  `Reset (fun () -> Location.warning_printer := warning_printer)

let print_warning l w =
  !Location.warning_printer l Caml.Format.err_formatter w
