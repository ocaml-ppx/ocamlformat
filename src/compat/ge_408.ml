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

let _ = Clflags.error_style := Some Misc.Error_style.Short

let setup_warning_filter f =
  let warning_reporter = !Location.warning_reporter in
  (Location.warning_reporter :=
     fun loc warn ->
       if f loc warn then Location.default_warning_reporter loc warn
       else None) ;
  `Reset (fun () -> Location.warning_reporter := warning_reporter)

let print_warning l w =
  Location.default_warning_reporter l w
  |> Option.iter ~f:(Location.print_report Caml.Format.err_formatter)
