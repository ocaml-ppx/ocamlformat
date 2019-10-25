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

let () = Clflags.error_style := Some Misc.Error_style.Short

let with_warning_filter ~filter ~f =
  let warning_reporter = !Location.warning_reporter in
  (Location.warning_reporter :=
     fun loc warn ->
       if filter loc warn then Location.default_warning_reporter loc warn
       else None) ;
  let reset () = Location.warning_reporter := warning_reporter in
  try
    let x = f () in
    reset () ; x
  with e -> reset () ; raise e

let print_warning l w =
  Location.default_warning_reporter l w
  |> Option.iter ~f:(Location.print_report Caml.Format.err_formatter)

let make_printf f x acc fmt = CamlinternalFormat.make_printf (f x) acc fmt

open Caml

module Stack = struct
  include Stack

  let top_opt st = try Some (top st) with Stack.Empty -> None

  let pop_opt st = try Some (pop st) with Stack.Empty -> None
end

module Queue = struct
  include Queue

  let take_opt q = try Some (take q) with Queue.Empty -> None

  let peek_opt q = try Some (peek q) with Queue.Empty -> None
end

module Int = Int
