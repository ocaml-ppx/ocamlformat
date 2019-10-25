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

let with_warning_filter ~filter ~f =
  let warning_printer = !Location.warning_printer in
  (Location.warning_printer :=
     fun loc fmt warn ->
       if filter loc warn then warning_printer loc fmt warn else ()) ;
  let reset () = Location.warning_printer := warning_printer in
  try
    let x = f () in
    reset () ; x
  with e -> reset () ; raise e

let print_warning l w =
  !Location.warning_printer l Caml.Format.err_formatter w

let make_printf = CamlinternalFormat.make_printf

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

module Int = struct
  let to_string = string_of_int
end
