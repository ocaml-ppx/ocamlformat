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

module Format = Format_
open Migrate_ast

module T = struct
  type t = string * Location.t

  let compare =
    Comparable.lexicographic
      [ Comparable.lift String.compare ~f:fst
      ; Comparable.lift Location.compare ~f:snd ]

  let sexp_of_t (txt, loc) =
    Sexp.Atom (Format.asprintf "%s %a" txt Migrate_ast.Location.fmt loc)
end

include T
include Comparator.Make (T)

let map ~f (txt, loc) = (f txt, loc)
