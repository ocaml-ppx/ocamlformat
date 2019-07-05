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

(** Opened in each source module to establish global namespace *)

include (
  Base :
    sig
      [@@@warning "-3"]

      include
        module type of Base
          (* [Filename], [Format], [Scanf] are all deprecated in [Base],
             erase them and use the ones from the stdlib. *)
          with module Filename := Base.Filename
           and module Format := Base.Format
           and module Scanf := Base.Scanf
    end )

include Option.Monad_infix
include Stdio

let ( >> ) f g x = g (f x)

let ( $ ) f g x = f x ; g x

let impossible msg = failwith msg

let user_error msg kvs =
  Error.raise_s (Sexp.message ("User Error: " ^ msg) kvs)

let check f x =
  assert (
    ignore (f x) ;
    true ) ;
  x

module Fpath = struct
  include Fpath

  let cwd () = Unix.getcwd () |> v

  let exists p = to_string p |> Caml.Sys.file_exists

  let to_absolute file = if is_rel file then append (cwd ()) file else file

  let to_string ?(pretty = false) p =
    if pretty then
      Option.value_map
        (relativize ~root:(cwd ()) p)
        ~default:(to_string p) ~f:to_string
    else to_string p

  let pp fmt p = Format.fprintf fmt "%s" (to_string ~pretty:true p)
end
