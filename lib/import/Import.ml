(**************************************************************************)
(*                                                                        *)
(*                              OCamlFormat                               *)
(*                                                                        *)
(*            Copyright (c) Facebook, Inc. and its affiliates.            *)
(*                                                                        *)
(*      This source code is licensed under the MIT license found in       *)
(*      the LICENSE file in the root directory of this source tree.       *)
(*                                                                        *)
(**************************************************************************)

(** Opened in each source module to establish global namespace *)

include Base
include Stdio
include Compat
module Format = Caml.Format
module Filename = Caml.Filename

let ( >> ) f g x = g (f x)

let impossible msg = failwith msg

module Fpath = struct
  include Fpath

  let cwd () = Unix.getcwd () |> v

  let exists p = to_string p |> Caml.Sys.file_exists

  let to_absolute file = if is_rel file then append (cwd ()) file else file

  let to_string ?(relativize = false) p =
    if relativize then
      Option.value_map
        (Fpath.relativize ~root:(cwd ()) p)
        ~default:(to_string p) ~f:to_string
    else to_string p

  let pp fmt p = Format.fprintf fmt "%s" (to_string ~relativize:true p)
end
