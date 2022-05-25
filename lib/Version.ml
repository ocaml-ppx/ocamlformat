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

type t = {major: int; minor: int; patch: int option}

let make ~major ~minor ~patch = {major; minor; patch}

let to_string = function
  | {major; minor; patch= None} -> Format.sprintf "%i.%i" major minor
  | {major; minor; patch= Some patch} ->
      Format.sprintf "%i.%i.%i" major minor patch

let pp fs v = Format.fprintf fs "%s" (to_string v)

let current =
  let open Build_info.V1 in
  version () |> Option.value_map ~f:Version.to_string ~default:"unknown"
