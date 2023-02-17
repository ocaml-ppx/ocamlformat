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

let ( = ) v1 v2 =
  match (v1, v2) with
  | {major; minor; patch}, {major= major'; minor= minor'; patch= patch'} ->
      major = major' && minor = minor' && Option.equal ( = ) patch patch'

let of_string s =
  match String.split s ~on:'.' with
  | [major; minor] ->
      let open Option.O in
      let* major = Stdlib.int_of_string_opt major in
      let+ minor = Stdlib.int_of_string_opt minor in
      {major; minor; patch= None}
  | [major; minor; patch] ->
      let open Option.O in
      let* major = Stdlib.int_of_string_opt major in
      let* minor = Stdlib.int_of_string_opt minor in
      let+ patch = Stdlib.int_of_string_opt patch in
      {major; minor; patch= Some patch}
  | _ -> None

let equals_string v s =
  match of_string s with Some v' -> v = v' | None -> false

let to_string = function
  | {major; minor; patch= None} -> Format.sprintf "%i.%i" major minor
  | {major; minor; patch= Some patch} ->
      Format.sprintf "%i.%i.%i" major minor patch

let pp fs v = Format.fprintf fs "%s" (to_string v)

let current = {major= 0; minor= 24; patch= Some 1}

let all = [current]

let available v = List.exists all ~f:((=) v)

let dune_build_info =
  let open Build_info.V1 in
  version () |> Option.value_map ~f:Version.to_string ~default:"unknown"
