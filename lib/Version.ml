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

type t =
  | V0_10_0
  | V0_12_0
  | V0_14_2
  | V0_16_0
  | V0_17_0
  | V0_20_0
  | V0_21_0
  | V1_0_0

let to_string = function
  | V0_10_0 -> "0.10.0"
  | V0_12_0 -> "0.12.0"
  | V0_14_2 -> "0.14.2"
  | V0_16_0 -> "0.16.0"
  | V0_17_0 -> "0.17.0"
  | V0_20_0 -> "0.20.0"
  | V0_21_0 -> "0.21.0"
  | V1_0_0 -> "1.0.0"

let pp fs v = Format.fprintf fs "%s" (to_string v)

let current =
  let open Build_info.V1 in
  version () |> Option.value_map ~f:Version.to_string ~default:"unknown"
