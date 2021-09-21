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

let () =
  let open Yojson.Basic in
  let js_output =
    `Assoc [("name", `String "ocamlformat"); ("results", `List [])]
  in
  Format.fprintf Format.std_formatter "%a\n" pp js_output
