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

module VConf = VConf

module For_external_use = struct
  let parse_and_format ?(version = Version.current) syntax ?output_file
      ~input_name ~source conf =
    let open Result.O in
    let* m = VConf.module_of_version version in
    let module V = (val m : VConf.Ocamlformat_interface) in
    V.parse_and_format syntax ?output_file ~input_name ~source conf

  let numeric ?(version = Version.current) syntax ~input_name ~source conf =
    let numeric =
      match VConf.module_of_version version with
      | Error _ -> Ocamlformat_lib_latest.numeric
      | Ok m ->
          let module V = (val m : VConf.Ocamlformat_interface) in
          V.numeric
    in
    numeric syntax ~input_name ~source conf

  let print_error ?(version = Version.current) ?debug ?quiet fmt err =
    match VConf.module_of_version version with
    | Error err -> Ocamlformat_lib_latest.print_error ?debug ?quiet fmt err
    | Ok m ->
        let module V = (val m : VConf.Ocamlformat_interface) in
        V.print_error ?debug ?quiet fmt err
end

open VConf

let parse_and_format syntax ?output_file ~input_name ~source vconf =
  let module V = (val vconf.version_module : VConf.Ocamlformat_interface) in
  V.parse_and_format syntax ?output_file ~input_name ~source vconf.conf

let numeric syntax ~input_name ~source vconf =
  let module V = (val vconf.version_module : VConf.Ocamlformat_interface) in
  V.numeric syntax ~input_name ~source vconf.conf

let print_error ?debug ?quiet vconf fmt err =
  let module V = (val vconf.version_module : VConf.Ocamlformat_interface) in
  V.print_error ?debug ?quiet fmt err
