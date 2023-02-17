module Versionned_conf = Versionned_conf

module For_external_use = struct
  let parse_and_format ?(version = Version.current) syntax ?output_file
      ~input_name ~source conf =
    let open Result.O in
    let* m = Versionned_conf.m_of_version version in
    let module Ocamlformat = (val m : Ocamlformat_version.S) in
    Ocamlformat.Translation_unit.parse_and_format syntax ?output_file
      ~input_name ~source conf

  let numeric ?(version = Version.current) syntax ~input_name ~source conf =
    let numeric =
      match Versionned_conf.m_of_version version with
      | Error _ -> Ocamlformat_lib_latest.Translation_unit.numeric
      | Ok m ->
          let module Ocamlformat = (val m : Ocamlformat_version.S) in
          Ocamlformat.Translation_unit.numeric
    in
    numeric syntax ~input_name ~source conf

  let print_error ?(version = Version.current) ?debug ?quiet fmt err =
    match Versionned_conf.m_of_version version with
    | Error err ->
        Ocamlformat_lib_latest.Translation_unit.print_error ?debug ?quiet fmt
          err
    | Ok m ->
        let module Ocamlformat = (val m : Ocamlformat_version.S) in
        Ocamlformat.Translation_unit.print_error ?debug ?quiet fmt err
end

open Versionned_conf

let parse_and_format syntax ?output_file ~input_name ~source vconf =
  let module Ocamlformat = (val vconf.version_module : Ocamlformat_version.S)
  in
  Ocamlformat.Translation_unit.parse_and_format syntax ?output_file
    ~input_name ~source vconf.conf

let numeric syntax ~input_name ~source vconf =
  let module Ocamlformat = (val vconf.version_module : Ocamlformat_version.S)
  in
  Ocamlformat.Translation_unit.numeric syntax ~input_name ~source vconf.conf

let print_error ?debug ?quiet vconf fmt err =
  let module Ocamlformat = (val vconf.version_module : Ocamlformat_version.S)
  in
  Ocamlformat.Translation_unit.print_error ?debug ?quiet fmt err
