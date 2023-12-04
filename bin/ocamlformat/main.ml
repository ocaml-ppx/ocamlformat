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

(** OCamlFormat *)

open Ocamlformat_lib
open Ocamlformat_format ;;

Stdlib.at_exit (Format.pp_print_flush Format.err_formatter) ;;

Stdlib.at_exit (Format_.pp_print_flush Format_.err_formatter)

let format ?output_file ~kind ~input_name ~source (conf : Conf.t) =
  if conf.opr_opts.disable.v then Ok source
  else
    Translation_unit.parse_and_format kind ?output_file ~input_name ~source
      conf

let write_all output_file ~data =
  Out_channel.with_file ~binary:true output_file ~f:(fun oc ->
      Out_channel.output_string oc data )

let to_output_file output_file data =
  match output_file with
  | None ->
      Out_channel.flush Out_channel.stdout ;
      Out_channel.set_binary_mode Out_channel.stdout true ;
      Out_channel.output_string Out_channel.stdout data ;
      Out_channel.flush Out_channel.stdout ;
      Out_channel.set_binary_mode Out_channel.stdout false
  | Some output_file -> write_all output_file ~data

let source_from_file = function
  | Bin_conf.Stdin -> In_channel.input_all In_channel.stdin
  | File f -> In_channel.with_file f ~f:In_channel.input_all

let print_error (conf : Conf.t) e =
  Translation_unit.Error.print Format.err_formatter
    ~debug:conf.opr_opts.debug.v ~quiet:conf.opr_opts.quiet.v e

let run_action action =
  match action with
  | Bin_conf.Inplace inputs ->
      let f {Bin_conf.kind; name= input_name; file= input_file; conf} =
        let input_file =
          match input_file with
          | File f -> f
          | _ -> impossible "checked by validate"
        in
        let source =
          In_channel.with_file input_file ~f:In_channel.input_all
        in
        let result = format ~kind ~input_name ~source conf in
        match result with
        | Ok formatted ->
            if not (String.equal formatted source) then
              write_all input_file ~data:formatted ;
            Ok ()
        | Error e -> Error (fun () -> print_error conf e)
      in
      Result.combine_errors_unit (List.map inputs ~f)
  | In_out ({kind; file; name= input_name; conf}, output_file) -> (
      let source = source_from_file file in
      match format ?output_file ~kind ~input_name ~source conf with
      | Ok s ->
          to_output_file output_file s ;
          Ok ()
      | Error e -> Error [(fun () -> print_error conf e)] )
  | Check inputs ->
      let f {Bin_conf.kind; name= input_name; file; conf} =
        let source = source_from_file file in
        let result = format ~kind ~input_name ~source conf in
        match result with
        | Ok res when String.equal res source -> Ok ()
        | Ok _ -> Error (fun () -> ())
        | Error e -> Error (fun () -> print_error conf e)
      in
      Result.combine_errors_unit (List.map inputs ~f)
  | Print_config conf -> Conf.print_config conf ; Ok ()
;;

match Bin_conf.action () with
| Ok (`Ok action) -> (
  match run_action action with
  | Ok () -> Stdlib.exit 0
  | Error errors ->
      List.iter errors ~f:(fun error -> error ()) ;
      Stdlib.exit 1 )
| Ok (`Version | `Help) -> Stdlib.exit 0
| Error _ -> Stdlib.exit 1
