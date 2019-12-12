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

(** OCamlFormat to convert Reason code to OCaml *)

open Ocamlformat_lib

(** Operations on binary serialized Reason implementations. *)
let impl : _ Translation_unit.t =
  { init_cmts= Cmts.init_impl
  ; fmt= Fmt_ast.fmt_structure
  ; parse= Migrate_ast.Parse.implementation
  ; equal= Reason.equal_impl
  ; moved_docstrings= Reason.moved_docstrings_impl
  ; normalize= Reason.norm_impl
  ; printast= Migrate_ast.Printast.implementation }

(** Operations on binary serialized Reason interfaces. *)
let intf : _ Translation_unit.t =
  { init_cmts= Cmts.init_intf
  ; fmt= Fmt_ast.fmt_signature
  ; parse= Migrate_ast.Parse.interface
  ; equal= Reason.equal_intf
  ; moved_docstrings= Reason.moved_docstrings_intf
  ; normalize= Reason.norm_intf
  ; printast= Migrate_ast.Printast.interface }

let try_read_original_source filename =
  try In_channel.read_all filename with _ -> ""

type xunit =
  | Pack :
      {parse: In_channel.t -> 'a Reason.t; xunit: 'a Translation_unit.t}
      -> xunit

let pack_of_kind = function
  | `Impl -> Pack {parse= Reason.input_bin_impl; xunit= impl}
  | `Intf -> Pack {parse= Reason.input_bin_intf; xunit= intf}

let format xunit ?output_file ~input_name ~source ~parsed conf opts =
  Location.input_name := input_name ;
  if conf.Conf.disable then Ok source
  else
    Translation_unit.format xunit ?output_file ~input_name ~source ~parsed
      conf opts

let to_output_file output_file data =
  match output_file with
  | None -> Out_channel.output_string Out_channel.stdout data
  | Some output_file -> Out_channel.write_all output_file ~data

let run_action action opts =
  match action with
  | Conf.Inplace __ ->
      Error (fun () -> failwith "Cannot convert Reason code with --inplace")
  | Check _ ->
      Error (fun () -> failwith "Cannot check Reason code with --check")
  | In_out ({kind; file; name= input_name; conf}, output_file) -> (
      let (Pack {parse; xunit}) = pack_of_kind kind in
      let t =
        match file with
        | Stdin -> parse In_channel.stdin
        | File input_file -> In_channel.with_file input_file ~f:parse
      in
      let source = try_read_original_source t.origin_filename in
      let parsed = t.ast_and_comment in
      match
        format xunit ?output_file ~input_name ~source ~parsed conf opts
      with
      | Ok s ->
          to_output_file output_file s ;
          Ok ()
      | Error e ->
          Error
            (fun () ->
              Translation_unit.print_error ~debug:opts.debug
                ~quiet:conf.quiet ~input_name e) )
  | Print_config conf -> Conf.print_config conf ; Ok ()

;;
match Conf.action () with
| `Ok (action, opts) -> (
  match run_action action opts with
  | Ok () -> Caml.exit 0
  | Error error -> error () ; Caml.exit 1 )
| `Version | `Help -> Caml.exit 0
| `Error _ -> Caml.exit 1
