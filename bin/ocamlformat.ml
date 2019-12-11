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

(** OCamlFormat *)

open Ocamlformat_lib

let normalize norm c {Parse_with_comments.ast; _} = norm c ast

let equal eq ~ignore_doc_comments c a b =
  eq ~ignore_doc_comments c a.Parse_with_comments.ast
    b.Parse_with_comments.ast

let moved_docstrings f c a b =
  f c a.Parse_with_comments.ast b.Parse_with_comments.ast

(** Operations on implementation files. *)
let impl : _ Translation_unit.t =
  { parse= Migrate_ast.Parse.use_file
  ; init_cmts= Cmts.init_toplevel
  ; fmt= Fmt_ast.fmt_toplevel
  ; equal= equal Normalize.equal_toplevel
  ; moved_docstrings= moved_docstrings Normalize.moved_docstrings_toplevel
  ; normalize= normalize Normalize.toplevel
  ; printast= Migrate_ast.Printast.use_file }

(** Operations on interface files. *)
let intf : _ Translation_unit.t =
  { parse= Migrate_ast.Parse.interface
  ; init_cmts= Cmts.init_intf
  ; fmt= Fmt_ast.fmt_signature
  ; equal= equal Normalize.equal_intf
  ; moved_docstrings= moved_docstrings Normalize.moved_docstrings_intf
  ; normalize= normalize Normalize.intf
  ; printast= Migrate_ast.Printast.interface }

;;
Caml.at_exit (Format.pp_print_flush Format.err_formatter)

;;
Caml.at_exit (Format_.pp_print_flush Format_.err_formatter)

let format ?output_file ~kind ~input_name ~source conf opts =
  if conf.Conf.disable then Ok source
  else
    let f =
      match kind with
      | `Impl -> Translation_unit.parse_and_format impl
      | `Intf -> Translation_unit.parse_and_format intf
    in
    f ?output_file ~input_name ~source conf opts

let to_output_file output_file data =
  match output_file with
  | None -> Out_channel.output_string Out_channel.stdout data
  | Some output_file -> Out_channel.write_all output_file ~data

let source_from_file = function
  | Conf.Stdin -> In_channel.input_all In_channel.stdin
  | File f -> In_channel.with_file f ~f:In_channel.input_all

let print_error conf opts ~input_name e =
  Translation_unit.print_error ~debug:opts.Conf.debug ~quiet:conf.Conf.quiet
    ~input_name e

let run_action action opts =
  match action with
  | Conf.Inplace inputs ->
      let f {Conf.kind; name= input_name; file= input_file; conf} =
        let input_file =
          match input_file with
          | File f -> f
          | _ -> impossible "checked by validate"
        in
        let source =
          In_channel.with_file input_file ~f:In_channel.input_all
        in
        let result = format ~kind ~input_name ~source conf opts in
        match result with
        | Ok formatted ->
            if not (String.equal formatted source) then
              Out_channel.write_all input_file ~data:formatted ;
            Ok ()
        | Error e -> Error (fun () -> print_error conf opts ~input_name e)
      in
      Result.combine_errors_unit (List.map inputs ~f)
  | In_out ({kind; file; name= input_name; conf}, output_file) -> (
      let source = source_from_file file in
      match format ?output_file ~kind ~input_name ~source conf opts with
      | Ok s ->
          to_output_file output_file s ;
          Ok ()
      | Error e -> Error [(fun () -> print_error conf opts ~input_name e)] )
  | Check inputs ->
      let f {Conf.kind; name= input_name; file; conf} =
        let source = source_from_file file in
        let result = format ~kind ~input_name ~source conf opts in
        match result with
        | Ok res when String.equal res source -> Ok ()
        | Ok _ -> Error (fun () -> ())
        | Error e -> Error (fun () -> print_error conf opts ~input_name e)
      in
      Result.combine_errors_unit (List.map inputs ~f)
  | Print_config conf -> Conf.print_config conf ; Ok ()

;;
match Conf.action () with
| `Ok (action, opts) -> (
  match run_action action opts with
  | Ok () -> Caml.exit 0
  | Error errors ->
      List.iter errors ~f:(fun error -> error ()) ;
      Caml.exit 1 )
| `Version | `Help -> Caml.exit 0
| `Error _ -> Caml.exit 1
