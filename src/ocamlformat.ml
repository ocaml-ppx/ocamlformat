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

let normalize norm c {Translation_unit.ast; _} = norm c ast

let equal eq ~ignore_doc_comments c a b =
  eq ~ignore_doc_comments c a.Translation_unit.ast b.Translation_unit.ast

let moved_docstrings f c a b =
  f c a.Translation_unit.ast b.Translation_unit.ast

let parse = Migrate_ast.Parse.implementation

let format = Fmt_ast.fmt_structure

(** Operations on implementation files. *)
let impl : _ Translation_unit.t =
  { parse= Migrate_ast.Parse.implementation
  ; init_cmts= Cmts.init_impl ~parse ~format
  ; fmt= Fmt_ast.fmt_structure
  ; equal= equal Normalize.equal_impl
  ; moved_docstrings= moved_docstrings Normalize.moved_docstrings_impl
  ; normalize= normalize Normalize.impl
  ; printast= Migrate_ast.Printast.implementation }

(** Operations on interface files. *)
let intf : _ Translation_unit.t =
  { parse= Migrate_ast.Parse.interface
  ; init_cmts= Cmts.init_intf ~parse ~format
  ; fmt= Fmt_ast.fmt_signature
  ; equal= equal Normalize.equal_intf
  ; moved_docstrings= moved_docstrings Normalize.moved_docstrings_intf
  ; normalize= normalize Normalize.intf
  ; printast= Migrate_ast.Printast.interface }

(** Operations on use_file files. *)
let use_file : _ Translation_unit.t =
  { parse= Migrate_ast.Parse.use_file
  ; init_cmts= Cmts.init_use_file ~parse ~format
  ; fmt= Fmt_ast.fmt_use_file
  ; equal= equal Normalize.equal_use_file
  ; moved_docstrings= moved_docstrings Normalize.moved_docstrings_use_file
  ; normalize= normalize Normalize.use_file
  ; printast= Migrate_ast.Printast.use_file }

;;
Caml.at_exit (Format.pp_print_flush Format.err_formatter)

;;
Caml.at_exit (Format_.pp_print_flush Format_.err_formatter)

let format ~kind =
  match kind with
  | `Impl -> Translation_unit.parse_and_format impl
  | `Intf -> Translation_unit.parse_and_format intf
  | `Use_file -> Translation_unit.parse_and_format use_file

let to_output_file output_file data =
  match output_file with
  | None -> Out_channel.output_string Out_channel.stdout data
  | Some output_file -> Out_channel.write_all output_file ~data

;;
match Conf.action with
| Inplace inputs ->
    let output_file = None in
    let errors =
      List.filter_map inputs
        ~f:(fun {Conf.kind; name= input_name; file= input_file; conf} ->
          let source =
            In_channel.with_file input_file ~f:In_channel.input_all
          in
          let result =
            format conf ?output_file ~kind ~input_name ~source ()
          in
          match result with
          | Error _ -> Some ()
          | Ok formatted ->
              if String.equal formatted source then ()
              else Out_channel.write_all input_file ~data:formatted ;
              None)
    in
    if List.is_empty errors then Caml.exit 0 else Caml.exit 1
| In_out
    ( { kind= (`Impl | `Intf | `Use_file) as kind
      ; file= "-"
      ; name= input_name
      ; conf }
    , output_file ) -> (
    let source = In_channel.input_all In_channel.stdin in
    let result = format conf ?output_file ~kind ~input_name ~source () in
    match result with
    | Ok s ->
        to_output_file output_file s ;
        Caml.exit 0
    | Error _ -> Caml.exit 1 )
| In_out
    ( { kind= (`Impl | `Intf | `Use_file) as kind
      ; file= input_file
      ; name= input_name
      ; conf }
    , output_file ) -> (
    let source = In_channel.with_file input_file ~f:In_channel.input_all in
    let result = format conf ?output_file ~kind ~input_name ~source () in
    match result with
    | Ok s ->
        to_output_file output_file s ;
        Caml.exit 0
    | Error _ -> Caml.exit 1 )
| Check inputs ->
    let f {Conf.kind; name= input_name; file; conf} =
      let source = In_channel.with_file file ~f:In_channel.input_all in
      match format conf ~kind ~input_name ~source () with
      | Ok res -> String.equal res source
      | Error _ -> false
    in
    let checked = List.for_all inputs ~f in
    Caml.exit (if checked then 0 else 1)
