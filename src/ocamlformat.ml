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

;;
Caml.at_exit (Format.pp_print_flush Format.err_formatter)

;;
Caml.at_exit (Format_.pp_print_flush Format_.err_formatter)

open Ocamlformat_helper

;;
match Conf.action with
| Inplace inputs ->
    let results =
      List.filter_map inputs
        ~f:(fun {Conf.kind; name= input_name; file= input_file; conf} ->
          let source =
            In_channel.with_file input_file ~f:In_channel.input_all
          in
          let result =
            let output_file = None in
            let dump_ast = dump_ast ?output_file ~input_name in
            let dump_formatted = dump_formatted ?output_file ~input_name in
            Ocaml_api.format conf ~kind ~dump_ast ~dump_formatted
              ~input_name ~source ()
          in
          match result with
          | Error err -> print_error conf err ; Some ()
          | Ok formatted ->
              if String.equal formatted source then ()
              else Out_channel.write_all input_file ~data:formatted ;
              None )
    in
    if List.is_empty results then Caml.exit 0 else Caml.exit 1
| In_out
    ( { kind= (`Impl | `Intf | `Use_file) as kind
      ; file= "-"
      ; name= input_name
      ; conf }
    , output_file ) -> (
    let result =
      let dump_ast = dump_ast ?output_file ~input_name in
      let dump_formatted = dump_formatted ?output_file ~input_name in
      let source = In_channel.input_all In_channel.stdin in
      Ocaml_api.format conf ~kind ~dump_ast ~dump_formatted ~input_name
        ~source ()
    in
    match result with
    | Ok s ->
        to_output_file output_file s ;
        Caml.exit 0
    | Error err -> print_error conf err ; Caml.exit 1 )
| In_out
    ( { kind= (`Impl | `Intf | `Use_file) as kind
      ; file= input_file
      ; name= input_name
      ; conf }
    , output_file ) -> (
    let result =
      let dump_ast = dump_ast ?output_file ~input_name in
      let dump_formatted = dump_formatted ?output_file ~input_name in
      let source =
        In_channel.with_file input_file ~f:In_channel.input_all
      in
      Ocaml_api.format conf ~kind ~dump_ast ~dump_formatted ~input_name
        ~source ()
    in
    match result with
    | Ok s ->
        to_output_file output_file s ;
        Caml.exit 0
    | Error err -> print_error conf err ; Caml.exit 1 )
