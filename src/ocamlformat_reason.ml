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

let try_read_original_source filename =
  try In_channel.read_all filename with _ -> ""

open Ocamlformat_helper

type xunit =
  | Pack :
      { parse: In_channel.t -> 'a Reason.t
      ; print: (module Reason_api.Formattable with type t = 'a) }
      -> xunit

let pack_of_kind = function
  | `Impl ->
      Pack {parse= Reason.input_bin_impl; print= (module Reason_api.Impl)}
  | `Intf ->
      Pack {parse= Reason.input_bin_intf; print= (module Reason_api.Intf)}

;;
match Conf.action with
| In_out ({kind= `Use_file; _}, _) ->
    user_error "Cannot convert Reason code with --use-file" []
| Inplace _ -> user_error "Cannot convert Reason code with --inplace" []
| In_out
    ( {kind= (`Impl | `Intf) as kind; file= "-"; name= input_name; conf}
    , output_file ) -> (
    let result =
      let (Pack {parse; print= (module Print)}) = pack_of_kind kind in
      let t = parse In_channel.stdin in
      let source = try_read_original_source t.origin_filename in
      let dump_ast = dump_ast ?output_file ~input_name in
      let dump_formatted = dump_formatted ?output_file ~input_name in
      Print.format conf ~dump_ast ~dump_formatted ~input_name ~source
        ~parsed:t.ast_and_comment ()
    in
    match result with
    | Ok s ->
        to_output_file output_file s ;
        Caml.exit 0
    | Error err -> print_error conf err ; Caml.exit 1 )
| In_out
    ( { kind= (`Impl | `Intf) as kind
      ; name= input_name
      ; file= input_file
      ; conf }
    , output_file ) -> (
    let result =
      let (Pack {parse; print= (module Print)}) = pack_of_kind kind in
      let t = In_channel.with_file input_file ~f:parse in
      let source = try_read_original_source t.origin_filename in
      let dump_ast = dump_ast ?output_file ~input_name in
      let dump_formatted = dump_formatted ?output_file ~input_name in
      Print.format conf ~dump_ast ~dump_formatted ~input_name ~source
        ~parsed:t.ast_and_comment ()
    in
    match result with
    | Ok s ->
        to_output_file output_file s ;
        Caml.exit 0
    | Error err -> print_error conf err ; Caml.exit 1 )
