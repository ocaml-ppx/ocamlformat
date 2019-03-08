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

(** Operations on binary serialized Reason implementations. *)
let impl : _ Translation_unit.t =
  let parse = Migrate_ast.Parse.implementation in
  { init_cmts= Cmts.init_impl
  ; fmt= Fmt_ast.fmt_structure
  ; parse
  ; equal= Reason.equal_impl
  ; moved_docstrings= Reason.moved_docstrings_impl
  ; normalize= Reason.norm_impl
  ; printast= Migrate_ast.Printast.implementation }

(** Operations on binary serialized Reason interfaces. *)
let intf : _ Translation_unit.t =
  let parse = Migrate_ast.Parse.interface in
  { init_cmts= Cmts.init_intf
  ; fmt= Fmt_ast.fmt_signature
  ; parse
  ; equal= Reason.equal_intf
  ; moved_docstrings= Reason.moved_docstrings_intf
  ; normalize= Reason.norm_intf
  ; printast= Migrate_ast.Printast.interface }

let try_read_original_source filename =
  try In_channel.read_all filename with _ -> ""

type xunit =
  | Pack :
      { parse: In_channel.t -> 'a Reason.t
      ; xunit: 'a Translation_unit.t }
      -> xunit

let pack_of_kind = function
  | `Impl -> Pack {parse= Reason.input_bin_impl; xunit= impl}
  | `Intf -> Pack {parse= Reason.input_bin_intf; xunit= intf}

let format xunit conf ?output_file ~input_name ~source ~parsed () =
  Location.input_name := input_name ;
  let parsed = Ok parsed in
  Translation_unit.format xunit conf ?output_file ~input_name ~source
    ~parsed ()

let to_output_file output_file data =
  match output_file with
  | None -> Out_channel.output_string Out_channel.stdout data
  | Some output_file -> Out_channel.write_all output_file ~data

;;
match Conf.action with
| In_out ({kind= `Use_file; _}, _) ->
    user_error "Cannot convert Reason code with --use-file" []
| Inplace _ -> user_error "Cannot convert Reason code with --inplace" []
| In_out
    ( {kind= (`Impl | `Intf) as kind; file= "-"; name= input_name; conf}
    , output_file ) -> (
    let result =
      let (Pack {parse; xunit}) = pack_of_kind kind in
      let t = parse In_channel.stdin in
      let source = try_read_original_source t.origin_filename in
      format xunit conf ?output_file ~input_name ~source
        ~parsed:t.ast_and_comment ()
    in
    match result with
    | Ok s ->
        to_output_file output_file s ;
        Caml.exit 0
    | Error _ -> Caml.exit 1 )
| In_out
    ( { kind= (`Impl | `Intf) as kind
      ; name= input_name
      ; file= input_file
      ; conf }
    , output_file ) -> (
    let result =
      let (Pack {parse; xunit}) = pack_of_kind kind in
      let t = In_channel.with_file input_file ~f:parse in
      let source = try_read_original_source t.origin_filename in
      format xunit conf ?output_file ~input_name ~source
        ~parsed:t.ast_and_comment ()
    in
    match result with
    | Ok s ->
        to_output_file output_file s ;
        Caml.exit 0
    | Error _ -> Caml.exit 1 )
| Check inputs ->
    let output_file = None in
    let checked =
      List.for_all inputs
        ~f:(fun {Conf.kind; name= input_name; file= input_file; conf} ->
          match kind with
          | `Use_file ->
              user_error "Cannot check Reason formatting with --use-file" []
          | (`Impl | `Intf) as kind -> (
              let (Pack {parse; xunit}) = pack_of_kind kind in
              let t = In_channel.with_file input_file ~f:parse in
              let source = try_read_original_source t.origin_filename in
              let result =
                format xunit conf ?output_file ~input_name ~source
                  ~parsed:t.ast_and_comment ()
              in
              match result with
              | Ok res -> String.equal res source
              | Error _ -> false ) )
    in
    Caml.exit (if checked then 0 else 1)
