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
let reason_impl : _ Translation_unit.t =
  let parse = Migrate_ast.Parse.implementation in
  { input= (fun conf ~input_file:_ ic -> Reason.input_impl conf ic)
  ; init_cmts= Cmts.init_impl
  ; fmt= Fmt_ast.fmt_structure
  ; parse
  ; equal= Reason.equal_impl
  ; moved_docstrings= Reason.moved_docstrings_impl
  ; normalize= Reason.norm_impl
  ; printast= Migrate_ast.Printast.implementation }

(** Operations on binary serialized Reason interfaces. *)
let reason_intf : _ Translation_unit.t =
  let parse = Migrate_ast.Parse.interface in
  { input= (fun conf ~input_file:_ ic -> Reason.input_intf conf ic)
  ; init_cmts= Cmts.init_intf
  ; fmt= Fmt_ast.fmt_signature
  ; parse
  ; equal= Reason.equal_intf
  ; moved_docstrings= Reason.moved_docstrings_intf
  ; normalize= Reason.norm_intf
  ; printast= Migrate_ast.Printast.interface }

(** Select translation unit type and operations based on kind. *)
let xunit_of_kind : _ -> Translation_unit.x = function
  | `Impl -> XUnit reason_impl
  | `Intf -> XUnit reason_intf

;;
match Conf.action with
| In_out
    ( {kind= (`Impl | `Intf) as kind; file= "-"; name= input_name; conf}
    , output_file ) ->
    Translation_unit.parse_print (xunit_of_kind kind) conf ~input_name
      ~input_file:"" stdin output_file
| In_out
    ( { kind= (`Impl | `Intf) as kind
      ; name= input_name
      ; file= input_file
      ; conf }
    , output_file ) ->
    In_channel.with_file input_file ~f:(fun ic ->
        Translation_unit.parse_print (xunit_of_kind kind) conf ~input_name
          ~input_file:"" ic output_file )
| In_out ({kind= `Use_file; _}, _) ->
    user_error "Cannot convert Reason code with --use-file" []
| Inplace _ -> user_error "Cannot convert Reason code with --inplace" []
