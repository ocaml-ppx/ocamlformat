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
  { input= Reason.input_impl
  ; init_cmts= Cmts.init_impl
  ; fmt= Fmt_ast.fmt_structure
  ; parse
  ; equal= Reason.equal_impl
  ; normalize= Reason.norm_impl
  ; no_translation= Fn.const false
  ; printast= Migrate_ast.Printast.implementation }

(** Operations on binary serialized Reason interfaces. *)
let reason_intf : _ Translation_unit.t =
  let parse = Migrate_ast.Parse.interface in
  { input= Reason.input_intf
  ; init_cmts= Cmts.init_intf
  ; fmt= Fmt_ast.fmt_signature
  ; parse
  ; equal= Reason.equal_intf
  ; normalize= Reason.norm_intf
  ; no_translation= Fn.const false
  ; printast= Migrate_ast.Printast.interface }

(** Select translation unit type and operations based on kind. *)
let xunit_of_kind : _ -> Translation_unit.x = function
  | `Impl -> XUnit reason_impl
  | `Intf -> XUnit reason_intf

;; match Conf.action with
   | In_out
       ( { kind= (`Impl | `Intf) as kind
         ; name= input_name
         ; file= input_file
         ; conf }
       , output_file ) ->
       Translation_unit.parse_print (xunit_of_kind kind) conf ~input_name
         ~input_file In_channel.stdin output_file
   | Inplace _ -> user_error "Cannot convert Reason code with --inplace" []
