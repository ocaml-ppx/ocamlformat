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

let normalize norm {Translation_unit.ast; _} = norm ast

let equal eq ~ignore_doc_comments a b =
  eq ~ignore_doc_comments a.Translation_unit.ast b.Translation_unit.ast

(** Operations on implementation files. *)
let impl : _ Translation_unit.t =
  let parse = Migrate_ast.Parse.implementation in
  { input= Translation_unit.parse parse
  ; parse
  ; init_cmts= Cmts.init_impl
  ; fmt= Fmt_ast.fmt_structure
  ; equal= equal Normalize.equal_impl
  ; normalize= normalize Normalize.impl
  ; printast= Migrate_ast.Printast.implementation }

(** Operations on interface files. *)
let intf : _ Translation_unit.t =
  let parse = Migrate_ast.Parse.interface in
  { input= Translation_unit.parse parse
  ; parse
  ; init_cmts= Cmts.init_intf
  ; fmt= Fmt_ast.fmt_signature
  ; equal= equal Normalize.equal_intf
  ; normalize= normalize Normalize.intf
  ; printast= Migrate_ast.Printast.interface }

(** Operations on use_file files. *)
let use_file : _ Translation_unit.t =
  let parse = Migrate_ast.Parse.use_file in
  { input= Translation_unit.parse parse
  ; parse
  ; init_cmts= Cmts.init_use_file
  ; fmt= Fmt_ast.fmt_use_file
  ; equal= equal Normalize.equal_use_file
  ; normalize= normalize Normalize.use_file
  ; printast= Migrate_ast.Printast.use_file }

(** Select translation unit type and operations based on kind. *)
let xunit_of_kind : _ -> Translation_unit.x = function
  | `Impl -> XUnit impl
  | `Intf -> XUnit intf
  | `Use_file -> XUnit use_file

;;
Caml.at_exit (Format.pp_print_flush Format.err_formatter)

;;
Caml.at_exit (Format_.pp_print_flush Format_.err_formatter)

;;
match Conf.action with
| Inplace inputs ->
    let results : Translation_unit.result list =
      List.map inputs
        ~f:(fun {Conf.kind; name= input_name; file= input_file; conf} ->
          In_channel.with_file input_file ~f:(fun ic ->
              Translation_unit.parse_print (xunit_of_kind kind) conf
                ~input_name ~input_file ic (Some input_file) ) )
    in
    if
      List.for_all results ~f:(fun result ->
          match (result : Translation_unit.result) with
          | Ok -> true
          | Unstable _ | Ocamlformat_bug _ | Invalid_source _ -> false )
    then Caml.exit 0
    else Caml.exit 1
| In_out
    ( { kind= (`Impl | `Intf | `Use_file) as kind
      ; file= input_file
      ; name= input_name
      ; conf }
    , output_file ) -> (
  match
    In_channel.with_file input_file ~f:(fun ic ->
        Translation_unit.parse_print (xunit_of_kind kind) conf ~input_name
          ~input_file ic output_file )
  with
  | Ok -> Caml.exit 0
  | Unstable _ | Ocamlformat_bug _ | Invalid_source _ -> Caml.exit 1 )
