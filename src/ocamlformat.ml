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

(** Operations on implementation files. *)
let impl : _ Translation_unit.t =
  let parse = Translation_unit.parse Migrate_ast.Parse.implementation in
  { parse
  ; input= parse ?warn:None
  ; init_cmts= Cmts.init_impl
  ; fmt= Fmt_ast.fmt_structure
  ; equal= (fun (ast1, _) (ast2, _) -> Normalize.equal_impl ast1 ast2)
  ; normalize= (fun (ast, _) -> Normalize.impl ast)
  ; no_translation= List.is_empty
  ; printast= Migrate_ast.Printast.implementation }

(** Operations on interface files. *)
let intf : _ Translation_unit.t =
  let parse = Translation_unit.parse Migrate_ast.Parse.interface in
  { parse
  ; input= parse ?warn:None
  ; init_cmts= Cmts.init_intf
  ; fmt= Fmt_ast.fmt_signature
  ; equal= (fun (ast1, _) (ast2, _) -> Normalize.equal_intf ast1 ast2)
  ; normalize= (fun (ast, _) -> Normalize.intf ast)
  ; no_translation= List.is_empty
  ; printast= Migrate_ast.Printast.interface }

(** Operations on use_file files. *)
let use_file : _ Translation_unit.t =
  let use_file lexbuf =
    List.filter (Migrate_ast.Parse.use_file lexbuf) ~f:(function
      | Ast_407.Parsetree.Ptop_def [] -> false
      | Ast_407.Parsetree.Ptop_def (_ :: _) | Ast_407.Parsetree.Ptop_dir _ ->
          true )
  in
  let parse = Translation_unit.parse use_file in
  { parse
  ; input= parse ?warn:None
  ; init_cmts= Cmts.init_use_file
  ; fmt= Fmt_ast.fmt_use_file
  ; equal= (fun (ast1, _) (ast2, _) -> Normalize.equal_use_file ast1 ast2)
  ; normalize= (fun (ast, _) -> Normalize.use_file ast)
  ; no_translation= List.is_empty
  ; printast= Migrate_ast.Printast.use_file }

(** Select translation unit type and operations based on kind. *)
let xunit_of_kind : _ -> Translation_unit.x = function
  | `Impl -> XUnit impl
  | `Intf -> XUnit intf
  | `Use_file -> XUnit use_file

;; Caml.at_exit (Format.pp_print_flush Format.err_formatter)

;; Caml.at_exit (Format_.pp_print_flush Format_.err_formatter)

;; match Conf.action with
   | Inplace inputs ->
       List.iter inputs ~f:
         (fun {Conf.kind; name= input_name; file= input_file; conf} ->
           In_channel.with_file input_file ~f:(fun ic ->
               Translation_unit.parse_print (xunit_of_kind kind) conf
                 ~input_name ~input_file ic (Some input_file) ) )
   | In_out
       ( { kind= (`Impl | `Intf | `Use_file) as kind
         ; file= input_file
         ; name= input_name
         ; conf }
       , output_file ) ->
       In_channel.with_file input_file ~f:(fun ic ->
           Translation_unit.parse_print (xunit_of_kind kind) conf
             ~input_name ~input_file ic output_file )
