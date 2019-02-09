(**********************************************************************
 *                                                                    *
 *                            OCamlFormat                             *
 *                                                                    *
 *  Copyright (c) 2019-present, Facebook, Inc.  All rights reserved.  *
 *                                                                    *
 *  This source code is licensed under the MIT license found in the   *
 *  LICENSE file in the root directory of this source tree.           *
 *                                                                    *
 **********************************************************************)

let normalize norm c {Translation_unit.ast; _} = norm c ast

let equal eq ~ignore_doc_comments c a b =
  eq ~ignore_doc_comments c a.Translation_unit.ast b.Translation_unit.ast

let moved_docstrings f c a b =
  f c a.Translation_unit.ast b.Translation_unit.ast

(** Operations on implementation files. *)
let impl : _ Translation_unit.t =
  let parse = Migrate_ast.Parse.implementation in
  { parse
  ; init_cmts= Cmts.init_impl
  ; fmt= Fmt_ast.fmt_structure
  ; equal= equal Normalize.equal_impl
  ; moved_docstrings= moved_docstrings Normalize.moved_docstrings_impl
  ; normalize= normalize Normalize.impl
  ; printast= Migrate_ast.Printast.implementation }

(** Operations on interface files. *)
let intf : _ Translation_unit.t =
  let parse = Migrate_ast.Parse.interface in
  { parse
  ; init_cmts= Cmts.init_intf
  ; fmt= Fmt_ast.fmt_signature
  ; equal= equal Normalize.equal_intf
  ; moved_docstrings= moved_docstrings Normalize.moved_docstrings_intf
  ; normalize= normalize Normalize.intf
  ; printast= Migrate_ast.Printast.interface }

(** Operations on use_file files. *)
let use_file : _ Translation_unit.t =
  let parse = Migrate_ast.Parse.use_file in
  { parse
  ; init_cmts= Cmts.init_use_file
  ; fmt= Fmt_ast.fmt_use_file
  ; equal= equal Normalize.equal_use_file
  ; moved_docstrings= moved_docstrings Normalize.moved_docstrings_use_file
  ; normalize= normalize Normalize.use_file
  ; printast= Migrate_ast.Printast.use_file }

let dummy_dump_ast ~suffix:_ _ = ()

let dummy_dump_formatted ~suffix:_ _ = None

let format (xunit : _ Translation_unit.t) conf ?(dump_ast = dummy_dump_ast)
    ?(dump_formatted = dummy_dump_formatted) ~input_name ~source () =
  Location.input_name := input_name ;
  let parsed =
    try Ok (Translation_unit.parse xunit.parse conf source)
    with e -> Error e
  in
  Translation_unit.format xunit conf ~dump_ast ~dump_formatted ~input_name
    ~source ~parsed ()

let format conf ~kind =
  match kind with
  | `Impl -> format impl conf
  | `Intf -> format intf conf
  | `Use_file -> format use_file conf
