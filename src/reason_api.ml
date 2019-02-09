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

module type Formattable = sig
  type t

  val format :
       Conf.t
    -> ?dump_ast:(suffix:string -> (Formatter.t -> unit) -> unit)
    -> ?dump_formatted:(suffix:string -> string -> string option)
    -> input_name:string
    -> source:string
    -> parsed:t Translation_unit.with_comments
    -> unit
    -> (string, Translation_unit.error) Result.t
end

module type T = sig
  type t

  val unit : t Translation_unit.t
end

module Make (X : T) = struct
  type t = X.t

  let dummy_dump_ast ~suffix:_ _ = ()

  let dummy_dump_formatted ~suffix:_ _ = None

  let format conf ?(dump_ast = dummy_dump_ast)
      ?(dump_formatted = dummy_dump_formatted) ~input_name ~source ~parsed
      () =
    Location.input_name := input_name ;
    let dump_formatted = dump_formatted in
    let parsed = Ok parsed in
    Translation_unit.format X.unit conf ~dump_ast ~dump_formatted
      ~input_name ~source ~parsed ()
end

module Impl = Make (struct
  type t = Migrate_ast.Parsetree.structure

  let unit = impl
end)

module Intf = Make (struct
  type t = Migrate_ast.Parsetree.signature

  let unit = intf
end)
