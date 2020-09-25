(**************************************************************************)
(*                                                                        *)
(*                              OCamlFormat                               *)
(*                                                                        *)
(*            Copyright (c) Facebook, Inc. and its affiliates.            *)
(*                                                                        *)
(*      This source code is licensed under the MIT license found in       *)
(*      the LICENSE file in the root directory of this source tree.       *)
(*                                                                        *)
(**************************************************************************)

let selected_version = Migrate_parsetree.Versions.ocaml_411

module Selected_version = Ast_411
module Ast_mapper = Selected_version.Ast_mapper
module Ast_helper = Selected_version.Ast_helper

module Parsetree = struct
  include Selected_version.Parsetree

  let equal_core_type : core_type -> core_type -> bool = Poly.equal

  let equal_structure : structure -> structure -> bool = Poly.equal

  let equal_signature : signature -> signature -> bool = Poly.equal

  let equal_toplevel_phrase : toplevel_phrase -> toplevel_phrase -> bool =
    Poly.equal
end

module Asttypes = struct
  include Selected_version.Asttypes

  let is_private = function Private -> true | Public -> false

  let is_open = function Open -> true | Closed -> false

  let is_override = function Override -> true | Fresh -> false

  let is_mutable = function Mutable -> true | Immutable -> false
end

module Mapper = struct
  let structure = Selected_version.map_structure

  let signature = Selected_version.map_signature

  (* Missing from ocaml_migrate_parsetree *)
  let use_file (mapper : Ast_mapper.mapper) use_file =
    let open Parsetree in
    List.map use_file ~f:(fun toplevel_phrase ->
        match (toplevel_phrase : toplevel_phrase) with
        | Ptop_def structure ->
            Ptop_def (mapper.Ast_mapper.structure mapper structure)
        | Ptop_dir {pdir_name; pdir_arg; pdir_loc} ->
            let pdir_arg =
              match pdir_arg with
              | None -> None
              | Some a ->
                  Some {a with pdira_loc= mapper.location mapper a.pdira_loc}
            in
            Ptop_dir
              { pdir_name=
                  {pdir_name with loc= mapper.location mapper pdir_name.loc}
              ; pdir_arg
              ; pdir_loc= mapper.location mapper pdir_loc })

  type 'a fragment =
    | Structure : Parsetree.structure fragment
    | Signature : Parsetree.signature fragment
    | Use_file : Parsetree.toplevel_phrase list fragment

  let equal (type a) (x : a fragment) : a -> a -> bool =
    match x with
    | Structure -> Parsetree.equal_structure
    | Signature -> Parsetree.equal_signature
    | Use_file -> List.equal Parsetree.equal_toplevel_phrase

  let map_ast (type a) (x : a fragment) : Ast_mapper.mapper -> a -> a =
    match x with
    | Structure -> structure
    | Signature -> signature
    | Use_file -> use_file
end

module Parse = struct
  open Migrate_parsetree

  let implementation = Parse.implementation selected_version

  let interface = Parse.interface selected_version

  let use_file lexbuf =
    List.filter (Parse.use_file selected_version lexbuf)
      ~f:(fun (p : Parsetree.toplevel_phrase) ->
        match p with
        | Ptop_def [] -> false
        | Ptop_def (_ :: _) | Ptop_dir _ -> true)

  let fragment (type a) (fragment : a Mapper.fragment) lexbuf : a =
    match fragment with
    | Mapper.Structure -> implementation lexbuf
    | Mapper.Signature -> interface lexbuf
    | Mapper.Use_file -> use_file lexbuf
end

let to_current =
  Migrate_parsetree.Versions.(migrate selected_version ocaml_current)

module Printast = struct
  open Printast

  let implementation f x = implementation f (to_current.copy_structure x)

  let interface f x = interface f (to_current.copy_signature x)

  let expression f x = expression 0 f (to_current.copy_expression x)

  let payload f (x : Parsetree.payload) =
    payload 0 f
      ( match x with
      | PStr x -> PStr (to_current.copy_structure x)
      | PSig x -> PSig (to_current.copy_signature x)
      | PTyp x -> PTyp (to_current.copy_core_type x)
      | PPat (x, y) ->
          PPat
            ( to_current.copy_pattern x
            , Option.map ~f:to_current.copy_expression y ) )

  let use_file f (x : Parsetree.toplevel_phrase list) =
    List.iter x ~f:(fun (p : Parsetree.toplevel_phrase) ->
        top_phrase f (to_current.copy_toplevel_phrase p))

  let fragment (type a) : a Mapper.fragment -> _ -> a -> _ = function
    | Mapper.Structure -> implementation
    | Mapper.Signature -> interface
    | Mapper.Use_file -> use_file
end

module Pprintast = struct
  open Pprintast

  let structure f x = structure f (to_current.copy_structure x)

  let signature f x = signature f (to_current.copy_signature x)

  let core_type f x = core_type f (to_current.copy_core_type x)

  let expression f x = expression f (to_current.copy_expression x)

  let pattern f x = pattern f (to_current.copy_pattern x)

  let toplevel_phrase f x =
    toplevel_phrase f (to_current.copy_toplevel_phrase x)
end

module Position = struct
  open Lexing

  type t = position

  let column {pos_bol; pos_cnum; _} = pos_cnum - pos_bol

  let fmt fs {pos_lnum; pos_bol; pos_cnum; _} =
    if pos_lnum = -1 then Format.fprintf fs "[%d]" pos_cnum
    else Format.fprintf fs "[%d,%d+%d]" pos_lnum pos_bol (pos_cnum - pos_bol)

  let to_string x = Format.asprintf "%a" fmt x

  let sexp_of_t x = Sexp.Atom (to_string x)

  let compare_col p1 p2 = Int.compare (column p1) (column p2)

  let compare p1 p2 =
    if phys_equal p1 p2 then 0 else Int.compare p1.pos_cnum p2.pos_cnum

  include (val Comparator.make ~compare ~sexp_of_t)

  let distance p1 p2 = p2.pos_cnum - p1.pos_cnum
end

module Location = struct
  include Selected_version.Location

  let fmt fs {loc_start; loc_end; loc_ghost} =
    Format.fprintf fs "(%a..%a)%s" Position.fmt loc_start Position.fmt
      loc_end
      (if loc_ghost then " ghost" else "")

  let to_string x = Format.asprintf "%a" fmt x

  let sexp_of_t x = Sexp.Atom (to_string x)

  let compare : t -> t -> int = Poly.compare

  type location = t

  module Location_comparator = Comparator.Make (struct
    type t = location

    let sexp_of_t = sexp_of_t

    let compare = compare
  end)

  include Location_comparator

  let compare_start x y = Position.compare x.loc_start y.loc_start

  let compare_start_col x y = Position.compare_col x.loc_start y.loc_start

  let compare_end x y = Position.compare x.loc_end y.loc_end

  let compare_end_col x y = Position.compare_col x.loc_end y.loc_end

  let contains l1 l2 = compare_start l1 l2 <= 0 && compare_end l1 l2 >= 0

  let width x = Position.distance x.loc_start x.loc_end

  let descending cmp a b = -cmp a b

  let compare_width_decreasing =
    Comparable.lexicographic [compare_start; descending compare_end; compare]

  let is_single_line x margin =
    (* The last character of a line can exceed the margin if it is not
       preceded by a break. Adding 1 here is a workaround for this bug. *)
    width x <= margin + 1 && x.loc_start.pos_lnum = x.loc_end.pos_lnum

  let smallest loc stack =
    let min a b = if width a < width b then a else b in
    List.reduce_exn (loc :: stack) ~f:min

  module Set = struct
    type t = (location, comparator_witness) Set.t

    let empty =
      Set.empty
        ( module struct
          type t = location

          include Location_comparator
        end )

    let add key t = Set.add t key

    let remove key t = Set.remove t key

    let to_list t = Set.to_list t
  end

  module Multimap = struct
    type 'a t = (location, 'a list, comparator_witness) Map.t

    let add_list map key data = Map.add_exn map ~key ~data

    let remove map loc = Map.remove map loc

    let update_multi map ~src ~dst ~f =
      Option.fold (Map.find map src) ~init:(Map.remove map src)
        ~f:(fun new_map src_data ->
          Map.update new_map dst ~f:(fun dst_data ->
              Option.fold dst_data ~init:src_data ~f))

    let empty =
      Map.empty
        ( module struct
          type t = location

          include Location_comparator
        end )

    let find map loc = Map.find map loc

    let filter map ~f = Map.map map ~f:(List.filter ~f)

    let mem map loc = Map.mem map loc

    let to_list map = Map.to_alist map |> List.concat_map ~f:snd

    let find_multi map loc = Map.find_multi map loc
  end
end

module Longident = struct
  type t = Longident.t =
    | Lident of string
    | Ldot of t * string
    | Lapply of t * t

  let flatten = Longident.flatten

  let last = Longident.last

  let lident s =
    assert (not (String.contains s '.')) ;
    Lident s
end

module Parser = Token_latest

module Lexer = struct
  let token lexbuf = Lexer.token lexbuf |> Token_latest.of_compiler_libs

  type error = Lexer.error

  exception Error = Lexer.Error
end
