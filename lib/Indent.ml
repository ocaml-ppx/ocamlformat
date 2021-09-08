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

open Migrate_ast

module Ocp_indent = struct
  let run ~source ~in_lines ~on_indent ~init =
    let config, _, _ = IndentConfig.local_default () in
    let output : _ IndentPrinter.output =
      { debug= false
      ; config
      ; in_lines
      ; adaptive= true
      ; indent_empty= false
      ; kind= Numeric on_indent }
    in
    let stream = Nstream.of_string source in
    IndentPrinter.proceed output stream IndentBlock.empty init
end

module Valid_ast = struct
  let loc_of_line loctree locs line =
    let rec aux = function
      | [] -> None
      | (h : Location.t) :: t ->
          if h.loc_start.pos_lnum = line then Some h
          else if h.loc_start.pos_lnum <= line && line <= h.loc_end.pos_lnum
          then
            match Loc_tree.children loctree h with
            | [] -> Some h
            | children -> Some (Option.value (aux children) ~default:h)
          else aux t
    in
    aux locs

  let matching_loc locs locs' loc : Location.t =
    match List.zip locs locs' with
    | Ok assoc -> (
        let equal x y = Location.compare x y = 0 in
        match List.Assoc.find assoc ~equal loc with
        | Some loc -> loc
        | None ->
            impossible "Cannot find matching location in formatted output." )
    | Unequal_lengths ->
        impossible "Cannot match pre-post formatting locations."

  let indent_range fragment ~unformatted:(ast, txt_src)
      ~formatted:(fmted_ast, fmted_src) ~lines ~range:(low, high) =
    let loctree, locs = Loc_tree.of_ast fragment ast in
    let _, locs' = Loc_tree.of_ast fragment fmted_ast in
    let indent_line i =
      match loc_of_line loctree locs i with
      | Some loc -> (
          let fmted_loc = matching_loc locs locs' loc in
          let fmted_lnum = fmted_loc.loc_start.pos_lnum in
          match Source.find_first_token_on_line fmted_src fmted_lnum with
          | Some (_, loc) -> Some (Position.column loc.loc_start)
          | None -> None )
      | None -> None
    in
    let rec aux ?prev acc idx_ocpi i =
      if i > high then (List.rev acc, idx_ocpi)
      else
        match
          match List.nth lines (i - 1) with
          | Some _ -> (
            match indent_line i with
            | Some indent -> (
              match prev with
              | Some prev_indent when indent = prev_indent -> `Maybe indent
              | _ -> `Ok indent )
            | None -> `Ok 0 )
          | None -> `Ok 0
        with
        | `Ok x -> aux ~prev:x (`Ok x :: acc) idx_ocpi (i + 1)
        | `Maybe x -> aux ~prev:x (`Maybe x :: acc) (i :: idx_ocpi) (i + 1)
    in
    let prev =
      let i = low - 1 in
      match List.nth lines (i - 1) with
      | Some _ -> indent_line i
      | None -> None
    in
    let aux_ret, idx_ocpi = aux ?prev [] [] low in
    let rec on_indent i (aux_ret, res) =
      match aux_ret with
      | [] -> impossible "list expected to contain at least a `Maybe item"
      | `Ok x :: t -> on_indent i (t, x :: res)
      | `Maybe x :: t -> (t, (if i = 0 then x else i) :: res)
    in
    let aux_ret, ret =
      Ocp_indent.run ~source:txt_src ~init:(aux_ret, []) ~on_indent
        ~in_lines:(List.mem idx_ocpi ~equal:Int.equal)
    in
    List.rev
    @@ List.fold_left aux_ret ~init:ret ~f:(fun t -> function
         | `Ok h -> h :: t
         | `Maybe _ -> impossible "all `Maybe were handled by ocp-indent" )
end

module Partial_ast = struct
  let indent_range ~source ~range:(low, high) =
    List.rev
    @@ Ocp_indent.run ~source ~init:[]
         ~in_lines:(fun i -> low <= i && i <= high)
         ~on_indent:(fun i acc -> i :: acc)
end
