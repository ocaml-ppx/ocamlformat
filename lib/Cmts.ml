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

(** Placing and formatting comments in a parsetree. *)

module Format = Format_
open Migrate_ast
open Asttypes
open Parsetree

type t =
  { debug: bool
  ; mutable cmts_before: Cmt.t Location.Multimap.t
  ; mutable cmts_after: Cmt.t Location.Multimap.t
  ; mutable cmts_within: Cmt.t Location.Multimap.t
  ; source: Source.t
  ; mutable remaining: Location.Set.t
  ; remove: bool }

let update_remaining t ~f = t.remaining <- f t.remaining

let update_cmts t pos ~f =
  match pos with
  | `After -> t.cmts_after <- f t.cmts_after
  | `Before -> t.cmts_before <- f t.cmts_before
  | `Within -> t.cmts_within <- f t.cmts_within

let find_at_position t loc pos =
  let map =
    match pos with
    | `After -> t.cmts_after
    | `Before -> t.cmts_before
    | `Within -> t.cmts_within
  in
  Location.Multimap.find map loc

module Loc_tree : sig
  include Non_overlapping_interval_tree.S with type itv = Location.t

  val of_ast :
    (Ast_mapper.mapper -> 'a -> _) -> 'a -> Source.t -> t * Location.t list
end = struct
  include Non_overlapping_interval_tree.Make (Location)

  (* Use Ast_mapper to collect all locs in ast, and create tree of them. *)

  let of_ast map_ast ast src =
    let attribute (m : Ast_mapper.mapper) attr =
      (* ignore location of docstrings *)
      if Ast.Attr.is_doc attr then attr
      else Ast_mapper.default_mapper.attribute m attr
    in
    let locs = ref [] in
    let location _ loc =
      locs := loc :: !locs ;
      loc
    in
    let pat m p =
      ( match p.ppat_desc with
      | Ppat_record (flds, Open) ->
          Option.iter (Source.loc_of_underscore src flds p.ppat_loc)
            ~f:(fun loc -> locs := loc :: !locs)
      | Ppat_constant _ -> locs := Source.loc_of_pat_constant src p :: !locs
      | _ -> () ) ;
      Ast_mapper.default_mapper.pat m p
    in
    let expr m e =
      ( match e.pexp_desc with
      | Pexp_constant _ -> locs := Source.loc_of_expr_constant src e :: !locs
      | _ -> () ) ;
      Ast_mapper.default_mapper.expr m e
    in
    let mapper =
      Ast_mapper.{default_mapper with location; pat; attribute; expr}
    in
    map_ast mapper ast |> ignore ;
    (of_list !locs, !locs)
end

(** Sets of comments supporting splitting by locations. *)
module CmtSet : sig
  type t

  val of_list : Cmt.t list -> t

  val to_list : t -> Cmt.t list
  (** ordered by start location *)

  val empty : t

  val is_empty : t -> bool

  val split : t -> Location.t -> t * t * t
  (** [split s {loc_start; loc_end}] splits [s] into the subset of comments
      that end before [loc_start], those that start after [loc_end], and
      those within the loc. *)
end = struct
  type t = Cmt.t list Map.M(Position).t

  let empty = Map.empty (module Position)

  let is_empty = Map.is_empty

  let of_list cmts =
    List.fold cmts ~init:empty ~f:(fun map cmt ->
        let pos = cmt.Cmt.loc.loc_start in
        Map.add_multi map ~key:pos ~data:cmt)

  let to_list map = List.concat (Map.data map)

  (** Assuming loc_start <= loc_end, the locs are split in 5 sets:

      - a: before start
      - b: at start
      - c: after start, before end
      - d: at end
      - e: after end *)
  let split t {Location.loc_start; loc_end; _} =
    let add_opt kvo init =
      Option.fold kvo ~init ~f:(fun m (key, data) -> Map.set m ~key ~data)
    in
    let ( ++ ) = add_opt in
    let a_b_c, d, e = Map.split t loc_end in
    let a, b, c = Map.split a_b_c loc_start in
    (a, b ++ c, d ++ e)
end

(** Heuristic to determine if two locations should be considered "adjacent".
    Holds if there is only whitespace between the locations, or if there is a
    [|] character and the first location begins a line and the start column
    of the first location is lower than that of the second location. *)
let is_adjacent t (l1 : Location.t) (l2 : Location.t) =
  Option.value_map (Source.string_between t.source l1.loc_end l2.loc_start)
    ~default:false ~f:(fun btw ->
      match String.strip btw with
      | "" -> true
      | "|" ->
          Source.begins_line t.source l1
          && Position.column l1.loc_start < Position.column l2.loc_start
      | _ -> false)

(** Whether the symbol preceding location [loc] is an infix symbol or a
    semicolon. If it is the case, comments attached to the following item
    should be kept after the infix symbol. *)
let infix_symbol_before t (loc : Location.t) =
  let pos_cnum = loc.loc_end.pos_cnum - 1 in
  let loc_end = {loc.loc_end with pos_cnum} in
  match Source.position_before t.source loc_end with
  | Some loc_start ->
      if loc_start.pos_cnum < loc.loc_end.pos_cnum then
        let str = Source.string_at t.source loc_start loc.loc_end in
        String.equal str ";" || Ast.String_id.is_infix str
      else false
  | None -> false

(** Heuristic to choose between placing a comment after the previous location
    or before the next one. *)
let partition_after_prev_or_before_next t ~prev cmts ~next =
  match CmtSet.to_list cmts with
  | {loc; _} :: _ as cmtl when is_adjacent t prev loc -> (
    match
      List.group cmtl ~break:(fun l1 l2 ->
          not (is_adjacent t (Cmt.loc l1) (Cmt.loc l2)))
    with
    | [cmtl] when is_adjacent t (List.last_exn cmtl).loc next ->
        let open Location in
        let same_line_as_prev l =
          prev.loc_end.pos_lnum = l.loc_start.pos_lnum
        in
        let same_line_as_next l =
          next.loc_start.pos_lnum = l.loc_start.pos_lnum
        in
        let prev, next =
          if not (same_line_as_prev next) then
            let next, prev =
              List.partition_tf cmtl ~f:(fun {Cmt.loc= l1; _} ->
                  same_line_as_next l1
                  || (same_line_as_prev l1 && infix_symbol_before t prev)
                  || not (same_line_as_prev l1))
            in
            (prev, next)
          else ([], cmtl)
        in
        (CmtSet.of_list prev, CmtSet.of_list next)
    | after :: befores ->
        (CmtSet.of_list after, CmtSet.of_list (List.concat befores))
    | [] -> impossible "by parent match" )
  | _ -> (CmtSet.empty, cmts)

let position_to_string = function
  | `Before -> "before"
  | `After -> "after"
  | `Within -> "within"

let add_cmts t ?prev ?next position loc cmts =
  if not (CmtSet.is_empty cmts) then (
    let cmtl = CmtSet.to_list cmts in
    if t.debug then
      List.iter cmtl ~f:(fun {Cmt.txt= cmt_txt; loc= cmt_loc} ->
          let string_between (l1 : Location.t) (l2 : Location.t) =
            match Source.string_between t.source l1.loc_end l2.loc_start with
            | None -> "swapped"
            | Some s -> s
          in
          let btw_prev =
            Option.value_map prev ~default:"no prev"
              ~f:(Fn.flip string_between cmt_loc)
          in
          let btw_next =
            Option.value_map next ~default:"no next"
              ~f:(string_between cmt_loc)
          in
          Format.eprintf "add %s %a: %a \"%s\" %s \"%s\"@\n%!"
            (position_to_string position)
            Location.fmt loc Location.fmt cmt_loc (String.escaped btw_prev)
            cmt_txt (String.escaped btw_next)) ;
    update_cmts t position ~f:(fun v ->
        Location.Multimap.add_list v loc cmtl) )

(** Traverse the location tree from locs, find the deepest location that
    contains each comment, intersperse comments between that location's
    children. *)
let rec place t loc_tree ?prev_loc locs cmts =
  match locs with
  | curr_loc :: next_locs ->
      let before, within, after = CmtSet.split cmts curr_loc in
      let before_curr =
        match prev_loc with
        | None -> before
        | Some prev_loc ->
            let after_prev, before_curr =
              partition_after_prev_or_before_next t ~prev:prev_loc before
                ~next:curr_loc
            in
            add_cmts t `After ~prev:prev_loc ~next:curr_loc prev_loc
              after_prev ;
            before_curr
      in
      add_cmts t `Before ?prev:prev_loc ~next:curr_loc curr_loc before_curr ;
      ( match Loc_tree.children loc_tree curr_loc with
      | [] ->
          add_cmts t `Within ?prev:prev_loc ~next:curr_loc curr_loc within
      | children -> place t loc_tree children within ) ;
      place t loc_tree ~prev_loc:curr_loc next_locs after
  | [] -> (
    match prev_loc with
    | Some prev_loc -> add_cmts t `After ~prev:prev_loc prev_loc cmts
    | None ->
        if t.debug then
          List.iter (CmtSet.to_list cmts) ~f:(fun {Cmt.txt; _} ->
              Format.eprintf "lost: %s@\n%!" txt) )

(** Relocate comments, for Ast transformations such as sugaring. *)
let relocate (t : t) ~src ~before ~after =
  if t.remove then (
    if t.debug then
      Format.eprintf "relocate %a to %a and %a@\n%!" Location.fmt src
        Location.fmt before Location.fmt after ;
    let merge_and_sort x y =
      List.rev_append x y
      |> List.sort
           ~compare:(Comparable.lift Location.compare_start ~f:Cmt.loc)
    in
    update_cmts t `Before
      ~f:(Location.Multimap.update_multi ~src ~dst:before ~f:merge_and_sort) ;
    update_cmts t `After
      ~f:(Location.Multimap.update_multi ~src ~dst:after ~f:merge_and_sort) ;
    update_cmts t `Within
      ~f:(Location.Multimap.update_multi ~src ~dst:after ~f:merge_and_sort) ;
    if t.debug then
      update_remaining t ~f:(fun r ->
          r |> Location.Set.remove src |> Location.Set.add after
          |> Location.Set.add before) )

let relocate_pattern_matching_cmts (t : t) src tok ~whole_loc ~matched_loc =
  let kwd_loc = Option.value_exn (Source.loc_of_keyword src whole_loc tok) in
  let f map =
    let before, after =
      List.partition_tf (Location.Multimap.find_multi map matched_loc)
        ~f:(fun Cmt.{loc; _} -> Location.compare_end loc kwd_loc < 0)
    in
    let map =
      List.fold_left ~init:map (List.rev before) ~f:(fun map ->
          Location.Multimap.add_multi map whole_loc)
    in
    Location.Multimap.change_multi map matched_loc after
  in
  update_cmts t `Before ~f ;
  update_cmts t `After ~f ;
  update_cmts t `Within ~f

(** Initialize global state and place comments. *)
let init map_ast ~debug source asts comments_n_docstrings =
  let t =
    { debug
    ; cmts_before= Location.Multimap.empty
    ; cmts_after= Location.Multimap.empty
    ; cmts_within= Location.Multimap.empty
    ; source
    ; remaining= Location.Set.empty
    ; remove= true }
  in
  let comments = Normalize.dedup_cmts map_ast asts comments_n_docstrings in
  if debug then (
    Format.eprintf "\nComments:\n%!" ;
    List.iter comments ~f:(fun {Cmt.txt; loc} ->
        Format.eprintf "%a %s %s@\n%!" Location.fmt loc txt
          (if Source.ends_line source loc then "eol" else "")) ) ;
  if not (List.is_empty comments) then (
    let loc_tree, locs = Loc_tree.of_ast map_ast asts source in
    if debug then
      List.iter locs ~f:(fun loc ->
          if not (Location.compare loc Location.none = 0) then
            update_remaining t ~f:(Location.Set.add loc)) ;
    if debug then (
      let dump fs lt = Fmt.eval fs (Loc_tree.dump lt) in
      Format.eprintf "\nLoc_tree:\n%!" ;
      Format.eprintf "@\n%a@\n@\n%!" dump loc_tree ) ;
    let locs = Loc_tree.roots loc_tree in
    let cmts = CmtSet.of_list comments in
    match locs with
    | [] -> add_cmts t `After ~prev:Location.none Location.none cmts
    | _ -> place t loc_tree locs cmts ) ;
  let () =
    let relocate_loc_stack loc stack =
      List.iter stack ~f:(fun src -> relocate t ~src ~before:loc ~after:loc)
    in
    let expr (m : Ast_mapper.mapper) x =
      relocate_loc_stack x.pexp_loc x.pexp_loc_stack ;
      Ast_mapper.default_mapper.expr m x
    in
    let typ (m : Ast_mapper.mapper) x =
      relocate_loc_stack x.ptyp_loc x.ptyp_loc_stack ;
      Ast_mapper.default_mapper.typ m x
    in
    let pat (m : Ast_mapper.mapper) x =
      relocate_loc_stack x.ppat_loc x.ppat_loc_stack ;
      Ast_mapper.default_mapper.pat m x
    in
    let _ = map_ast Ast_mapper.{default_mapper with pat; typ; expr} asts in
    ()
  in
  t

let init_impl = init Mapper.structure

let init_intf = init Mapper.signature

let init_toplevel = init Mapper.use_file

let preserve fmt_x t =
  let buf = Buffer.create 128 in
  let fs = Format.formatter_of_buffer buf in
  Fmt.eval fs (fmt_x {t with remove= false}) ;
  Format.pp_print_flush fs () ;
  Buffer.contents buf

let pop_if_debug t loc =
  if t.debug && t.remove then update_remaining t ~f:(Location.Set.remove loc)

let find_cmts t pos loc =
  pop_if_debug t loc ;
  let r = find_at_position t loc pos in
  if t.remove then
    update_cmts t pos ~f:(fun m -> Location.Multimap.remove m loc) ;
  r

let line_dist a b =
  b.Location.loc_start.pos_lnum - a.Location.loc_end.pos_lnum

let break_comment_group source margin {Cmt.loc= a; _} {Cmt.loc= b; _} =
  let vertical_align =
    line_dist a b = 1
    && Location.compare_start_col a b = 0
    && Location.compare_end_col a b = 0
  in
  let horizontal_align =
    line_dist a b = 0
    && Option.value_map (Source.string_between source a.loc_end b.loc_start)
         ~default:true ~f:(fun x -> String.(strip x |> is_empty))
  in
  not
    ( (Location.is_single_line a margin && Location.is_single_line b margin)
    && (vertical_align || horizontal_align) )

(** Format comments for loc. *)
let fmt_cmts t (conf : Conf.t) ~fmt_code ?pro ?epi ?(eol = Fmt.fmt "@\n")
    ?(adj = eol) found loc pos =
  let open Fmt in
  match found with
  | None | Some [] -> noop
  | Some cmts ->
      let groups =
        List.group cmts ~break:(break_comment_group t.source conf.margin)
      in
      let last_loc = Cmt.loc (List.last_exn cmts) in
      let eol_cmt = Source.ends_line t.source last_loc in
      let adj_cmt = eol_cmt && line_dist last_loc loc = 1 in
      fmt_opt pro
      $ vbox 0
          (list_pn groups (fun ~prev:_ group ~next ->
               ( match group with
               | [] -> impossible "previous match"
               | [cmt] ->
                   Cmt.fmt cmt t.source ~wrap:conf.wrap_comments
                     ~ocp_indent_compat:conf.ocp_indent_compat
                     ~fmt_code:(fmt_code conf) pos
               | group ->
                   list group "@;<1000 0>" (fun cmt ->
                       wrap "(*" "*)" (str (Cmt.txt cmt))) )
               $
               match next with
               | Some (next :: _) ->
                   let last = List.last_exn group in
                   fmt_if (line_dist (Cmt.loc last) (Cmt.loc next) > 1) "\n"
                   $ fmt "@ "
               | _ -> noop))
      $ fmt_or_k eol_cmt (fmt_or_k adj_cmt adj eol) (fmt_opt epi)

let fmt_before t conf ~fmt_code ?pro ?(epi = Fmt.break 1 0) ?eol ?adj loc =
  fmt_cmts t conf
    (find_cmts t `Before loc)
    ~fmt_code ?pro ~epi ?eol ?adj loc Cmt.Before

let fmt_after t conf ~fmt_code ?(pro = Fmt.break 1 0) ?epi loc =
  let open Fmt in
  let within =
    fmt_cmts t conf
      (find_cmts t `Within loc)
      ~fmt_code ~pro ?epi loc Cmt.Within
  in
  let after =
    fmt_cmts t conf
      (find_cmts t `After loc)
      ~fmt_code ~pro ?epi ~eol:noop loc Cmt.After
  in
  within $ after

let fmt_within t conf ~fmt_code ?(pro = Fmt.break 1 0) ?(epi = Fmt.break 1 0)
    loc =
  fmt_cmts t conf
    (find_cmts t `Within loc)
    ~fmt_code ~pro ~epi ~eol:Fmt.noop loc Cmt.Within

let drop_inside t loc =
  let clear pos =
    update_cmts t pos
      ~f:
        (Location.Multimap.filter ~f:(fun {Cmt.loc= cmt_loc; _} ->
             not (Location.contains loc cmt_loc)))
  in
  clear `Before ;
  clear `Within ;
  clear `After

let drop_before t loc =
  update_cmts t `Before ~f:(fun m -> Location.Multimap.remove m loc) ;
  t

let has_before t loc =
  pop_if_debug t loc ;
  Location.Multimap.mem t.cmts_before loc

let has_within t loc =
  pop_if_debug t loc ;
  Location.Multimap.mem t.cmts_within loc

let has_after t loc =
  pop_if_debug t loc ;
  Location.Multimap.mem t.cmts_within loc
  || Location.Multimap.mem t.cmts_after loc

(** returns comments that have not been formatted *)
let remaining_comments t =
  List.concat_map ~f:Location.Multimap.to_list
    [t.cmts_before; t.cmts_within; t.cmts_after]

let remaining_before t loc = Location.Multimap.find_multi t.cmts_before loc

let remaining_locs t = Location.Set.to_list t.remaining

let diff (conf : Conf.t) x y =
  let norm z =
    let norm_non_code {Cmt.txt; _} = Normalize.comment txt in
    let f z =
      match Cmt.txt z with
      | "" | "$" -> norm_non_code z
      | str ->
          if Char.equal str.[0] '$' then
            let chars_removed =
              if Char.equal str.[String.length str - 1] '$' then 2 else 1
            in
            let len = String.length str - chars_removed in
            let str = String.sub ~pos:1 ~len str in
            try
              Migrate_ast.Parse.implementation (Lexing.from_string str)
              |> Normalize.impl conf
              |> Caml.Format.asprintf "%a" Printast.implementation
            with _ -> norm_non_code z
          else norm_non_code z
    in
    Set.of_list (module String) (List.map ~f z)
  in
  Set.symmetric_diff (norm x) (norm y)
