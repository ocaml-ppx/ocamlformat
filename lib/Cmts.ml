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
    let attribute (m : Ast_mapper.mapper) (attr : attribute) =
      match (attr.attr_name, attr.attr_payload) with
      | ( {txt= ("ocaml.doc" | "ocaml.text") as txt; _}
        , PStr
            [ { pstr_desc=
                  Pstr_eval
                    ( { pexp_desc= Pexp_constant (Pconst_string (doc, None))
                      ; pexp_attributes
                      ; _ }
                    , [] )
              ; _ } ] ) ->
          (* ignore location of docstrings *)
          { attr_name= {txt; loc= Location.none}
          ; attr_loc= Location.none
          ; attr_payload=
              m.payload m
                (PStr
                   [ { pstr_desc=
                         Pstr_eval
                           ( { pexp_desc=
                                 Pexp_constant (Pconst_string (doc, None))
                             ; pexp_loc= Location.none
                             ; pexp_attributes=
                                 m.attributes m pexp_attributes
                             ; pexp_loc_stack= [] }
                           , [] )
                     ; pstr_loc= Location.none } ]) }
      | _ -> Ast_mapper.default_mapper.attribute m attr
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
      | _ -> () ) ;
      Ast_mapper.default_mapper.pat m p
    in
    let mapper = Ast_mapper.{default_mapper with location; pat; attribute} in
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
  module Order_by_start = struct
    type t = Location.t

    include Comparator.Make (struct
      include Location

      let compare = compare_start
    end)
  end

  module Order_by_end = struct
    type t = Location.t

    include Comparator.Make (struct
      include Location

      let compare = compare_end
    end)
  end

  type smap = Cmt.t list Map.M(Order_by_start).t

  type emap = Cmt.t list Map.M(Order_by_end).t

  type t = smap * emap

  let empty_start = Map.empty (module Order_by_start)

  let empty_end = Map.empty (module Order_by_end)

  let empty : t = (empty_start, empty_end)

  let is_empty (smap, _) = Map.is_empty smap

  let of_list cmts =
    List.fold cmts ~init:empty ~f:(fun (smap, emap) cmt ->
        let {Cmt.loc; _} = cmt in
        ( Map.add_multi smap ~key:loc ~data:cmt
        , Map.add_multi emap ~key:loc ~data:cmt ))

  let to_list (smap, _) = List.concat (Map.data smap)

  let split (t : t) (loc : Location.t) =
    let addo m kvo =
      Option.fold kvo ~init:m ~f:(fun m (key, data) -> Map.set m ~key ~data)
    in
    let partition ((smap, emap) : t) (loc : Location.t) =
      let s_before, s_equal, s_after = Map.split smap loc in
      let s_after = addo s_after s_equal in
      let e_before, e_after =
        Map.partitioni_tf emap ~f:(fun ~key ~data:_ -> Map.mem s_before key)
      in
      ((s_before, e_before), (s_after, e_after))
    in
    let nafter, after = partition t {loc with loc_start= loc.loc_end} in
    let before, within =
      partition nafter {loc with loc_end= loc.loc_start}
    in
    (before, within, after)
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
        String.equal str ";" || Ast.is_infix_id str
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

let split_asterisk_prefixed {Cmt.txt; loc= {Location.loc_start; _}} =
  let len = Position.column loc_start + 3 in
  let pat =
    String.Search_pattern.create
      (String.init len ~f:(function
        | 0 -> '\n'
        | n when n < len - 1 -> ' '
        | _ -> '*'))
  in
  let rec split_asterisk_prefixed_ pos =
    match String.Search_pattern.index pat ~pos ~in_:txt with
    | Some 0 -> "" :: split_asterisk_prefixed_ len
    | Some idx ->
        String.sub txt ~pos ~len:(idx - pos)
        :: split_asterisk_prefixed_ (idx + len)
    | _ ->
        let drop = function ' ' | '\t' -> true | _ -> false in
        let line = String.rstrip ~drop (String.drop_prefix txt pos) in
        if String.is_empty line then [" "]
        else if Char.equal line.[String.length line - 1] '\n' then
          [String.drop_suffix line 1; ""]
        else if Char.is_whitespace txt.[String.length txt - 1] then
          [line ^ " "]
        else [line]
  in
  split_asterisk_prefixed_ 0

let unindent_lines ~opn_pos first_line tl_lines =
  let indent_of_line s =
    (* index of first non-whitespace is indentation, None means white line *)
    String.lfindi s ~f:(fun _ c -> not (Char.is_whitespace c))
  in
  (* The indentation of the first line must account for the location of the
     comment opening *)
  let fl_spaces = Option.value ~default:0 (indent_of_line first_line) in
  let fl_offset = opn_pos.Lexing.pos_cnum - opn_pos.pos_bol + 2 in
  let fl_indent = fl_spaces + fl_offset in
  let min_indent =
    List.fold_left ~init:fl_indent
      ~f:(fun acc s ->
        Option.value_map ~default:acc ~f:(min acc) (indent_of_line s))
      tl_lines
  in
  (* Completely trim the first line *)
  String.drop_prefix first_line fl_spaces
  :: List.map ~f:(fun s -> String.drop_prefix s min_indent) tl_lines

let fmt_multiline_cmt ?epi ~opn_pos ~starts_with_sp first_line tl_lines =
  let open Fmt in
  let is_white_line s = String.for_all s ~f:Char.is_whitespace in
  let unindented = unindent_lines ~opn_pos first_line tl_lines in
  let fmt_line ~first ~last:_ s =
    let sep, sp =
      if is_white_line s then (str "\n", noop)
      else (fmt "@;<1000 0>", fmt_if starts_with_sp " ")
    in
    fmt_if_k (not first) sep $ sp $ str (String.rstrip s)
  in
  vbox 0 (list_fl unindented fmt_line $ fmt_opt epi)

let fmt_cmt t (conf : Conf.t) ~fmt_code (cmt : Cmt.t) =
  let open Fmt in
  let fmt_asterisk_prefixed_lines lines =
    vbox 1
      ( fmt "(*"
      $ list_pn lines (fun ~prev:_ line ~next ->
            match (line, next) with
            | "", None -> fmt ")"
            | _, None -> str line $ fmt "*)"
            | _, Some _ -> str line $ fmt "@,*") )
  in
  let fmt_unwrapped_cmt ({txt= s; loc} : Cmt.t) =
    let begins_line = Source.begins_line t.source loc ~ignore_spaces:false in
    let is_sp = function ' ' | '\t' -> true | _ -> false in
    let epi =
      (* Preserve position of closing but strip empty lines at the end *)
      match String.rfindi s ~f:(fun _ c -> not (is_sp c)) with
      | Some i when Char.( = ) s.[i] '\n' ->
          break 1000 (-2) (* Break before closing *)
      | Some i when i < String.length s - 1 ->
          str " " (* Preserve a space at the end *)
      | _ -> noop
    in
    let stripped = String.rstrip s in
    match String.split_lines stripped with
    | first_line :: (_ :: _ as tl)
      when (not begins_line) && not (String.is_empty first_line) ->
        (* Preserve the first level of indentation *)
        let starts_with_sp = is_sp first_line.[0] in
        fmt_multiline_cmt ~opn_pos:loc.loc_start ~epi ~starts_with_sp
          first_line tl
    | _ -> str s
  in
  let fmt_non_code cmt =
    if not conf.wrap_comments then
      match split_asterisk_prefixed cmt with
      | [""] | [_] | [_; ""] -> wrap "(*" "*)" (fmt_unwrapped_cmt cmt)
      | asterisk_prefixed_lines ->
          fmt_asterisk_prefixed_lines asterisk_prefixed_lines
    else
      match split_asterisk_prefixed cmt with
      | [""] -> str "(* *)"
      | [text] -> str "(*" $ fill_text text ~epi:(str "*)")
      | [text; ""] -> str "(*" $ fill_text text ~epi:(str " *)")
      | asterisk_prefixed_lines ->
          fmt_asterisk_prefixed_lines asterisk_prefixed_lines
  in
  let fmt_code ({Cmt.txt= str; _} as cmt) =
    let dollar_last = Char.equal str.[String.length str - 1] '$' in
    let len = String.length str - if dollar_last then 2 else 1 in
    let source = String.sub ~pos:1 ~len str in
    match fmt_code conf source with
    | Ok formatted ->
        let cls : Fmt.s = if dollar_last then "$*)" else "*)" in
        hvbox 2 (wrap "(*$" cls (fmt "@;" $ formatted $ fmt "@;<1 -2>"))
    | Error () -> fmt_non_code cmt
  in
  match Cmt.txt cmt with
  | "" | "$" -> fmt_non_code cmt
  | str when Char.equal str.[0] '$' -> fmt_code cmt
  | _ -> fmt_non_code cmt

let pop_if_debug t loc =
  if t.debug && t.remove then update_remaining t ~f:(Location.Set.remove loc)

let find_cmts t pos loc =
  let r = find_at_position t loc pos in
  if t.remove then
    update_cmts t pos ~f:(fun m -> Location.Multimap.remove m loc) ;
  r

(** Find, remove, and format comments for loc. *)
let fmt_cmts t (conf : Conf.t) ~fmt_code ?pro ?epi ?(eol = Fmt.fmt "@\n")
    ?(adj = eol) find loc =
  let open Fmt in
  pop_if_debug t loc ;
  match find loc with
  | None | Some [] -> noop
  | Some cmts ->
      let line_dist a b =
        b.Location.loc_start.pos_lnum - a.Location.loc_end.pos_lnum
      in
      let groups =
        List.group cmts ~break:(fun {Cmt.loc= a; _} {Cmt.loc= b; _} ->
            let vertical_align =
              line_dist a b = 1
              && Location.compare_start_col a b = 0
              && Location.compare_end_col a b = 0
            in
            let horizontal_align =
              line_dist a b = 0
              && Option.value_map
                   (Source.string_between t.source a.loc_end b.loc_start)
                   ~default:true ~f:(fun x -> String.(strip x |> is_empty))
            in
            not
              ( ( Location.is_single_line a conf.margin
                && Location.is_single_line b conf.margin )
              && (vertical_align || horizontal_align) ))
      in
      let last_loc = Cmt.loc (List.last_exn cmts) in
      let eol_cmt = Source.ends_line t.source last_loc in
      let adj_cmt =
        eol_cmt
        && last_loc.Location.loc_end.pos_lnum + 1
           = loc.Location.loc_start.pos_lnum
      in
      let maybe_newline ~next {Cmt.loc= cur_last_loc; _} =
        match next with
        | Some ({Cmt.loc= next_loc; _} :: _) ->
            fmt_if (line_dist cur_last_loc next_loc > 1) "\n"
        | _ -> noop
      in
      list_pn groups (fun ~prev group ~next ->
          fmt_or_k (Option.is_none prev)
            (fmt_opt pro $ open_vbox 0)
            (fmt "@ ")
          $ ( match group with
            | [] -> impossible "previous match"
            | [cmt] -> fmt_cmt t conf cmt ~fmt_code $ maybe_newline ~next cmt
            | group ->
                list group "@;<1000 0>" (fun cmt ->
                    wrap "(*" "*)" (str (Cmt.txt cmt)))
                $ maybe_newline ~next (List.last_exn group) )
          $ fmt_if_k (Option.is_none next)
              ( close_box
              $ fmt_or_k eol_cmt (fmt_or_k adj_cmt adj eol) (fmt_opt epi) ))

let fmt_before t conf ~fmt_code ?pro ?(epi = Fmt.break 1 0) ?eol ?adj =
  fmt_cmts t conf (find_cmts t `Before) ~fmt_code ?pro ~epi ?eol ?adj

let fmt_after t conf ~fmt_code ?(pro = Fmt.break 1 0) ?epi =
  let open Fmt in
  let within = fmt_cmts t conf (find_cmts t `Within) ~fmt_code ~pro ?epi in
  let after =
    fmt_cmts t conf (find_cmts t `After) ~fmt_code ~pro ?epi ~eol:noop
  in
  fun loc -> within loc $ after loc

let fmt_within t conf ~fmt_code ?(pro = Fmt.break 1 0) ?(epi = Fmt.break 1 0)
    =
  fmt_cmts t conf (find_cmts t `Within) ~fmt_code ~pro ~epi ~eol:Fmt.noop

let fmt t conf ~fmt_code ?pro ?epi ?eol ?adj loc =
  let open Fmt in
  (* remove the before comments from the map first *)
  let before = fmt_before t conf ~fmt_code ?pro ?epi ?eol ?adj loc in
  (* remove the within comments from the map by accepting the continuation *)
  fun k ->
    (* delay the after comments until the within comments have been removed *)
    let after = fmt_after t conf ~fmt_code ?pro ?epi loc in
    let inner = k in
    before $ inner $ after

let fmt_list t conf ~fmt_code ?pro ?epi ?eol locs init =
  List.fold locs ~init ~f:(fun k loc ->
      fmt t conf ~fmt_code ?pro ?epi ?eol loc @@ k)

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
