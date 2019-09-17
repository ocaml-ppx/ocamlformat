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
  { cmts_before: (Location.t, Cmt.t list) Hashtbl.t
  ; cmts_after: (Location.t, Cmt.t list) Hashtbl.t
  ; cmts_within: (Location.t, Cmt.t list) Hashtbl.t
  ; source: Source.t
  ; remaining: (Location.t, unit) Hashtbl.t }

(** A tree of non-overlapping intervals. Intervals are non-overlapping if
    whenever 2 intervals share more than an end-point, then one contains the
    other. *)
module Non_overlapping_interval_tree (Itv : sig
  include Hashtbl.Key [@@ocaml.warning "-3"]

  val contains : t -> t -> bool

  val compare_width_decreasing : t -> t -> int
end) : sig
  type t

  val of_list : Itv.t list -> t
  (** If there are duplicates in the input list, earlier elements will be
      ancestors of later elements. *)

  val roots : t -> Itv.t list

  val children : t -> Itv.t -> Itv.t list

  val dump : t -> Fmt.t
  (** Debug: dump debug representation of tree. *)
end = struct
  (* simple but (asymptotically) suboptimal implementation *)

  type t = {mutable roots: Itv.t list; tbl: (Itv.t, Itv.t list) Hashtbl.t}

  let roots t = t.roots

  let create () = {roots= []; tbl= Hashtbl.create (module Itv)}

  (* Descend tree from roots, find deepest node that contains elt. *)

  let rec parents tbl roots ~ancestors elt =
    Option.value ~default:ancestors
      (List.find_map roots ~f:(fun root ->
           if Itv.contains root elt then
             Hashtbl.find_and_call tbl root
               ~if_found:(fun children ->
                 parents tbl children ~ancestors:(root :: ancestors) elt)
               ~if_not_found:(fun x -> x :: ancestors)
             |> Option.some
           else None))

  (* Add elements in decreasing width order to construct tree from roots to
     leaves. That is, when adding an interval to a partially constructed
     tree, it will already contain all wider intervals, so the new interval's
     parent will already be in the tree. *)

  let of_list elts =
    let elts_decreasing_width =
      List.dedup_and_sort ~compare:Itv.compare_width_decreasing elts
    in
    let tree = create () in
    let rec find_in_previous elt = function
      | [] -> parents tree.tbl tree.roots elt ~ancestors:[]
      | p :: ancestors when Itv.contains p elt ->
          parents tree.tbl [p] elt ~ancestors
      | _ :: px -> find_in_previous elt px
    in
    List.fold elts_decreasing_width ~init:[] ~f:(fun prev_ancestor elt ->
        let ancestors = find_in_previous elt prev_ancestor in
        ( match ancestors with
        | parent :: _ -> Hashtbl.add_multi tree.tbl ~key:parent ~data:elt
        | [] -> tree.roots <- elt :: tree.roots ) ;
        ancestors)
    |> (ignore : Itv.t list -> unit) ;
    let sort_itv_list = List.sort ~compare:Itv.compare_width_decreasing in
    { roots= sort_itv_list tree.roots
    ; tbl= Hashtbl.map tree.tbl ~f:sort_itv_list }

  let children {tbl; _} elt = Option.value ~default:[] (Hashtbl.find tbl elt)

  let dump tree =
    let open Fmt in
    let rec dump_ tree roots =
      vbox 0
        (list roots "@," (fun root ->
             let children = children tree root in
             vbox 1
               ( str (Sexp.to_string_hum (Itv.sexp_of_t root))
               $ wrap_if
                   (not (List.is_empty children))
                   "@,{" " }" (dump_ tree children) )))
    in
    if Conf.debug then set_margin 100000000 $ dump_ tree tree.roots
    else Fn.const ()
end

module Loc_tree = struct
  include Non_overlapping_interval_tree (Location)

  (* Use Ast_mapper to collect all locs in ast, and create tree of them. *)

  let of_ast map_ast ast =
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
    let mapper = Ast_mapper.{default_mapper with location; attribute} in
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
    of the first location is not greater than that of the second location. *)
let is_adjacent t (l1 : Location.t) (l2 : Location.t) =
  Option.value_map (Source.string_between t.source l1 l2) ~default:false
    ~f:(fun btw ->
      match String.strip btw with
      | "" -> true
      | "|" ->
          Source.begins_line t.source l1
          && Position.column l1.loc_start <= Position.column l2.loc_start
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
        let op_loc = {loc with loc_start} in
        let str = Source.string_at t.source op_loc in
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

let add_cmts t ?prev ?next tbl loc cmts =
  if not (CmtSet.is_empty cmts) then (
    let cmtl = CmtSet.to_list cmts in
    if Conf.debug then
      List.iter cmtl ~f:(fun {Cmt.txt= cmt_txt; loc= cmt_loc} ->
          let string_between (l1 : Location.t) (l2 : Location.t) =
            match Source.string_between t.source l1 l2 with
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
            ( if phys_equal tbl t.cmts_before then "before"
            else if phys_equal tbl t.cmts_after then "after"
            else "within" )
            Location.fmt loc Location.fmt cmt_loc (String.escaped btw_prev)
            cmt_txt (String.escaped btw_next)) ;
    Hashtbl.add_exn tbl ~key:loc ~data:cmtl )

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
            add_cmts t ~prev:prev_loc ~next:curr_loc t.cmts_after prev_loc
              after_prev ;
            before_curr
      in
      add_cmts t ?prev:prev_loc ~next:curr_loc t.cmts_before curr_loc
        before_curr ;
      ( match Loc_tree.children loc_tree curr_loc with
      | [] ->
          add_cmts t ?prev:prev_loc ~next:curr_loc t.cmts_within curr_loc
            within
      | children -> place t loc_tree children within ) ;
      place t loc_tree ~prev_loc:curr_loc next_locs after
  | [] -> (
    match prev_loc with
    | Some prev_loc -> add_cmts t ~prev:prev_loc t.cmts_after prev_loc cmts
    | None ->
        if Conf.debug then
          List.iter (CmtSet.to_list cmts) ~f:(fun {Cmt.txt; _} ->
              Format.eprintf "lost: %s@\n%!" txt) )

let remove = ref true

(** Relocate comments, for Ast transformations such as sugaring. *)
let relocate (t : t) ~src ~before ~after =
  if !remove then (
    let update_multi tbl src dst ~f =
      Option.iter (Hashtbl.find_and_remove tbl src) ~f:(fun src_data ->
          Hashtbl.update tbl dst ~f:(fun dst_data ->
              Option.fold dst_data ~init:src_data
                ~f:(fun src_data dst_data -> f src_data dst_data)))
    in
    if Conf.debug then
      Format.eprintf "relocate %a to %a and %a@\n%!" Location.fmt src
        Location.fmt before Location.fmt after ;
    let merge_and_sort x y =
      List.rev_append x y
      |> List.sort
           ~compare:(Comparable.lift Location.compare_start ~f:Cmt.loc)
    in
    update_multi t.cmts_before src before ~f:merge_and_sort ;
    update_multi t.cmts_after src after ~f:merge_and_sort ;
    update_multi t.cmts_within src after ~f:merge_and_sort ;
    if Conf.debug then (
      Hashtbl.remove t.remaining src ;
      Hashtbl.set t.remaining ~key:after ~data:() ;
      Hashtbl.set t.remaining ~key:before ~data:() ) )

(** Initialize global state and place comments. *)
let init map_ast source asts comments_n_docstrings =
  let t =
    { cmts_before= Hashtbl.create (module Location)
    ; cmts_after= Hashtbl.create (module Location)
    ; cmts_within= Hashtbl.create (module Location)
    ; source
    ; remaining= Hashtbl.create (module Location) }
  in
  let comments = Normalize.dedup_cmts map_ast asts comments_n_docstrings in
  if Conf.debug then (
    Format.eprintf "\nComments:\n%!" ;
    List.iter comments ~f:(fun {Cmt.txt; loc} ->
        Format.eprintf "%a %s %s@\n%!" Location.fmt loc txt
          (if Source.ends_line source loc then "eol" else "")) ) ;
  if not (List.is_empty comments) then (
    let loc_tree, locs = Loc_tree.of_ast map_ast asts in
    if Conf.debug then
      List.iter locs ~f:(fun loc ->
          if not (Location.compare loc Location.none = 0) then
            Hashtbl.set t.remaining ~key:loc ~data:()) ;
    if Conf.debug then (
      Format.eprintf "\nLoc_tree:\n%!" ;
      Format.eprintf "@\n%a@\n@\n%!" (Fn.flip Loc_tree.dump) loc_tree ) ;
    let locs = Loc_tree.roots loc_tree in
    let cmts = CmtSet.of_list comments in
    match locs with
    | [] -> add_cmts t ~prev:Location.none t.cmts_after Location.none cmts
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

let preserve fmt_x x =
  let buf = Buffer.create 128 in
  let fs = Format.formatter_of_buffer buf in
  let save = !remove in
  remove := false ;
  fmt_x x fs ;
  Format.pp_print_flush fs () ;
  remove := save ;
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

let fmt_cmt (conf : Conf.t) ~fmt_code (cmt : Cmt.t) =
  let open Fmt in
  let fmt_asterisk_prefixed_lines lines =
    vbox 1
      ( fmt "(*"
      $ list_pn lines (fun ?prev:_ line ?next ->
            match (line, next) with
            | "", None -> fmt ")"
            | _, None -> str line $ fmt "*)"
            | _, Some _ -> str line $ fmt "@,*") )
  in
  let fmt_non_code (cmt : Cmt.t) =
    if not conf.wrap_comments then
      match split_asterisk_prefixed cmt with
      | [""] | [_] | [_; ""] -> wrap "(*" "*)" (str cmt.txt)
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

(** Find, remove, and format comments for loc. *)
let fmt_cmts t (conf : Conf.t) ~fmt_code ?pro ?epi ?(eol = Fmt.fmt "@\n")
    ?(adj = eol) tbl loc =
  let open Fmt in
  if Conf.debug && !remove then Hashtbl.remove t.remaining loc ;
  let find = if !remove then Hashtbl.find_and_remove else Hashtbl.find in
  match find tbl loc with
  | None | Some [] -> noop
  | Some cmts ->
      let line_dist a b =
        b.Location.loc_start.pos_lnum - a.Location.loc_end.pos_lnum
      in
      let groups =
        List.group cmts ~break:(fun {Cmt.loc= a; _} {Cmt.loc= b; _} ->
            not
              ( Location.is_single_line a conf.margin
              && Location.is_single_line b conf.margin
              && line_dist a b = 1
              && Location.compare_start_col a b = 0
              && Location.compare_end_col a b = 0 ))
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
      list_pn groups (fun ?prev group ?next ->
          fmt_or_k (Option.is_none prev)
            (Option.call ~f:pro $ open_vbox 0)
            (fmt "@ ")
          $ ( match group with
            | [] -> impossible "previous match"
            | [cmt] -> fmt_cmt conf cmt ~fmt_code $ maybe_newline ~next cmt
            | group ->
                list group "@;<1000 0>" (fun cmt ->
                    wrap "(*" "*)" (str (Cmt.txt cmt)))
                $ maybe_newline ~next (List.last_exn group) )
          $ fmt_if_k (Option.is_none next)
              ( close_box
              $ fmt_or_k eol_cmt
                  (fmt_or_k adj_cmt adj eol)
                  (Option.call ~f:epi) ))

let fmt_before t conf ~fmt_code ?pro ?(epi = Fmt.break_unless_newline 1 0)
    ?eol ?adj =
  fmt_cmts t conf t.cmts_before ~fmt_code ?pro ~epi ?eol ?adj

let fmt_after t conf ~fmt_code ?(pro = Fmt.break_unless_newline 1 0) ?epi =
  let within = fmt_cmts t conf t.cmts_within ~fmt_code ~pro ?epi in
  let after =
    fmt_cmts t conf t.cmts_after ~fmt_code ~pro ?epi ~eol:Fmt.noop
  in
  fun loc -> within loc $ after loc

let fmt_within t conf ~fmt_code ?(pro = Fmt.break_unless_newline 1 0)
    ?(epi = Fmt.break_unless_newline 1 0) =
  fmt_cmts t conf t.cmts_within ~fmt_code ~pro ~epi ~eol:Fmt.noop

let fmt t conf ~fmt_code ?pro ?epi ?eol ?adj loc =
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
  let clear tbl =
    Hashtbl.map_inplace tbl ~f:(fun l ->
        List.filter l ~f:(fun {Cmt.loc= cmt_loc; _} ->
            not (Location.contains loc cmt_loc)))
  in
  clear t.cmts_before ; clear t.cmts_within ; clear t.cmts_after

let has_before t loc =
  if Conf.debug && !remove then Hashtbl.remove t.remaining loc ;
  Hashtbl.mem t.cmts_before loc

let has_within t loc =
  if Conf.debug && !remove then Hashtbl.remove t.remaining loc ;
  Hashtbl.mem t.cmts_within loc

let has_after t loc =
  if Conf.debug && !remove then Hashtbl.remove t.remaining loc ;
  Hashtbl.mem t.cmts_within loc || Hashtbl.mem t.cmts_after loc

(** returns comments that have not been formatted *)
let remaining_comments t =
  let get t before_after =
    Hashtbl.to_alist t
    |> List.concat_map ~f:(fun (ast_loc, cmts) ->
           List.map cmts ~f:(fun (cmt : Cmt.t) ->
               ( cmt
               , before_after
               , let open Sexp in
                 List
                   [ List [Atom "ast_loc"; Location.sexp_of_t ast_loc]
                   ; List [Atom "cmt_loc"; Location.sexp_of_t cmt.loc]
                   ; List [Atom "cmt_txt"; Atom cmt.txt] ] )))
  in
  List.concat
    [ get t.cmts_before "before"
    ; get t.cmts_within "within"
    ; get t.cmts_after "after" ]

let remaining_before t loc = Hashtbl.find_multi t.cmts_before loc

let remaining_locs t = Hashtbl.to_alist t.remaining |> List.map ~f:fst

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
