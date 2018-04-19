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

(** A tree of non-overlapping intervals. Intervals are non-overlapping if
    whenever 2 intervals share more than an end-point, then one contains the
    other. *)
module Non_overlapping_interval_tree (Itv : sig
  include Hashtbl.Key

  val contains : t -> t -> bool

  val compare_width_decreasing : t -> t -> int
end) : sig
  type t

  val of_list : Itv.t list -> t
  (** If there are duplicates in the input list, earlier elements will be
      ancestors of later elements. *)

  val children : t -> Itv.t -> Itv.t list

  val dump : t -> Fmt.t
  (** Debug: dump debug representation of tree. *)
end = struct
  (* simple but (asymptotically) suboptimal implementation *)

  type t = {mutable roots: Itv.t list; tbl: (Itv.t, Itv.t list) Hashtbl.t}

  let create () = {roots= []; tbl= Hashtbl.create (module Itv)}

  (* Descend tree from roots, find deepest node that contains elt. *)

  let rec parent tbl roots ?ancestor elt =
    Option.first_some
      (List.find_map roots ~f:(fun root ->
           if Itv.contains root elt then
             Hashtbl.find_and_call tbl root
               ~if_found:(fun children ->
                 parent tbl children ~ancestor:root elt )
               ~if_not_found:Option.some
           else None ))
      ancestor

  (* Add elements in decreasing width order to construct tree from roots to
     leaves. That is, when adding an interval to a partially constructed
     tree, it will already contain all wider intervals, so the new
     interval's parent will already be in the tree. *)

  let of_list elts =
    let elts_decreasing_width =
      List.sort ~compare:Itv.compare_width_decreasing
        (List.dedup_and_sort ~compare:Poly.compare elts)
    in
    let tree = create () in
    List.iter elts_decreasing_width ~f:(fun elt ->
        match parent tree.tbl tree.roots elt with
        | Some parent -> Hashtbl.add_multi tree.tbl ~key:parent ~data:elt
        | None -> tree.roots <- elt :: tree.roots ) ;
    { tree with
      tbl= Hashtbl.map tree.tbl ~f:(List.sort ~compare:Poly.compare) }

  let children {tbl} elt = Option.value ~default:[] (Hashtbl.find tbl elt)

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
                   "@,{" " }" (dump_ tree children) ) ))
    in
    if Conf.debug then set_margin 100000000 $ dump_ tree tree.roots
    else Fn.const ()
end

module Position = struct
  open Lexing

  let column {pos_bol; pos_cnum} = pos_cnum - pos_bol

  let fmt fs {pos_lnum; pos_bol; pos_cnum} =
    if pos_lnum = -1 then Format.fprintf fs "[%d]" pos_cnum
    else Format.fprintf fs "[%d,%d+%d]" pos_lnum pos_bol (pos_cnum - pos_bol)

  let compare p1 p2 =
    if phys_equal p1 p2 then 0 else Int.compare p1.pos_cnum p2.pos_cnum

  let distance p1 p2 = p2.pos_cnum - p1.pos_cnum
end

module Location = struct
  include Location

  let fmt fs {loc_start; loc_end; loc_ghost} =
    Format.fprintf fs "(%a..%a)%s" Position.fmt loc_start Position.fmt
      loc_end
      (if loc_ghost then " ghost" else "")

  let to_string x = Format.asprintf "%a" fmt x

  let sexp_of_t x = Sexp.Atom (to_string x)

  let compare = Poly.compare

  let hash = Hashtbl.hash

  let compare_start x y = Position.compare x.loc_start y.loc_start

  let compare_end x y = Position.compare x.loc_end y.loc_end

  let contains l1 l2 = compare_start l1 l2 <= 0 && compare_end l1 l2 >= 0

  let width x = Position.distance x.loc_start x.loc_end

  let compare_width_decreasing l1 l2 = Int.compare (width l2) (width l1)
end

module Loc_tree = struct
  include Non_overlapping_interval_tree (Location)

  (* Use Ast_mapper to collect all locs in ast, and create tree of them. *)

  let of_ast map_ast ast =
    let locs = ref [] in
    let location _ loc =
      locs := loc :: !locs ;
      loc
    in
    map_ast {Ast_mapper.default_mapper with location} ast |> ignore ;
    of_list !locs
end

module Cmt = struct
  module T = struct
    type t = string * Location.t

    let compare = Poly.compare

    let sexp_of_t (txt, loc) =
      Sexp.Atom (Format.asprintf "%s %a" txt Location.fmt loc)
  end

  include T
  include Comparator.Make (T)
end

(** Sets of comments supporting splitting by locations. *)
module CmtSet : sig
  type t

  val of_list : (string * Location.t) list -> t

  val to_list : t -> (string * Location.t) list
  (** ordered by start location *)

  val empty : t

  val is_empty : t -> bool

  val append : t -> t -> t

  val split : t -> Location.t -> t * t * t
  (** [split s {loc_start; loc_end}] splits [s] into the subset of comments
      that end before [loc_start], those that start after [loc_end], and
      those within the loc. *)

  include Invariant.S with type t := t
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

  type smap = (string * Location.t) list Map.M(Order_by_start).t

  type emap = (string * Location.t) list Map.M(Order_by_end).t

  type t = smap * emap

  let empty_start = Map.empty (module Order_by_start)

  let empty_end = Map.empty (module Order_by_end)

  let empty : t = (empty_start, empty_end)

  let invariant (smap, emap) =
    assert (
      List.equal ~equal:Poly.equal (Map.to_alist smap) (Map.to_alist emap)
    )

  let is_empty (smap, _) = Map.is_empty smap

  let of_list cmts =
    List.fold cmts ~init:empty ~f:(fun (smap, emap) cmt ->
        let _, loc = cmt in
        (Map.add_multi smap loc cmt, Map.add_multi emap loc cmt) )

  let to_list (smap, _) = List.concat (Map.data smap)

  let append (smap1, emap1) (smap2, emap2) =
    match
      ( Map.append ~lower_part:smap1 ~upper_part:smap2
      , Map.append ~lower_part:emap1 ~upper_part:emap2 )
    with
    | `Ok smap, `Ok emap -> (smap, emap)
    | _ -> internal_error "overlapping key ranges" []

  let split (smap, emap) (loc: Location.t) =
    let addo m kvo =
      Option.fold kvo ~init:m ~f:(fun m (key, data) -> Map.set m ~key ~data)
    in
    let partition dir (smap, emap) (loc: Location.t) =
      let before, equal, after = Map.split smap loc in
      let s_dir, s_ndir =
        match dir with
        | `Before -> (before, addo after equal)
        | `After -> (after, addo before equal)
      in
      let e_dir, e_ndir =
        Map.partitioni_tf emap ~f:(fun ~key ~data:_ -> Map.mem s_dir key)
      in
      ((s_dir, e_dir), (s_ndir, e_ndir))
    in
    let (s_after, e_after), (s_nafter, e_nafter) =
      partition `After (smap, emap) {loc with loc_start= loc.loc_end}
    in
    let (e_before, s_before), (e_within, s_within) =
      partition `Before (e_nafter, s_nafter)
        {loc with loc_end= loc.loc_start}
    in
    ((s_before, e_before), (s_within, e_within), (s_after, e_after))
end

(** Heuristic to determine if two locations should be considered "adjacent".
    Holds if there is only whitespace between the locations, or if there is
    a [|] character and the first location begins a line and the start
    column of the first location is not greater than that of the second
    location. *)
let is_adjacent (l1: Location.t) (l2: Location.t) =
  Option.value_map (Source.string_between l1 l2) ~default:false ~f:
    (fun btw ->
      match String.strip btw with
      | "" -> true
      | "|" ->
          Source.begins_line l1
          && Position.column l1.loc_start <= Position.column l2.loc_start
      | _ -> false )

(** Heuristic to choose between placing a comment after the previous loc or
    before the next loc. Places comment after prev loc only if they are
    "adjacent", and the comment and next loc are not "adjacent". *)
let partition_after_prev_or_before_next ~prev cmts ~next =
  match CmtSet.to_list cmts with
  | (_, loc) :: _ as cmtl when is_adjacent prev loc -> (
    match
      List.group cmtl ~break:(fun (_, l1) (_, l2) -> not (is_adjacent l1 l2))
    with
    | [cmtl] when is_adjacent (snd (List.last_exn cmtl)) next ->
        (CmtSet.empty, cmts)
    | after :: befores ->
        (CmtSet.of_list after, CmtSet.of_list (List.concat befores))
    | [] -> impossible "by parent match" )
  | _ -> (CmtSet.empty, cmts)

let cmts_before = Hashtbl.Poly.create ()

let cmts_after = Hashtbl.Poly.create ()

let add_cmts ?prev ?next tbl loc cmts =
  if not (CmtSet.is_empty cmts) then (
    let cmtl = CmtSet.to_list cmts in
    if Conf.debug then
      List.iter cmtl ~f:(fun (cmt_txt, cmt_loc) ->
          let string_between_inclusive (l1: Location.t) (l2: Location.t) =
            let pos = l1.loc_end.pos_cnum in
            let len = l2.loc_start.pos_cnum - l1.loc_end.pos_cnum in
            if len >= 0 then Source.sub ~pos ~len else "swapped"
          in
          let btw_prev =
            Option.value_map prev ~default:"no prev"
              ~f:(Fn.flip string_between_inclusive cmt_loc)
          in
          let btw_next =
            Option.value_map next ~default:"no next"
              ~f:(string_between_inclusive cmt_loc)
          in
          Format.eprintf "add %s %a: %a \"%s\" %s \"%s\"@\n"
            (if phys_equal tbl cmts_before then "before" else "after")
            Location.fmt loc Location.fmt cmt_loc (String.escaped btw_prev)
            cmt_txt (String.escaped btw_next) ) ;
    Hashtbl.add_exn tbl loc cmtl )

(** Traverse the location tree from locs, find the deepest location that
    contains each comment, intersperse comments between that location's
    children. *)
let rec place loc_tree ?prev_loc locs cmts =
  match locs with
  | curr_loc :: next_locs ->
      let before, within, after = CmtSet.split cmts curr_loc in
      let before_curr =
        match prev_loc with
        | None -> before
        | Some prev_loc ->
            let after_prev, before_curr =
              partition_after_prev_or_before_next ~prev:prev_loc before
                ~next:curr_loc
            in
            add_cmts ~prev:prev_loc ~next:curr_loc cmts_after prev_loc
              after_prev ;
            before_curr
      in
      add_cmts ?prev:prev_loc ~next:curr_loc cmts_before curr_loc
        before_curr ;
      let after =
        match Loc_tree.children loc_tree curr_loc with
        | [] -> CmtSet.append within after
        | children ->
            place loc_tree children within ;
            after
      in
      place loc_tree ~prev_loc:curr_loc next_locs after
  | [] ->
    match prev_loc with
    | Some prev_loc -> add_cmts ~prev:prev_loc cmts_after prev_loc cmts
    | None ->
        if Conf.debug then
          List.iter (CmtSet.to_list cmts) ~f:(fun (txt, _) ->
              Format.eprintf "lost: %s@\n" txt )

(** Remove comments that duplicate docstrings (or other comments). *)
let dedup_cmts map_ast ast comments =
  let of_ast map_ast ast =
    let docs = ref (Set.empty (module Cmt)) in
    let attribute _ atr =
      match atr with
      | ( {txt= "ocaml.doc" | "ocaml.text"}
        , PStr
            [ { pstr_desc=
                  Pstr_eval
                    ( { pexp_desc= Pexp_constant (Pconst_string (doc, None))
                      ; pexp_loc }
                    , [] ) } ] ) ->
          docs := Set.add !docs ("*" ^ doc, pexp_loc) ;
          atr
      | _ -> atr
    in
    map_ast {Ast_mapper.default_mapper with attribute} ast |> ignore ;
    !docs
  in
  Set.(to_list (diff (of_list (module Cmt) comments) (of_ast map_ast ast)))

let docs_memo = Hashtbl.Poly.create ()

(** Remember all docstrings that have been formatted, to avoid duplication
    in case of ambibuous placement. *)
let doc_is_dup doc =
  match Hashtbl.add docs_memo ~key:doc ~data:() with
  | `Ok -> false
  | `Duplicate -> true

(** Initialize global state and place comments. *)
let init map_ast loc_of_ast asts comments_n_docstrings =
  Hashtbl.clear docs_memo ;
  Hashtbl.clear cmts_before ;
  Hashtbl.clear cmts_after ;
  let comments = dedup_cmts map_ast asts comments_n_docstrings in
  if Conf.debug then
    List.iter comments ~f:(fun (txt, loc) ->
        Format.eprintf "%a %s %s@\n" Location.fmt loc txt
          (if Source.ends_line loc then "eol" else "") ) ;
  if not (List.is_empty comments) then (
    let loc_tree = Loc_tree.of_ast map_ast asts in
    if Conf.debug then
      Format.eprintf "@\n%a@\n@\n" (Fn.flip Loc_tree.dump) loc_tree ;
    let locs = List.map asts ~f:loc_of_ast in
    let cmts = CmtSet.of_list comments in
    place loc_tree locs cmts )

let init_impl = init map_structure (fun {Parsetree.pstr_loc} -> pstr_loc)

let init_intf = init map_signature (fun {Parsetree.psig_loc} -> psig_loc)

(** Relocate comments, for Ast transformations such as sugaring. *)
let relocate ~src ~before ~after =
  let update_multi tbl src dst ~f =
    Option.iter (Hashtbl.find_and_remove tbl src) ~f:(fun src_data ->
        Hashtbl.update tbl dst ~f:(fun dst_data ->
            Option.fold dst_data ~init:src_data ~f:(fun src_data dst_data ->
                f src_data dst_data ) ) )
  in
  if Conf.debug then
    Format.eprintf "relocate %a to %a and %a@\n" Location.fmt src
      Location.fmt before Location.fmt after ;
  update_multi cmts_before src before ~f:(fun src_cmts dst_cmts ->
      List.append src_cmts dst_cmts ) ;
  update_multi cmts_after src after ~f:(fun src_cmts dst_cmts ->
      List.append dst_cmts src_cmts )

let remove = ref true

let preserve f x =
  let save = !remove in
  remove := false ;
  f () x ;
  remove := save

let split_asterisk_prefixed (txt, {Location.loc_start}) =
  let len = Position.column loc_start + 3 in
  let pat =
    String.Search_pattern.create
      (String.init len ~f:(function
        | 0 -> '\n'
        | n when n < len - 1 -> ' '
        | _ -> '*' ))
  in
  let rec split_asterisk_prefixed_ pos =
    match String.Search_pattern.index pat ~pos ~in_:txt with
    | Some 0 -> "" :: split_asterisk_prefixed_ len
    | Some idx ->
        String.sub txt pos (idx - pos)
        :: split_asterisk_prefixed_ (idx + len)
    | _ ->
        let drop = function ' ' | '\t' -> true | _ -> false in
        let line = String.rstrip ~drop (String.drop_prefix txt pos) in
        if String.is_empty line then [line]
        else if Char.equal line.[String.length line - 1] '\n' then
          [String.drop_suffix line 1; ""]
        else if Char.is_whitespace txt.[String.length txt - 1] then
          [line ^ " "]
        else [line]
  in
  split_asterisk_prefixed_ 0

let fmt_cmt (c: Conf.t) cmt =
  let open Fmt in
  if not c.wrap_comments then wrap "(*" "*)" (str (fst cmt))
  else
    match split_asterisk_prefixed cmt with
    | [""] -> str "(* *)"
    | [text] -> wrap "(*" "*)" (fill_text text)
    | [text; ""] -> wrap "(*" " *)" (fill_text text)
    | asterisk_prefixed_lines ->
        vbox 1
          ( fmt "(*"
          $ list_pn asterisk_prefixed_lines (fun ?prev:_ line ?next ->
                match (line, next) with
                | "", None -> fmt ")"
                | _, None -> str line $ fmt "*)"
                | _, Some _ -> str line $ fmt "@,*" ) )

(** Find, remove, and format comments for loc. *)
let fmt_cmts c ?pro ?epi ?(eol= Fmt.fmt "@\n") ?(adj= eol) tbl loc =
  let open Fmt in
  let find = if !remove then Hashtbl.find_and_remove else Hashtbl.find in
  let cmts = Option.value (find tbl loc) ~default:[] in
  let last_cmt = List.last cmts in
  let eol_cmt =
    Option.value ~default:false
      (last_cmt >>| fun (_, loc) -> Source.ends_line loc)
  in
  let adj_cmt =
    eol_cmt
    && Option.value ~default:false
         ( last_cmt
         >>| fun (_, {Location.loc_end= {pos_lnum}}) ->
         pos_lnum + 1 = loc.Location.loc_start.pos_lnum )
  in
  fmt_if_k
    (not (List.is_empty cmts))
    ( Option.call ~f:pro
    $ vbox 0 (list cmts "@ " (fmt_cmt c))
    $ fmt_or_k eol_cmt (fmt_or_k adj_cmt adj eol) (Option.call ~f:epi) )

let fmt_before c ?pro ?(epi= Fmt.break_unless_newline 1 0) ?eol ?adj =
  fmt_cmts c cmts_before ?pro ~epi ?eol ?adj

let fmt_after c ?(pro= Fmt.break_unless_newline 1 0) ?epi =
  fmt_cmts c cmts_after ~pro ?epi ~eol:(Fmt.fmt "")

let fmt c ?pro ?epi ?eol ?adj loc =
  Fmt.wrap_k
    (fmt_before c ?pro ?epi ?eol ?adj loc)
    (fmt_after c ?pro ?epi loc)

let fmt_list c ?pro ?epi ?eol locs init =
  List.fold locs ~init ~f:(fun k loc -> fmt c ?pro ?epi ?eol loc @@ k)

(** check if any comments have not been formatted *)
let final_check () =
  if not (Hashtbl.is_empty cmts_before && Hashtbl.is_empty cmts_after) then
    let f before_after ~key:ast_loc ~data init =
      List.fold data ~init ~f:(fun z (cmt_txt, cmt_loc) ->
          let open Sexp in
          ( before_after
          , List
              [ List [Atom "ast_loc"; Location.sexp_of_t ast_loc]
              ; List [Atom "cmt_loc"; Location.sexp_of_t cmt_loc]
              ; List [Atom "cmt_txt"; Atom cmt_txt] ] )
          :: z )
    in
    internal_error "formatting lost comments"
      (Hashtbl.fold cmts_before ~f:(f "before")
         ~init:(Hashtbl.fold cmts_after ~f:(f "after") ~init:[]))

let diff x y =
  let norm z =
    (* normalize consecutive whitespace chars to a single space *)
    let f (txt, _) =
      String.concat ~sep:" "
        (List.filter ~f:(Fn.non String.is_empty)
           (String.split_on_chars txt
              ~on:['\t'; '\n'; '\011'; '\012'; '\r'; ' ']))
    in
    Set.of_list
      (module String)
      (List.map ~f (List.dedup_and_sort ~compare:Poly.compare z))
  in
  Set.symmetric_diff (norm x) (norm y)
