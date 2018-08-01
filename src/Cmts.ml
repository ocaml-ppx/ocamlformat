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
  { cmts_before: (Location.t, (string * Location.t) list) Hashtbl.t
  ; cmts_after: (Location.t, (string * Location.t) list) Hashtbl.t
  ; cmts_within: (Location.t, (string * Location.t) list) Hashtbl.t
  ; source: Source.t
  ; docs_memo: (string loc, unit) Hashtbl.t
  ; conf: Conf.t }

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
    let attribute (m: Ast_mapper.mapper) attr =
      match attr with
      | ( {txt= ("ocaml.doc" | "ocaml.text") as txt}
        , PStr
            [ { pstr_desc=
                  Pstr_eval
                    ( { pexp_desc= Pexp_constant (Pconst_string (doc, None))
                      ; pexp_attributes }
                    , [] ) } ] ) ->
          (* ignore location of docstrings *)
          ( {txt; loc= Location.none}
          , m.payload m
              (PStr
                 [ { pstr_desc=
                       Pstr_eval
                         ( { pexp_desc=
                               Pexp_constant (Pconst_string (doc, None))
                           ; pexp_loc= Location.none
                           ; pexp_attributes= m.attributes m pexp_attributes
                           }
                         , [] )
                   ; pstr_loc= Location.none } ]) )
      | attr -> Ast_mapper.default_mapper.attribute m attr
    in
    let locs = ref [] in
    let location _ loc =
      locs := loc :: !locs ;
      loc
    in
    map_ast
      Ast_mapper.{default_mapper with location}
      (map_ast Ast_mapper.{default_mapper with attribute} ast)
    |> ignore ;
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
        ( Map.add_multi smap ~key:loc ~data:cmt
        , Map.add_multi emap ~key:loc ~data:cmt ) )

  let to_list (smap, _) = List.concat (Map.data smap)

  let split (t: t) (loc: Location.t) =
    let addo m kvo =
      Option.fold kvo ~init:m ~f:(fun m (key, data) -> Map.set m ~key ~data)
    in
    let partition ((smap, emap): t) (loc: Location.t) =
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
    Holds if there is only whitespace between the locations, or if there is
    a [|] character and the first location begins a line and the start
    column of the first location is not greater than that of the second
    location. *)
let is_adjacent t (l1: Location.t) (l2: Location.t) =
  Option.value_map (Source.string_between t.source l1 l2) ~default:false ~f:
    (fun btw ->
      match String.strip btw with
      | "" -> true
      | "|" ->
          Source.begins_line t.source l1
          && Position.column l1.loc_start <= Position.column l2.loc_start
      | _ -> false )

(** Heuristic to choose between placing a comment after the previous loc or
    before the next loc. Places comment after prev loc only if they are
    "adjacent", and the comment and next loc are not "adjacent". *)
let partition_after_prev_or_before_next t ~prev cmts ~next =
  match CmtSet.to_list cmts with
  | (_, loc) :: _ as cmtl when is_adjacent t prev loc -> (
    match
      List.group cmtl ~break:(fun (_, l1) (_, l2) ->
          not (is_adjacent t l1 l2) )
    with
    | [cmtl] when is_adjacent t (snd (List.last_exn cmtl)) next ->
        (CmtSet.empty, cmts)
    | after :: befores ->
        (CmtSet.of_list after, CmtSet.of_list (List.concat befores))
    | [] -> impossible "by parent match" )
  | _ -> (CmtSet.empty, cmts)

let add_cmts t ?prev ?next tbl loc cmts =
  if not (CmtSet.is_empty cmts) then (
    let cmtl = CmtSet.to_list cmts in
    if Conf.debug then
      List.iter cmtl ~f:(fun (cmt_txt, cmt_loc) ->
          let string_between (l1: Location.t) (l2: Location.t) =
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
            cmt_txt (String.escaped btw_next) ) ;
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
  | [] ->
    match prev_loc with
    | Some prev_loc -> add_cmts t ~prev:prev_loc t.cmts_after prev_loc cmts
    | None ->
        if Conf.debug then
          List.iter (CmtSet.to_list cmts) ~f:(fun (txt, _) ->
              Format.eprintf "lost: %s@\n%!" txt )

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

(** Remember all docstrings that have been formatted, to avoid duplication
    in case of ambibuous placement. *)
let doc_is_dup t doc =
  match Hashtbl.add t.docs_memo ~key:doc ~data:() with
  | `Ok -> false
  | `Duplicate -> true

(** Initialize global state and place comments. *)
let init map_ast loc_of_ast source conf asts comments_n_docstrings =
  let t =
    { docs_memo= Hashtbl.Poly.create ()
    ; cmts_before= Hashtbl.Poly.create ()
    ; cmts_after= Hashtbl.Poly.create ()
    ; cmts_within= Hashtbl.Poly.create ()
    ; source
    ; conf }
  in
  let comments = dedup_cmts map_ast asts comments_n_docstrings in
  if Conf.debug then
    List.iter comments ~f:(fun (txt, loc) ->
        Format.eprintf "%a %s %s@\n%!" Location.fmt loc txt
          (if Source.ends_line source loc then "eol" else "") ) ;
  if not (List.is_empty comments) then (
    let loc_tree = Loc_tree.of_ast map_ast asts in
    if Conf.debug then
      Format.eprintf "@\n%a@\n@\n%!" (Fn.flip Loc_tree.dump) loc_tree ;
    let locs = loc_of_ast asts in
    let cmts = CmtSet.of_list comments in
    place t loc_tree locs cmts ) ;
  t

let init_impl =
  init map_structure (List.map ~f:(fun {Parsetree.pstr_loc} -> pstr_loc))

let init_intf =
  init map_signature (List.map ~f:(fun {Parsetree.psig_loc} -> psig_loc))

let init_use_file =
  init Migrate_ast.map_use_file
    (List.concat_map ~f:(fun toplevel_phrase ->
         match (toplevel_phrase : toplevel_phrase) with
         | Ptop_def items ->
             List.map items ~f:(fun {Parsetree.pstr_loc} -> pstr_loc)
         | Ptop_dir _ -> [] ))

(** Relocate comments, for Ast transformations such as sugaring. *)
let relocate t ~src ~before ~after =
  let update_multi tbl src dst ~f =
    Option.iter (Hashtbl.find_and_remove tbl src) ~f:(fun src_data ->
        Hashtbl.update tbl dst ~f:(fun dst_data ->
            Option.fold dst_data ~init:src_data ~f:(fun src_data dst_data ->
                f src_data dst_data ) ) )
  in
  if Conf.debug then
    Format.eprintf "relocate %a to %a and %a@\n%!" Location.fmt src
      Location.fmt before Location.fmt after ;
  update_multi t.cmts_before src before ~f:(fun src_cmts dst_cmts ->
      List.append src_cmts dst_cmts ) ;
  update_multi t.cmts_after src after ~f:(fun src_cmts dst_cmts ->
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
        String.sub txt ~pos ~len:(idx - pos)
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

let fmt_cmt t cmt =
  let open Fmt in
  if not t.conf.wrap_comments then wrap "(*" "*)" (str (fst cmt))
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
let fmt_cmts t ?pro ?epi ?(eol= Fmt.fmt "@\n") ?(adj= eol) tbl loc =
  let open Fmt in
  let find = if !remove then Hashtbl.find_and_remove else Hashtbl.find in
  let cmts = Option.value (find tbl loc) ~default:[] in
  let last_cmt = List.last cmts in
  let eol_cmt =
    Option.value ~default:false
      (last_cmt >>| fun (_, loc) -> Source.ends_line t.source loc)
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
    $ vbox 0 (list cmts "@ " (fmt_cmt t))
    $ fmt_or_k eol_cmt (fmt_or_k adj_cmt adj eol) (Option.call ~f:epi) )

let fmt_before t ?pro ?(epi= Fmt.break_unless_newline 1 0) ?eol ?adj =
  fmt_cmts t t.cmts_before ?pro ~epi ?eol ?adj

let fmt_after t ?(pro= Fmt.break_unless_newline 1 0) ?epi =
  let within = fmt_cmts t t.cmts_within ~pro ?epi in
  let after = fmt_cmts t t.cmts_after ~pro ?epi ~eol:(Fmt.fmt "") in
  fun loc -> within loc $ after loc

let fmt_within t ?(pro= Fmt.break_unless_newline 1 0)
    ?(epi= Fmt.break_unless_newline 1 0) =
  fmt_cmts t t.cmts_within ~pro ~epi ~eol:(Fmt.fmt "")

let fmt t ?pro ?epi ?eol ?adj loc =
  (* remove the before comments from the map first *)
  let before = fmt_before t ?pro ?epi ?eol ?adj loc in
  (* remove the within comments from the map by accepting the continuation *)
  fun k ->
    (* delay the after comments until the within comments have been removed *)
    let after = fmt_after t ?pro ?epi loc in
    let inner = k in
    before $ inner $ after

let fmt_list t ?pro ?epi ?eol locs init =
  List.fold locs ~init ~f:(fun k loc -> fmt t ?pro ?epi ?eol loc @@ k)

(** returns comments that have not been formatted *)
let remaining_comments t =
  let get t before_after =
    Hashtbl.to_alist t
    |> List.concat_map ~f:(fun (ast_loc, cmts) ->
           List.map cmts ~f:(fun (cmt_txt, cmt_loc) ->
               ( before_after
               , let open Sexp in
                 List
                   [ List [Atom "ast_loc"; Location.sexp_of_t ast_loc]
                   ; List [Atom "cmt_loc"; Location.sexp_of_t cmt_loc]
                   ; List [Atom "cmt_txt"; Atom cmt_txt] ] ) ) )
  in
  List.concat
    [ get t.cmts_before "before"
    ; get t.cmts_within "within"
    ; get t.cmts_after "after" ]

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
