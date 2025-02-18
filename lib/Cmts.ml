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

open Migrate_ast

type layout_cache_key =
  | Arg of Asttypes.arg_label * Parsetree.expression
  | Pattern of Parsetree.pattern
  | Expression of Parsetree.expression

module Layout_cache = struct
  module Key = struct
    type t = layout_cache_key

    let expression_to_string e = Format.asprintf "%a" Printast.expression e

    let pattern_to_string e = Format.asprintf "%a" Printast.pattern e

    let sexp_of_arg_label = function
      | Asttypes.Nolabel -> Sexp.Atom "Nolabel"
      | Labelled label -> List [Atom "Labelled"; sexp_of_string label.txt]
      | Optional label -> List [Atom "Optional"; sexp_of_string label.txt]

    let sexp_of_t = function
      | Arg (label, expression) ->
          Sexp.List
            [ Atom "Arg"
            ; sexp_of_arg_label label
            ; sexp_of_string (expression_to_string expression) ]
      | Pattern pattern ->
          List [Atom "Pattern"; sexp_of_string (pattern_to_string pattern)]
      | Expression expression ->
          List
            [ Atom "Expression"
            ; sexp_of_string (expression_to_string expression) ]

    let compare = Poly.compare

    let hash = Hashtbl.hash
  end

  let create () = Hashtbl.create (module Key)

  type t = string Hashtbl.M(Key).t

  let memo t ~key ~f = Hashtbl.find_or_add t key ~default:f
end

type t =
  { debug: bool
  ; mutable cmts_before: Cmt.t Multimap.M(Location).t
  ; mutable cmts_after: Cmt.t Multimap.M(Location).t
  ; mutable cmts_within: Cmt.t Multimap.M(Location).t
  ; source: Source.t
  ; mutable remaining: Set.M(Location).t
  ; layout_cache: Layout_cache.t }

let copy
    { debug
    ; cmts_before
    ; cmts_after
    ; cmts_within
    ; source
    ; remaining
    ; layout_cache } =
  { debug
  ; cmts_before
  ; cmts_after
  ; cmts_within
  ; source
  ; remaining
  ; layout_cache }

let restore src ~into =
  into.cmts_before <- src.cmts_before ;
  into.cmts_after <- src.cmts_after ;
  into.cmts_within <- src.cmts_within ;
  into.remaining <- src.remaining

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
  Map.find map loc

let no_filter (_ : Parser.token) = true

(** Heuristic to determine if two locations should be considered "adjacent".
    Holds if there is only whitespace between the locations, or if there is a
    [|] character and the first location begins a line and the start column
    of the first location is lower than that of the second location. *)
let is_adjacent ?(filter = no_filter) src (l1 : Location.t) (l2 : Location.t)
    =
  match Source.tokens_between src l1.loc_end l2.loc_start ~filter with
  | [] -> true
  | [(BAR, _)] ->
      Source.begins_line src l1
      && Position.column l1.loc_start < Position.column l2.loc_start
  | _ -> false

(** Whether the symbol preceding location [loc] is an infix symbol
    (corresponding to [Ast.String_id.is_infix]) or a semicolon. If it is the
    case, comments attached to the following item should be kept after the
    infix symbol. *)
let infix_symbol_before src (loc : Location.t) =
  match
    Source.find_token_before src ~filter:(function _ -> true) loc.loc_start
  with
  | Some (Parser.SEMI, loc') ->
      Source.begins_line ~ignore_spaces:true src loc'
  | Some (x, _) -> Ast.Token.is_infix x
  | None -> false

(** Sets of comments supporting splitting by locations. *)
module CmtSet : sig
  type t

  val of_list : Cmt.t list -> t

  val to_list : t -> Cmt.t list
  (** ordered by start location *)

  val is_empty : t -> bool

  val split : t -> Location.t -> t * t * t
  (** [split s {loc_start; loc_end}] splits [s] into the subset of comments
      that end before [loc_start], those that start after [loc_end], and
      those within the loc. *)

  val partition :
    Source.t -> prev:Location.t -> next:Location.t -> t -> t * t
  (** Heuristic to choose between placing a comment after the previous
      location or before the next one. *)
end = struct
  type t = Cmt.t list Map.M(Position).t

  let empty = Map.empty (module Position)

  let is_empty = Map.is_empty

  let of_list cmts =
    List.fold cmts ~init:empty ~f:(fun map cmt ->
        let pos = (Cmt.loc cmt).loc_start in
        Map.add_multi map ~key:pos ~data:cmt )

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

  let partition src ~prev ~next cmts =
    let ignore_docstrings = function
      | Parser.DOCSTRING _ -> false
      | _ -> true
    in
    match to_list cmts with
    | cmt :: _ as cmtl
      when is_adjacent ~filter:ignore_docstrings src prev (Cmt.loc cmt) -> (
      match
        List.group cmtl ~break:(fun l1 l2 ->
            not (is_adjacent src (Cmt.loc l1) (Cmt.loc l2)) )
      with
      | [cmtl] when is_adjacent src (Cmt.loc (List.last_exn cmtl)) next ->
          let open Location in
          let first_loc = Cmt.loc (List.hd_exn cmtl) in
          let last_loc = Cmt.loc (List.last_exn cmtl) in
          let same_line_as_prev l =
            prev.loc_end.pos_lnum = l.loc_start.pos_lnum
          in
          let decide loc =
            match
              ( loc.loc_start.pos_lnum - prev.loc_end.pos_lnum
              , next.loc_start.pos_lnum - loc.loc_end.pos_lnum )
            with
            | 0, 0 -> `Before_next
            | 0, _ when infix_symbol_before src loc -> `Before_next
            | 0, _ -> `After_prev
            | 1, 1 ->
                if
                  Location.compare_start_col
                    (Cmt.loc (List.last_exn cmtl))
                    next
                  <= 0
                then `Before_next
                else `After_prev
            | 1, y when y > 1 && Source.empty_line_after src loc ->
                `After_prev
            | _, y
              when y > 1
                   && first_loc.loc_start.pos_lnum - prev.loc_end.pos_lnum
                      >= 1
                   && Source.empty_line_after src last_loc
                   && not (Source.empty_line_before src first_loc) ->
                `After_prev
            | _ -> `Before_next
          in
          let prev, next =
            if not (same_line_as_prev next) then
              let next, prev =
                List.partition_tf cmtl ~f:(fun cmt ->
                    match decide (Cmt.loc cmt) with
                    | `After_prev -> false
                    | `Before_next -> true )
              in
              (prev, next)
            else ([], cmtl)
          in
          (of_list prev, of_list next)
      | after :: befores -> (of_list after, of_list (List.concat befores))
      | [] -> impossible "by parent match" )
    | _ -> (empty, cmts)
end

let add_cmts t position loc ?deep_loc cmts =
  if not (CmtSet.is_empty cmts) then
    let cmtl = CmtSet.to_list cmts in
    let key =
      match deep_loc with
      | Some deep_loc ->
          let cmt_loc = Cmt.loc (List.last_exn cmtl) in
          if
            is_adjacent t.source deep_loc cmt_loc
            && not (Source.begins_line ~ignore_spaces:true t.source cmt_loc)
          then deep_loc
          else loc
      | None -> loc
    in
    update_cmts t position ~f:(Map.add_exn ~key ~data:cmtl)

(** Traverse the location tree from locs, find the deepest location that
    contains each comment, intersperse comments between that location's
    children. *)
let rec place t loc_tree ?prev_loc ?deep_loc locs cmts =
  match locs with
  | curr_loc :: next_locs ->
      let before, within, after = CmtSet.split cmts curr_loc in
      let before_curr =
        match prev_loc with
        | None -> before
        (* Location.none is a special case, it shouldn't be the location of a
           previous element, so if a comment is at the beginning of the file,
           it should be placed before the next element. *)
        | Some prev_loc when Location.(compare prev_loc none) = 0 -> before
        | Some prev_loc ->
            let after_prev, before_curr =
              CmtSet.partition t.source ~prev:prev_loc ~next:curr_loc before
            in
            add_cmts t `After prev_loc after_prev ?deep_loc ;
            before_curr
      in
      add_cmts t `Before curr_loc before_curr ;
      let deep_loc =
        match Loc_tree.children loc_tree curr_loc with
        | [] ->
            add_cmts t `Within curr_loc within ;
            Option.some_if (List.is_empty next_locs) curr_loc
        | children -> place t loc_tree children within
      in
      place t loc_tree ~prev_loc:curr_loc next_locs after ?deep_loc
  | [] ->
      ( match prev_loc with
      | Some prev_loc -> add_cmts t `After prev_loc cmts ?deep_loc
      | None ->
          if t.debug then
            List.iter (CmtSet.to_list cmts) ~f:(fun cmt ->
                Format_.eprintf "lost: %s@\n%!" (Cmt.txt cmt) ) ) ;
      deep_loc

(** Relocate comments, for Ast transformations such as sugaring. *)
let relocate (t : t) ~src ~before ~after =
  if t.debug then
    Format.eprintf "relocate %a to %a and %a@\n%!" Location.fmt src
      Location.fmt before Location.fmt after ;
  let merge_and_sort x y =
    List.rev_append x y
    |> List.sort ~compare:(Comparable.lift Location.compare_start ~f:Cmt.loc)
  in
  update_cmts t `Before
    ~f:(Multimap.update_multi ~src ~dst:before ~f:merge_and_sort) ;
  update_cmts t `After
    ~f:(Multimap.update_multi ~src ~dst:after ~f:merge_and_sort) ;
  update_cmts t `Within
    ~f:(Multimap.update_multi ~src ~dst:after ~f:merge_and_sort) ;
  if t.debug then
    update_remaining t ~f:(fun s ->
        let s = Set.remove s src in
        let s = Set.add s after in
        Set.add s before )

let relocate_cmts_before (t : t) ~src ~sep ~dst =
  let f map =
    Multimap.partition_multi map ~src ~dst ~f:(fun cmt ->
        Location.compare_end (Cmt.loc cmt) sep < 0 )
  in
  update_cmts t `Before ~f ; update_cmts t `Within ~f

let relocate_pattern_matching_cmts (t : t) src tok ~whole_loc ~matched_loc =
  if not (whole_loc.Location.loc_ghost || matched_loc.Location.loc_ghost)
  then
    let kwd_loc =
      Option.value_exn (Source.loc_of_first_token_at src whole_loc tok)
    in
    relocate_cmts_before t ~src:matched_loc ~sep:kwd_loc ~dst:whole_loc

let relocate_ext_cmts (t : t) src (pre, pld) ~whole_loc =
  let open Extended_ast in
  match pld with
  | PStr
      [ { pstr_desc=
            Pstr_eval
              ( { pexp_desc= Pexp_constant {pconst_desc= Pconst_string _; _}
                ; pexp_loc= _
                ; pexp_loc_stack= _
                ; pexp_attributes= _ }
              , [] )
        ; pstr_loc } ]
    when Source.is_quoted_string src pstr_loc ->
      ()
  | PStr
      [ { pstr_desc=
            Pstr_eval
              ( { pexp_desc= Pexp_sequence (e1, _)
                ; pexp_loc= _
                ; pexp_loc_stack= _
                ; pexp_attributes }
              , [] )
        ; pstr_loc= _ } ]
    when List.is_empty pexp_attributes
         && Source.extension_using_sugar ~name:pre ~payload:e1.pexp_loc ->
      ()
  | PStr [{pstr_desc= Pstr_eval _; pstr_loc; _}] ->
      let kwd_loc =
        match Source.loc_of_first_token_at src whole_loc LBRACKETPERCENT with
        | Some loc -> loc
        | None -> (
          match Source.loc_of_first_token_at src whole_loc PERCENT with
          | Some loc -> loc
          | None -> impossible "expect token starting extension" )
      in
      relocate_cmts_before t ~src:pstr_loc ~sep:kwd_loc ~dst:whole_loc
  | _ -> ()

let relocate_wrongfully_attached_cmts t src exp =
  let open Extended_ast in
  match exp.pexp_desc with
  | Pexp_match (e0, _) ->
      relocate_pattern_matching_cmts t src Parser.MATCH
        ~whole_loc:exp.pexp_loc ~matched_loc:e0.pexp_loc
  | Pexp_try (e0, _) ->
      relocate_pattern_matching_cmts t src Parser.TRY ~whole_loc:exp.pexp_loc
        ~matched_loc:e0.pexp_loc
  | Pexp_extension ext -> relocate_ext_cmts t src ext ~whole_loc:exp.pexp_loc
  | _ -> ()

(** Initialize global state and place comments. *)
let init fragment ~debug source asts comments_n_docstrings =
  let t =
    { debug
    ; cmts_before= Map.empty (module Location)
    ; cmts_after= Map.empty (module Location)
    ; cmts_within= Map.empty (module Location)
    ; source
    ; remaining= Set.empty (module Location)
    ; layout_cache= Layout_cache.create () }
  in
  let comments =
    Normalize_extended_ast.dedup_cmts fragment asts comments_n_docstrings
  in
  if not (List.is_empty comments) then (
    let loc_tree, locs = Loc_tree.of_ast fragment asts in
    if debug then
      List.iter locs ~f:(fun loc ->
          if not (Location.compare loc Location.none = 0) then
            update_remaining t ~f:(fun s -> Set.add s loc) ) ;
    let locs = Loc_tree.roots loc_tree in
    let cmts = CmtSet.of_list comments in
    ( match locs with
    | [] -> add_cmts t `After Location.none cmts
    | _ -> ignore @@ place t loc_tree locs cmts ) ;
    if debug then (
      let get_cmts pos loc =
        let cmts = find_at_position t loc pos in
        Option.map cmts ~f:(List.map ~f:Cmt.txt)
      in
      let cmts : Printast.cmts =
        { before= get_cmts `Before
        ; within= get_cmts `Within
        ; after= get_cmts `After }
      in
      Printast.cmts := Some cmts ;
      Format.eprintf "AST:\n%a\n%!" (Extended_ast.Printast.ast fragment) asts
      ) ) ;
  t

let preserve_nomemo f t =
  let original = copy t in
  let finally () = restore original ~into:t in
  Exn.protect ~finally ~f:(fun () ->
      let buf = Buffer.create 128 in
      let fs = Format_.formatter_of_buffer buf in
      Fmt.eval fs (f ()) ;
      Format_.pp_print_flush fs () ;
      Buffer.contents buf )

let preserve ~cache_key f t =
  Layout_cache.memo t.layout_cache ~key:cache_key ~f:(fun () ->
      preserve_nomemo f t )

let pop_if_debug t loc =
  if t.debug then update_remaining t ~f:(fun s -> Set.remove s loc)

let find_cmts ?(filter = Fn.const true) t pos loc =
  pop_if_debug t loc ;
  Option.map (find_at_position t loc pos) ~f:(fun cmts ->
      let picked, not_picked = List.partition_tf cmts ~f:filter in
      update_cmts t pos ~f:(Map.set ~key:loc ~data:not_picked) ;
      picked )

let break_comment_group source a b =
  let a = Cmt.loc a and b = Cmt.loc b in
  let vertical_align =
    Location.line_difference a b = 1 && Location.compare_start_col a b = 0
  in
  let horizontal_align =
    Location.line_difference a b = 0
    && List.is_empty
         (Source.tokens_between source a.loc_end b.loc_start
            ~filter:(function _ -> true ))
  in
  not (vertical_align || horizontal_align)

let is_only_whitespaces s = String.for_all s ~f:Char.is_whitespace

module Wrapped = struct
  let fmt ~pro ~epi text =
    let open Fmt in
    assert (not (String.is_empty text)) ;
    let prefix = if String.starts_with_whitespace text then " " else ""
    and suffix = if String.ends_with_whitespace text then " " else "" in
    let fmt_line line =
      let words =
        List.filter ~f:(Fn.non String.is_empty)
          (String.split_on_chars line
             ~on:['\t'; '\n'; '\011'; '\012'; '\r'; ' '] )
      in
      list words space_break str
    in
    let lines =
      List.remove_consecutive_duplicates
        ~equal:(fun x y -> String.is_empty x && String.is_empty y)
        (String.split (String.rstrip text) ~on:'\n')
    in
    let groups =
      List.group lines ~break:(fun _ y -> is_only_whitespaces y)
    in
    pro $ str prefix
    $ hovbox 0
        (list_fl groups (fun ~first ~last:last_group group ->
             let group = List.filter group ~f:(Fn.non is_only_whitespaces) in
             fmt_if (not first) (str "\n" $ force_newline)
             $ hovbox 0
                 (list_fl group (fun ~first ~last x ->
                      fmt_if (not first) space_break
                      $ fmt_line x
                      $ fmt_if (last_group && last) (str suffix $ epi) ) ) )
        )
end

module Asterisk_prefixed = struct
  open Fmt

  let fmt_line ~first:_ ~last s =
    if last && is_only_whitespaces s then cut_break
    else cut_break $ str "*" $ str s

  let fmt ~pro ~epi = function
    | hd :: tl -> vbox 1 (pro $ str hd $ list_fl tl fmt_line $ epi)
    | [] -> noop
end

module Unwrapped = struct
  open Fmt

  let fmt_line ~first:_ ~last:_ l =
    (* The last line will be followed by the [epi]. *)
    str "\n" $ str l

  (** [txt] contains trailing spaces and leading/trailing empty lines. *)
  let fmt ~pro ~epi txt =
    match String.split ~on:'\n' txt with
    | hd :: tl ->
        vbox 0 ~name:"unwrapped" (pro $ str hd $ list_fl tl fmt_line) $ epi
    | [] -> noop
end

module Verbatim = struct
  let fmt ~pro ~epi s =
    let open Fmt in
    pro $ str s $ epi
end

module Cinaps = struct
  open Fmt

  (** Comments enclosed in [(*$], [$*)] are formatted as code. *)
  let fmt ~pro ~epi ~fmt_code conf ~offset code =
    match fmt_code conf ~offset ~set_margin:false code with
    | Ok formatted ->
        hvbox 0
          (pro $ hvbox (-1) (space_break $ formatted) $ space_break $ epi)
    | Error _ -> Verbatim.fmt ~pro ~epi code
end

module Doc = struct
  let fmt ~pro ~epi ~fmt_code conf ~loc txt ~offset =
    (* Whether the doc starts and ends with an empty line. *)
    let pre_nl, trail_nl =
      let lines = String.split ~on:'\n' txt in
      match lines with
      | [] | [_] -> (false, false)
      | h :: _ ->
          let l = List.last_exn lines in
          (is_only_whitespaces h, is_only_whitespaces l)
    in
    let force_trailing_space =
      String.length txt > 1 && String.ends_with_whitespace txt
    in
    let txt = if pre_nl then String.lstrip txt else txt in
    let txt = if trail_nl then String.rstrip txt else txt in
    let parsed = Docstring.parse ~loc txt in
    (* Disable warnings when parsing of code blocks fails. *)
    let quiet = Conf_t.Elt.make true `Default in
    let conf = {conf with Conf.opr_opts= {conf.Conf.opr_opts with quiet}} in
    let open Fmt in
    let trailing_space =
      if (not trail_nl) && force_trailing_space then str " " else noop
    in
    let doc =
      Fmt_odoc.fmt_parsed conf ~trailing_space ~fmt_code ~input:txt ~offset
        parsed
    in
    hvbox 2
      ( pro
      $ fmt_if pre_nl (break 1000 1)
      $ doc
      $ fmt_if trail_nl (break 1000 (-2))
      $ epi )
end

let fmt_cmt (conf : Conf.t) cmt ~fmt_code =
  let open Fmt in
  let decoded = Cmt.decode cmt in
  (* TODO: Offset should be computed from location. *)
  let offset = 2 + String.length decoded.prefix in
  let pro = str "(*" $ str decoded.prefix
  and epi = str decoded.suffix $ str "*)" in
  let fmt_doc txt =
    Doc.fmt ~pro ~epi ~fmt_code conf ~loc:(Cmt.loc cmt) txt ~offset
  in
  match decoded.kind with
  | Verbatim txt -> Verbatim.fmt ~pro ~epi txt
  | Doc txt -> fmt_doc txt
  | Normal txt ->
      if
        conf.fmt_opts.ocp_indent_compat.v && conf.fmt_opts.parse_docstrings.v
      then fmt_doc txt
      else if conf.fmt_opts.wrap_comments.v then Wrapped.fmt ~pro ~epi txt
      else Unwrapped.fmt ~pro ~epi txt
  | Code code -> Cinaps.fmt ~pro ~epi ~fmt_code conf ~offset code
  | Asterisk_prefixed lines -> Asterisk_prefixed.fmt ~pro ~epi lines

let fmt_cmts_aux t (conf : Conf.t) cmts ~fmt_code pos =
  let open Fmt in
  let groups = List.group cmts ~break:(break_comment_group t.source) in
  vbox 0 ~name:"cmts"
    (list_pn groups (fun ~prev:_ group ~next ->
         ( match group with
         | [] -> impossible "previous match"
         | [cmt] ->
             let break =
               fmt_if
                 ( conf.fmt_opts.ocp_indent_compat.v
                 && Poly.(pos = Cmt.After)
                 && String.contains (Cmt.txt cmt) '\n' )
                 (break_unless_newline 1000 0)
             in
             break $ fmt_cmt conf cmt ~fmt_code
         | group ->
             list group force_break (fun cmt ->
                 wrap (str "(*") (str "*)") (str (Cmt.txt cmt)) ) )
         $
         match next with
         | Some (next :: _) ->
             let last = List.last_exn group in
             fmt_if
               (Location.line_difference (Cmt.loc last) (Cmt.loc next) > 1)
               (str "\n")
             $ space_break
         | _ -> noop ) )

(** Format comments for loc. *)
let fmt_cmts t conf ~fmt_code ?pro ?epi ?(eol = Fmt.break 1000 0)
    ?(adj = eol) found loc pos =
  let open Fmt in
  match found with
  | None | Some [] -> noop
  | Some cmts ->
      let epi =
        let last_loc = Cmt.loc (List.last_exn cmts) in
        let eol_cmt = Source.ends_line t.source last_loc in
        let adj_cmt = eol_cmt && Location.line_difference last_loc loc = 1 in
        fmt_or eol_cmt (fmt_or adj_cmt adj eol) (fmt_opt epi)
      in
      fmt_opt pro $ fmt_cmts_aux t conf cmts ~fmt_code pos $ epi

let fmt_before t conf ~fmt_code ?pro ?(epi = Fmt.break 1 0) ?eol ?adj loc =
  fmt_cmts t conf (find_cmts t `Before loc) ~fmt_code ?pro ~epi ?eol ?adj loc
    Before

let fmt_after t conf ~fmt_code ?(pro = Fmt.break 1 0) ?epi ?filter loc =
  let open Fmt in
  let within =
    let cmts = find_cmts ?filter t `Within loc in
    fmt_cmts t conf cmts ~fmt_code ~pro ?epi loc Within
  in
  let after =
    let cmts = find_cmts ?filter t `After loc in
    fmt_cmts t conf cmts ~fmt_code ~pro ?epi ~eol:noop loc After
  in
  within $ after

let fmt_within t conf ~fmt_code ?(pro = Fmt.break 1 0) ?(epi = Fmt.break 1 0)
    loc =
  fmt_cmts t conf (find_cmts t `Within loc) ~fmt_code ~pro ~epi ~eol:Fmt.noop
    loc Within

module Toplevel = struct
  let fmt_cmts t conf ~fmt_code found (pos : Cmt.pos) =
    let open Fmt in
    match found with
    | None | Some [] -> noop
    | Some (first :: _ as cmts) ->
        let first_loc = Cmt.loc first in
        let pro =
          match pos with
          | Before -> noop
          | Within | After ->
              if Source.begins_line t.source first_loc then
                fmt_or
                  (Source.empty_line_before t.source first_loc)
                  (str "\n" $ force_break)
                  force_newline
              else break 1 0
        in
        let epi =
          let last_loc = Cmt.loc (List.last_exn cmts) in
          match pos with
          | Before | Within ->
              if Source.ends_line t.source last_loc then
                fmt_or
                  (Source.empty_line_after t.source last_loc)
                  (str "\n" $ force_break)
                  force_newline
              else break 1 0
          | After -> noop
        in
        pro $ fmt_cmts_aux t conf cmts ~fmt_code pos $ epi

  let fmt_before t conf ~fmt_code loc =
    fmt_cmts t conf (find_cmts t `Before loc) ~fmt_code Before

  let fmt_after t conf ~fmt_code loc =
    let open Fmt in
    let within =
      fmt_cmts t conf (find_cmts t `Within loc) ~fmt_code Within
    in
    let after = fmt_cmts t conf (find_cmts t `After loc) ~fmt_code After in
    within $ after
end

let clear_remaining_inside t loc =
  if t.debug then
    update_remaining t ~f:(Set.filter ~f:(Fn.non (Location.contains loc)))

let drop_inside t loc =
  let clear pos =
    update_cmts t pos
      ~f:
        (Multimap.filter ~f:(fun cmt ->
             not (Location.contains loc (Cmt.loc cmt)) ) )
  in
  clear `Before ;
  clear `Within ;
  clear `After ;
  clear_remaining_inside t loc

let drop_before t loc =
  update_cmts t `Before ~f:(fun m -> Map.remove m loc) ;
  t

let has_before t loc = pop_if_debug t loc ; Map.mem t.cmts_before loc

let has_within t loc = pop_if_debug t loc ; Map.mem t.cmts_within loc

let has_after t loc =
  pop_if_debug t loc ;
  Map.mem t.cmts_within loc || Map.mem t.cmts_after loc

(** returns comments that have not been formatted *)
let remaining_comments t =
  List.concat_map ~f:Multimap.to_list
    [t.cmts_before; t.cmts_within; t.cmts_after]

let remaining_before t loc = Map.find_multi t.cmts_before loc

let remaining_locs t = Set.to_list t.remaining
