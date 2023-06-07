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

let break_comment_group source margin a b =
  let a = Cmt.loc a and b = Cmt.loc b in
  let vertical_align =
    Location.line_difference a b = 1 && Location.compare_start_col a b = 0
  in
  let horizontal_align =
    Location.line_difference a b = 0
    && List.is_empty
         (Source.tokens_between source a.loc_end b.loc_start
              ~filter:(function _ -> true) )
  in
  not
    ( (Location.is_single_line a margin && Location.is_single_line b margin)
    && (vertical_align || horizontal_align) )

module Asterisk_prefixed = struct
  let split txt {Location.loc_start; _} =
    let len = Position.column loc_start + 3 in
    let pat =
      String.Search_pattern.create
        (String.init len ~f:(function
          | 0 -> '\n'
          | n when n < len - 1 -> ' '
          | _ -> '*' ) )
    in
    let rec split_ pos =
      match String.Search_pattern.index pat ~pos ~in_:txt with
      | Some 0 -> "" :: split_ len
      | Some idx -> String.sub txt ~pos ~len:(idx - pos) :: split_ (idx + len)
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
    split_ 0

  let fmt ~opn lines =
    let open Fmt in
    vbox 1
      ( opn
      $ list_fl lines (fun ~first:_ ~last line ->
            match line with
            | "" when last -> fmt ")"
            | _ -> str line $ fmt_or last "*)" "@,*" ) )
end

module Unwrapped = struct
  let fmt_multiline_cmt ?epi ~offset ~starts_with_sp lines =
    let open Fmt in
    let is_white_line s = String.for_all s ~f:Char.is_whitespace in
    let unindented = Cmt.unindent_lines ~offset lines in
    let fmt_line ~first ~last:_ s =
      let sep, sp =
        if is_white_line s then (str "\n", noop)
        else (fmt "@;<1000 0>", fmt_if starts_with_sp " ")
      in
      fmt_if_k (not first) sep $ sp $ str (String.rstrip s)
    in
    vbox 0 ~name:"multiline" (list_fl unindented fmt_line $ fmt_opt epi)

  let fmt ~opn ~offset s =
    let open Fmt in
    let is_sp = function ' ' | '\t' -> true | _ -> false in
    match String.split_lines (String.rstrip s) with
    | first_line :: _ :: _ as lines when not (String.is_empty first_line) ->
        let epi =
          (* Preserve position of closing but strip empty lines at the end *)
          match String.rfindi s ~f:(fun _ c -> not (is_sp c)) with
          | Some i when Char.( = ) s.[i] '\n' ->
              break 1000 (-2) (* Break before closing *)
          | Some i when i < String.length s - 1 ->
              str " " (* Preserve a space at the end *)
          | _ -> noop
        in
        (* Preserve the first level of indentation *)
        let starts_with_sp = is_sp first_line.[0] in
        opn $ fmt_multiline_cmt ~offset ~epi ~starts_with_sp lines $ str "*)"
    | _ -> opn $ str s $ str "*)"
end

module Verbatim = struct
  let fmt s (pos : Cmt.pos) =
    let open Fmt in
    fmt_if_k
      (Poly.(pos = After) && String.contains s '\n')
      (break_unless_newline 1000 0)
    $ wrap "(*" "*)" @@ str s
end

module Cinaps = struct
  open Fmt

  (** Comments enclosed in [(*$], [$*)] are formatted as code. *)
  let fmt ~cls code =
    let wrap k = hvbox 2 (fmt "(*$" $ k $ fmt cls) in
    match String.split_lines code with
    | [] | [""] -> wrap (str " ")
    | [line] -> wrap (fmt "@ " $ str line $ fmt "@;<1 -2>")
    | lines ->
        let fmt_line = function
          | "" -> fmt "\n"
          | line -> fmt "@\n" $ str line
        in
        wrap (list lines "" fmt_line $ fmt "@;<1000 -2>")
end

module Ocp_indent_compat = struct
  let fmt ~fmt_code conf txt ~loc ~offset ~opn (pos : Cmt.pos) ~post =
    let pre, doc, post =
      let lines = String.split_lines txt in
      match lines with
      | [] | [_] -> (false, txt, false)
      | h :: _ ->
          let pre = String.is_empty (String.strip h) in
          let doc = if pre then String.lstrip txt else txt in
          let doc = if Option.is_some post then String.rstrip doc else doc in
          (pre, doc, Option.is_some post)
    in
    let parsed = Docstring.parse ~loc doc in
    (* Disable warnings when parsing fails *)
    let quiet = Conf_t.Elt.make true `Default in
    let conf = {conf with Conf.opr_opts= {conf.Conf.opr_opts with quiet}} in
    let doc = Fmt_odoc.fmt_parsed conf ~fmt_code ~input:doc ~offset parsed in
    let open Fmt in
    fmt_if_k
      (Poly.(pos = After) && String.contains txt '\n')
      (break_unless_newline 1000 0)
    $ opn
    $ wrap_k (fmt_if pre "@;<1000 3>") (fmt_if post "@\n") doc
    $ str "*)"
end

let fmt_cmt (conf : Conf.t) cmt ~fmt_code pos =
  let loc = Cmt.loc cmt in
  let offset =
    let pos = loc.Location.loc_start in
    pos.pos_cnum - pos.pos_bol + 2
  in
  let mode =
    match Cmt.txt cmt with
    | "" -> `Verbatim ""
    (* "(**)" is not parsed as a docstring but as a regular comment
       containing '*' and would be rewritten as "(***)" *)
    | "*" when Location.width loc = 4 -> `Verbatim ""
    | "*" -> `Verbatim "*"
    | "$" -> `Verbatim "$"
    (* Qtest pragmas *)
    | str when Char.(str.[0] = '$' && not (is_whitespace str.[1])) ->
        `Verbatim str
    | str when Char.equal str.[0] '$' -> (
        let dollar_suf = Char.equal str.[String.length str - 1] '$' in
        let cls : Fmt.s = if dollar_suf then "$*)" else "*)" in
        let len = String.length str - if dollar_suf then 2 else 1 in
        let offset = offset + 1 in
        let source = String.sub ~pos:1 ~len str in
        let source =
          String.split_lines source
          |> Cmt.unindent_lines ~offset
          |> String.concat ~sep:"\n"
        in
        match fmt_code conf ~offset source with
        | Ok formatted -> `Code (formatted, cls)
        | Error (`Msg _) -> `Unwrapped (str, None) )
    | txt when Char.equal txt.[0] '=' -> `Verbatim txt
    | txt -> (
        let txt =
          (* Windows compatibility *)
          let filter = function '\r' -> false | _ -> true in
          String.filter txt ~f:filter
        in
        match Asterisk_prefixed.split txt loc with
        | [] | [""] -> impossible "not produced by split_asterisk_prefixed"
        (* Comments like [(*\n*)] would be normalized as [(* *)] *)
        | [""; ""] when conf.fmt_opts.ocp_indent_compat.v ->
            `Unwrapped (txt, None)
        | [""; ""] -> `Verbatim " "
        | [text] when conf.fmt_opts.wrap_comments.v -> `Wrapped (text, "*)")
        | [text; ""] when conf.fmt_opts.wrap_comments.v ->
            `Wrapped (text, " *)")
        | [_] -> `Unwrapped (txt, None)
        | [_; ""] -> `Unwrapped (txt, Some `Ln)
        | lines -> `Asterisk_prefixed lines )
  in
  let open Fmt in
  let opn = if Cmt.is_docstring cmt then str "(**" else str "(*" in
  match mode with
  | `Verbatim x -> Verbatim.fmt x pos
  | `Code (code, cls) -> Cinaps.fmt ~cls code
  | `Wrapped (x, epi) -> opn $ fill_text x ~epi
  | `Unwrapped (x, ln) when conf.fmt_opts.ocp_indent_compat.v ->
      Ocp_indent_compat.fmt ~fmt_code conf x ~loc ~offset ~opn pos ~post:ln
  | `Unwrapped (x, _) -> Unwrapped.fmt ~opn ~offset x
  | `Asterisk_prefixed x -> Asterisk_prefixed.fmt ~opn x

let fmt_cmts_aux t (conf : Conf.t) cmts ~fmt_code pos =
  let open Fmt in
  let groups =
    List.group cmts
      ~break:(break_comment_group t.source conf.fmt_opts.margin.v)
  in
  vbox 0 ~name:"cmts"
    (list_pn groups (fun ~prev:_ group ~next ->
         ( match group with
         | [] -> impossible "previous match"
         | [cmt] -> fmt_cmt conf cmt ~fmt_code pos
         | group ->
             list group "@;<1000 0>" (fun cmt ->
                 wrap "(*" "*)" (str (Cmt.txt cmt)) ) )
         $
         match next with
         | Some (next :: _) ->
             let last = List.last_exn group in
             fmt_if
               (Location.line_difference (Cmt.loc last) (Cmt.loc next) > 1)
               "\n"
             $ fmt "@ "
         | _ -> noop ) )

(** Format comments for loc. *)
let fmt_cmts t conf ~fmt_code ?pro ?epi ?(eol = Fmt.fmt "@\n") ?(adj = eol)
    found loc pos =
  let open Fmt in
  match found with
  | None | Some [] -> noop
  | Some cmts ->
      let epi =
        let last_loc = Cmt.loc (List.last_exn cmts) in
        let eol_cmt = Source.ends_line t.source last_loc in
        let adj_cmt = eol_cmt && Location.line_difference last_loc loc = 1 in
        fmt_or_k eol_cmt (fmt_or_k adj_cmt adj eol) (fmt_opt epi)
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
                  "\n@;<1000 0>" "@\n"
              else break 1 0
        in
        let epi =
          let last_loc = Cmt.loc (List.last_exn cmts) in
          match pos with
          | Before | Within ->
              if Source.ends_line t.source last_loc then
                fmt_or
                  (Source.empty_line_after t.source last_loc)
                  "\n@;<1000 0>" "@\n"
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

let is_docstring (conf : Conf.t) cmt =
  let might_be_docstring cmt =
    match Cmt.txt cmt with
    | "" | "*" -> false
    | txt -> not (Char.equal txt.[0] '$')
  in
  (* In ocp_indent_compat mode, comments are parsed like docstrings. *)
  if
    conf.fmt_opts.parse_docstrings.v
    && ( Cmt.is_docstring cmt
       || (conf.fmt_opts.ocp_indent_compat.v && might_be_docstring cmt) )
  then Either.First cmt
  else Either.Second cmt
