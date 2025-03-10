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

(** Format OCaml Ast *)

open Migrate_ast
open Extended_ast
open Asttypes
open Ast
open Fmt

type c =
  { conf: Conf.t
  ; debug: bool
  ; source: Source.t
  ; cmts: Cmts.t
  ; fmt_code: Fmt_odoc.fmt_code }

module Cmts = struct
  include Cmts

  let fmt_before c = fmt_before c.cmts c.conf ~fmt_code:c.fmt_code

  let fmt_within c = fmt_within c.cmts c.conf ~fmt_code:c.fmt_code

  let fmt_after c = fmt_after c.cmts c.conf ~fmt_code:c.fmt_code

  let fmt c ?pro ?epi ?eol ?adj loc =
    (* remove the before comments from the map first *)
    let before = fmt_before c ?pro ?epi ?eol ?adj loc in
    (* remove the within comments from the map by accepting the
       continuation *)
    fun inner ->
      (* delay the after comments until the within comments have been
         removed *)
      let after = fmt_after c ?pro ?epi loc in
      let open Fmt in
      before $ inner $ after

  module Toplevel = struct
    let fmt_before c = Toplevel.fmt_before c.cmts c.conf ~fmt_code:c.fmt_code

    let fmt_after c = Toplevel.fmt_after c.cmts c.conf ~fmt_code:c.fmt_code
  end
end

let cmt_checker {cmts; _} =
  { cmts_before= Cmts.has_before cmts
  ; cmts_within= Cmts.has_within cmts
  ; cmts_after= Cmts.has_after cmts }

let break_between c = Ast.break_between c.source (cmt_checker c)

type block =
  { opn: Fmt.t option
  ; pro: Fmt.t option
  ; psp: Fmt.t
  ; bdy: Fmt.t
  ; cls: Fmt.t
  ; esp: Fmt.t
  ; epi: Fmt.t option }

let empty =
  { opn= None
  ; pro= None
  ; psp= noop
  ; bdy= noop
  ; cls= noop
  ; esp= noop
  ; epi= None }

let blk_box ?(box = true) blk k =
  match blk.opn with Some opn -> wrap_if box opn blk.cls k | None -> k

let compose_module' ?box ?pro ?epi ({psp; bdy; esp; _} as blk) =
  ( blk_box ?box blk (fmt_opt pro $ (fmt_opt blk.pro $ psp $ bdy)) $ esp
  , fmt_opt blk.epi $ fmt_opt epi )

let compose_module ?box ?pro ?epi blk ~f =
  let bdy, epi' = compose_module' ?box ?pro blk in
  f (bdy $ epi') $ fmt_opt epi

(* Debug: catch and report failures at nearest enclosing Ast.t *)

let protect =
  let first = ref true in
  fun c ast pp ->
    Fmt.protect pp ~on_error:(fun exc ->
        if !first && c.debug then (
          let bt = Stdlib.Printexc.get_backtrace () in
          Stdlib.Format.eprintf "@\nFAIL@\n%a@\n%s@.%!" Ast.dump ast bt ;
          first := false ) ;
        raise exc )

let update_config ?quiet c l =
  {c with conf= List.fold ~init:c.conf l ~f:(Conf.update ?quiet)}

let update_config_attrs ?quiet c {attrs_before; attrs_after; _} =
  update_config ?quiet (update_config ?quiet c attrs_before) attrs_after

(* Preserve the position of comments located after the last element of a
   list/array (after `;`), otherwise comments are picked up by
   `fmt_expression` and printed before `;`. *)
let collection_last_cmt ?pro c (loc : Location.t) locs =
  let filter = function Parser.SEMI -> true | _ -> false in
  opt (List.last locs) (fun (last : Location.t) ->
      match
        Source.tokens_between c.source last.loc_end loc.loc_end ~filter
      with
      | [] -> noop
      | (_, semicolon_loc) :: _ ->
          Cmts.fmt_after ?pro c last ~filter:(fun cmt ->
              Location.compare (Cmt.loc cmt) semicolon_loc >= 0 ) )

let fmt_elements_collection ?pro ?(first_sep = true) ?(last_sep = true) c
    (p : Params.elements_collection) f loc fmt_x xs =
  let fmt_one ~first ~last x =
    fmt_if (not (first && first_sep)) p.sep_before
    $ fmt_x x
    $ fmt_or (last && last_sep) p.sep_after_final p.sep_after_non_final
  in
  list_fl xs fmt_one $ collection_last_cmt ?pro c loc (List.map ~f xs)

let fmt_expressions c width sub_exp exprs fmt_expr p loc =
  match c.conf.fmt_opts.break_collection_expressions.v with
  | `Fit_or_vertical ->
      fmt_elements_collection c p Exp.location loc fmt_expr exprs
  | `Wrap ->
      let is_simple x = is_simple c.conf width (sub_exp x) in
      let break x1 x2 = not (is_simple x1 && is_simple x2) in
      let grps = List.group exprs ~break in
      let fmt_grp ~first:first_grp ~last:last_grp exprs =
        fmt_elements_collection c ~first_sep:first_grp ~last_sep:last_grp p
          Exp.location loc fmt_expr exprs
      in
      list_fl grps fmt_grp

(** Handle the `break-fun-decl` option *)
let wrap_fun_decl_args c k =
  match c.conf.fmt_opts.break_fun_decl.v with
  | `Wrap | `Fit_or_vertical -> k
  | `Smart -> hvbox 0 k

let box_fun_decl_args c =
  match c.conf.fmt_opts.break_fun_decl.v with
  | `Fit_or_vertical -> hvbox
  | `Wrap | `Smart -> hovbox

(** Handle the `break-fun-sig` option *)
let box_fun_sig_args c =
  match c.conf.fmt_opts.break_fun_sig.v with
  | _ when c.conf.fmt_opts.ocp_indent_compat.v -> hvbox
  | `Fit_or_vertical -> hvbox
  | `Wrap | `Smart -> hovbox

let closing_paren ?(force_space = false) ?force ?(offset = 0) c =
  if force_space then str " )"
  else
    match c.conf.fmt_opts.indicate_multiline_delimiters.v with
    | `No -> str ")"
    | `Space -> fits_breaks ")" " )" ?force
    | `Closing_on_separate_line -> fits_breaks ")" ")" ~hint:(1000, offset)

let maybe_disabled_k c (loc : Location.t) (l : attributes) f k =
  if not c.conf.opr_opts.disable.v then f c
  else
    let loc = Source.extend_loc_to_include_attributes loc l in
    Cmts.drop_inside c.cmts loc ;
    let s = Source.string_at c.source loc in
    k (Cmts.fmt c loc (str s))

let maybe_disabled c loc l f = maybe_disabled_k c loc l f Fn.id

let update_config_maybe_disabled c loc l f =
  let c = update_config c l in
  maybe_disabled c loc l f

let update_config_maybe_disabled_k c loc l ~if_disabled enabled =
  let c = update_config c l in
  maybe_disabled_k c loc l enabled if_disabled

let update_config_maybe_disabled_attrs c loc attrs f =
  let l = attrs.attrs_before @ attrs.attrs_after in
  update_config_maybe_disabled c loc l f

let update_config_maybe_disabled_block c loc l f =
  let fmt bdy = {empty with opn= Some (open_vbox 2); bdy; cls= close_box} in
  let c = update_config c l in
  maybe_disabled_k c loc l f fmt

let update_items_config c items update_config =
  let with_config c i =
    let c = update_config c i in
    (c, (i, c))
  in
  let _, items = List.fold_map items ~init:c ~f:with_config in
  items

let box_semisemi c ~parent_ctx b k =
  let space = Poly.(c.conf.fmt_opts.sequence_style.v = `Separator) in
  match parent_ctx with
  | _ when not b -> k
  | Rep -> k $ fmt_if space (str " ") $ str ";;"
  | _ -> hvbox 0 (k $ fmt_or space space_break cut_break $ str ";;")

let fmt_hole () = str "_"

let fmt_item_list c ctx0 update_config ast fmt_item items =
  let items = update_items_config c items update_config in
  let break_struct = c.conf.fmt_opts.break_struct.v || is_top ctx0 in
  hvbox 0 @@ list_pn items
  @@ fun ~prev (itm, c) ~next ->
  let ctx = ast itm in
  let loc = Ast.location ctx in
  maybe_disabled c loc [] (fun c -> fmt_item c ctx ~prev ~next itm)
  $ opt next (fun (i_n, c_n) ->
        fmt_or
          (break_between c (ctx, c.conf) (ast i_n, c_n.conf))
          (str "\n" $ force_break)
          (fmt_or break_struct force_break space_break) )

let fmt_recmodule c ctx items fmt_item ast sub =
  let update_config c i = update_config c (Ast.attributes (ast i)) in
  let fmt_item c ctx ~prev ~next:_ i =
    fmt_item c ~rec_flag:true ~first:(Option.is_none prev) (sub ~ctx i)
  in
  fmt_item_list c ctx update_config ast fmt_item items

(* In several places, naked newlines (i.e. not "@\n") are used to avoid
   trailing space in open lines. *)
(* In several places, a break such as [Fmt.force_break] is used to force the
   enclosing box to break across multiple lines. *)

let rec fmt_longident (li : Longident.t) =
  let fmt_id id =
    wrap_if
      (Std_longident.String_id.is_symbol id)
      (str "( ") (str " )") (str id)
  in
  match li with
  | Lident id -> fmt_id id
  | Ldot (li, id) ->
      hvbox 0 (fmt_longident li $ cut_break $ str "." $ fmt_id id)
  | Lapply (li1, li2) ->
      hvbox 2
        ( fmt_longident li1
        $ wrap (cut_break $ str "(") (str ")") (fmt_longident li2) )

let fmt_longident_loc c ?pre {txt; loc} =
  Cmts.fmt c loc (opt pre str $ fmt_longident txt)

let str_longident x =
  Format_.asprintf "%a" (fun fs x -> eval fs (fmt_longident x)) x

let fmt_str_loc c ?pre {txt; loc} = Cmts.fmt c loc (opt pre str $ str txt)

let fmt_str_loc_opt c ?pre ?(default = "_") {txt; loc} =
  Cmts.fmt c loc (opt pre str $ str (Option.value ~default txt))

let variant_var c ({txt= x; loc} : variant_var) =
  Cmts.fmt c loc @@ (str "`" $ fmt_str_loc c x)

let fmt_constant c ?epi {pconst_desc; pconst_loc= loc} =
  Cmts.fmt c loc
  @@
  match pconst_desc with
  | Pconst_integer (lit, suf) | Pconst_float (lit, suf) ->
      str lit $ opt suf char
  | Pconst_char (_, s) -> wrap (str "'") (str "'") @@ str s
  | Pconst_string (s, loc', Some delim) ->
      Cmts.fmt c loc'
      @@
      (* If a multiline string has newlines in it, the configuration might
         specify it should get treated as a "long" box element. To do so, we
         pretend it is 1000 characters long. *)
      ( if
          c.conf.fmt_opts.break_around_multiline_strings.v
          && String.mem s '\n'
        then str_as 1000
        else str )
        (Format_.sprintf "{%s|%s|%s}" delim s delim)
  | Pconst_string (orig_s, loc', None) -> (
      let delim = ["@,"; "@;"] in
      let contains_pp_commands s =
        let is_substring substring = String.is_substring s ~substring in
        List.exists delim ~f:is_substring
      in
      let fmt_string_auto ~break_on_newlines s =
        let fmt_words ~epi s =
          let words = String.split s ~on:' ' in
          let fmt_word ~prev:_ curr ~next =
            match next with
            | Some "" -> str curr $ str " "
            | Some _ ->
                str curr $ cbreak ~fits:("", 1, "") ~breaks:(" \\", 0, "")
            | None -> str curr
          in
          hovbox_if (List.length words > 1) 0 (list_pn words fmt_word $ epi)
        in
        let fmt_line ~epi ~prev:_ curr ~next =
          let not_suffix suffix = not (String.is_suffix curr ~suffix) in
          let print_ln =
            List.for_all delim ~f:not_suffix || not break_on_newlines
          in
          let fmt_next next =
            if String.is_empty next then fmt_if print_ln (str "\\n")
            else if Char.equal next.[0] ' ' then
              fmt_if print_ln (str "\\n")
              $ cbreak ~fits:("", 0, "") ~breaks:("\\", -1, "\\")
            else
              fmt_if print_ln (str "\\n")
              $ cbreak ~fits:("", 0, "") ~breaks:("\\", 0, "")
          in
          let epi = match next with Some _ -> noop | None -> epi in
          fmt_words ~epi curr $ opt next fmt_next
        in
        let lines = String.split ~on:'\n' s in
        let lines =
          if break_on_newlines then lines
          else
            let n_lines = List.length lines in
            (* linebreaks are merged with the preceding line when possible
               instead of having a blank line in the list *)
            List.foldi lines ~init:[] ~f:(fun i acc -> function
              | "" when i < n_lines - 1 -> (
                match acc with [] -> [""] | h :: t -> (h ^ "\\n") :: t )
              | line -> line :: acc )
            |> List.rev
        in
        let epi = str "\"" $ fmt_opt epi in
        hvbox 1 (str "\"" $ list_pn lines (fmt_line ~epi))
      in
      let preserve_or_normalize =
        match c.conf.fmt_opts.break_string_literals.v with
        | `Never -> `Preserve
        | `Auto -> `Normalize
      in
      let s =
        if loc.loc_ghost then String.escaped orig_s
        else Source.string_literal c.source preserve_or_normalize loc
      in
      Cmts.fmt c loc'
      @@
      match c.conf.fmt_opts.break_string_literals.v with
      | `Auto when contains_pp_commands s ->
          let break_on_pp_commands in_ pattern =
            String.substr_replace_all in_ ~pattern ~with_:(pattern ^ "\n")
          in
          List.fold_left delim ~init:s ~f:break_on_pp_commands
          |> fmt_string_auto ~break_on_newlines:true
      | `Auto -> fmt_string_auto ~break_on_newlines:false s
      | `Never -> wrap (str "\"") (str "\"") (str s) )

let fmt_variance_injectivity c vc = hvbox 0 (list vc noop (fmt_str_loc c))

let fmt_label lbl sep =
  (* No comment can be attached here. *)
  match lbl with
  | Nolabel -> noop
  | Labelled l -> str "~" $ str l.txt $ sep
  | Optional l -> str "?" $ str l.txt $ sep

let fmt_direction_flag = function
  | Upto -> space_break $ str "to "
  | Downto -> space_break $ str "downto "

let fmt_private ?(pro = space_break) c loc =
  pro $ hvbox 0 @@ Cmts.fmt c loc @@ str "private"

let fmt_virtual ?(pro = space_break) c loc =
  pro $ hvbox 0 @@ Cmts.fmt c loc @@ str "virtual"

let fmt_mutable ?(pro = space_break) ?(epi = noop) c loc =
  pro $ hvbox 0 (Cmts.fmt c loc (str "mutable")) $ epi

let fmt_private_flag c = function
  | Private loc -> fmt_private c loc
  | Public -> noop

let fmt_virtual_flag c = function
  | Virtual loc -> fmt_virtual c loc
  | Concrete -> noop

let fmt_mutable_flag ?pro ?epi c = function
  | Mutable loc -> fmt_mutable ?pro ?epi c loc
  | Immutable -> noop

let fmt_mutable_virtual_flag c = function
  | {mv_mut= Some m; mv_virt= Some v} when Location.compare_start v m < 1 ->
      fmt_virtual c v $ fmt_mutable c m
  | {mv_mut; mv_virt} ->
      opt mv_mut (fmt_mutable c) $ opt mv_virt (fmt_virtual c)

let fmt_private_virtual_flag c = function
  | {pv_priv= Some p; pv_virt= Some v} when Location.compare_start v p < 1 ->
      fmt_virtual c v $ fmt_private c p
  | {pv_priv; pv_virt} ->
      opt pv_priv (fmt_private c) $ opt pv_virt (fmt_virtual c)

let virtual_or_override = function
  | Cfk_virtual _ -> noop
  | Cfk_concrete (Override, _, _) -> str "!"
  | Cfk_concrete (Fresh, _, _) -> noop

(** Format the [:] before a type constraint. *)
let fmt_constraint_sep ?(pro_space = true) c sep =
  match c.conf.fmt_opts.break_colon.v with
  | `Before -> fmt_if pro_space space_break $ str sep $ str " "
  | `After -> fmt_if pro_space (str " ") $ str sep $ space_break

let fmt_parsed_docstring c ~loc ?pro ~epi input parsed =
  assert (not (String.is_empty input)) ;
  let offset =
    let pos = loc.Location.loc_start in
    pos.pos_cnum - pos.pos_bol + 3
  and fmt_code = c.fmt_code in
  let doc = Fmt_odoc.fmt_parsed c.conf ~fmt_code ~offset ~input parsed in
  Cmts.fmt c loc
  @@ vbox_if (Option.is_none pro) 0
       (fmt_opt pro $ hovbox 0 (str "(**" $ doc $ str "*)") $ epi)

let docstring_epi ~standalone ~next ~epi ~floating =
  let epi = if Option.is_some next then force_newline else fmt_opt epi in
  match next with
  | (None | Some (_, false)) when floating && not standalone ->
      str "\n" $ epi
  | _ -> epi

let fmt_docstring c ?(standalone = false) ?pro ?epi doc =
  list_pn (Option.value ~default:[] doc)
    (fun ~prev:_ ({txt; loc}, floating) ~next ->
      let epi = docstring_epi ~standalone ~next ~epi ~floating in
      fmt_parsed_docstring c ~loc ?pro ~epi txt (Docstring.parse ~loc txt) )

let fmt_docstring_around_item' ?(is_val = false) ?(force_before = false)
    ?(fit = false) c doc1 doc2 =
  match (doc1, doc2) with
  | Some _, Some _ ->
      ( fmt_docstring c ~epi:force_newline doc1
      , fmt_docstring c ~pro:force_newline doc2 )
  | None, None -> (noop, noop)
  | None, Some doc | Some doc, None -> (
      let is_tag_only =
        List.for_all ~f:(function
          | Ok es, _ -> Docstring.is_tag_only es
          | _ -> false )
      in
      let fmt_doc ?epi ?pro doc =
        list_pn doc (fun ~prev:_ (parsed, ({txt; loc}, floating)) ~next ->
            let next = Option.map next ~f:snd in
            let epi = docstring_epi ~standalone:false ~next ~epi ~floating in
            fmt_parsed_docstring c ~loc ~epi ?pro txt parsed )
      in
      let floating_doc, doc =
        doc
        |> List.map ~f:(fun (({txt; loc}, _) as doc) ->
               (Docstring.parse ~loc txt, doc) )
        |> List.partition_tf ~f:(fun (_, (_, floating)) -> floating)
      in
      let placement =
        if force_before then `Before
        else if
          Poly.( = ) c.conf.fmt_opts.doc_comments_tag_only.v `Fit
          && fit && is_tag_only doc
        then `Fit
        else
          let ((`Before | `After) as conf) =
            match c.conf.fmt_opts.doc_comments.v with
            | `After_when_possible -> `After
            | `Before_except_val when is_val -> `After
            | `Before_except_val -> `Before
            | `Before -> `Before
          in
          conf
      in
      let floating_doc = fmt_doc ~epi:force_newline floating_doc in
      match placement with
      | `Before -> (floating_doc $ fmt_doc ~epi:force_newline doc, noop)
      | `After -> (floating_doc, fmt_doc ~pro:force_newline doc)
      | `Fit ->
          ( floating_doc
          , fmt_doc ~pro:(break c.conf.fmt_opts.doc_comments_padding.v 0) doc
          ) )

(** There can be up to two doc comments. *)
let extract_doc_attrs docs attrs =
  let extract_once acc attrs =
    if List.length acc >= 2 then (acc, attrs)
    else
      let doc, attrs = doc_atrs attrs in
      let acc = match doc with Some doc -> doc :: acc | None -> acc in
      (acc, attrs)
  in
  let docs, attrs = extract_once docs attrs in
  let docs, attrs = extract_once docs attrs in
  (List.rev docs, attrs)

(** Formats docstrings and decides where to place them Handles the
    [doc-comments] and [doc-comment-tag-only] options Returns the tuple
    [doc_before, doc_after, attrs] *)
let fmt_docstring_around_item ?is_val ?force_before ?fit c attrs =
  let doc1, attrs = doc_atrs attrs in
  let doc2, attrs = doc_atrs attrs in
  let doc_before, doc_after =
    fmt_docstring_around_item' ?is_val ?force_before ?fit c doc1 doc2
  in
  (doc_before, doc_after, attrs)

(** Returns the documentation before and after the item as well as the
    [ext_attrs] before and after attributes, modified.
    It is assumed that docstrings can only occurs in [attrs_after]. *)
let fmt_docstring_around_item_attrs ?is_val ?force_before ?fit c attrs =
  let doc_before, doc_after, attrs_after =
    fmt_docstring_around_item ?is_val ?force_before ?fit c attrs.attrs_after
  in
  (doc_before, doc_after, attrs.attrs_before, attrs_after)

let fmt_extension_suffix ?epi c ext =
  opt ext (fun name -> str "%" $ fmt_str_loc c name $ fmt_opt epi)

let is_arrow_or_poly = function
  | {ptyp_desc= Ptyp_arrow _ | Ptyp_poly _; _} -> true
  | _ -> false

let fmt_assign_arrow c =
  match c.conf.fmt_opts.assignment_operator.v with
  | `Begin_line ->
      break 1 (Params.Indent.assignment_operator_bol c.conf) $ str "<- "
  | `End_line -> str " <-" $ break 1 2

let arrow_sep c ~parens : Fmt.t =
  match c.conf.fmt_opts.break_separators.v with
  | `Before ->
      if parens then break 1 1 $ str "-> " else space_break $ str "-> "
  | `After -> str " ->" $ break 1 0

let fmt_docstring_padded c doc =
  fmt_docstring c ~pro:(break c.conf.fmt_opts.doc_comments_padding.v 0) doc

let sequence_blank_line c (l1 : Location.t) (l2 : Location.t) =
  match c.conf.fmt_opts.sequence_blank_line.v with
  | `Preserve_one ->
      let rec loop prev_pos = function
        | cmt :: tl ->
            let loc = Cmt.loc cmt in
            (* Check empty line before each comment *)
            Source.empty_line_between c.source prev_pos loc.loc_start
            || loop loc.loc_end tl
        | [] ->
            (* Check empty line after all comments *)
            Source.empty_line_between c.source prev_pos l2.loc_start
      in
      loop l1.loc_end (Cmts.remaining_before c.cmts l2)
  | `Compact -> false

let fmt_quoted_string c key ext s maybe_delim =
  ( if c.conf.fmt_opts.break_around_multiline_strings.v && String.mem s '\n'
    then str_as 1000
    else str )
  @@
  match maybe_delim with
  | None -> Format_.sprintf "{%s%s|%s|}" key ext s
  | Some delim ->
      let ext_and_delim =
        if String.is_empty delim then ext
        else Format_.sprintf "%s %s" ext delim
      in
      Format_.sprintf "{%s%s|%s|%s}" key ext_and_delim s delim

let fmt_type_var s =
  str "'"
  (* [' a'] is a valid type variable, the space is required to not lex as a
     char. https://github.com/ocaml/ocaml/pull/2034 *)
  $ fmt_if (String.length s > 1 && Char.equal s.[1] '\'') (str " ")
  $ str s

let rec fmt_extension_aux c ctx ~key (ext, pld) =
  match (ext.txt, pld, ctx) with
  (* Quoted extensions (since ocaml 4.11). *)
  | ( ext
    , PStr
        [ { pstr_desc=
              Pstr_eval
                ( { pexp_desc=
                      Pexp_constant
                        {pconst_desc= Pconst_string (str, loc, delim); _}
                  ; pexp_loc
                  ; pexp_loc_stack= _
                  ; pexp_attributes= [] }
                , [] )
          ; pstr_loc } ]
    , _ )
    when Source.is_quoted_string c.source pstr_loc ->
      (* Comments and attributes are not allowed by the parser *)
      assert (not (Cmts.has_before c.cmts loc)) ;
      assert (not (Cmts.has_after c.cmts loc)) ;
      assert (not (Cmts.has_before c.cmts pexp_loc)) ;
      assert (not (Cmts.has_after c.cmts pexp_loc)) ;
      assert (not (Cmts.has_before c.cmts pstr_loc)) ;
      assert (not (Cmts.has_after c.cmts pstr_loc)) ;
      hvbox 0 (fmt_quoted_string c (Ext.Key.to_string key) ext str delim)
  | _, PStr [({pstr_loc; _} as si)], (Pld _ | Str _ | Top)
    when Source.extension_using_sugar ~name:ext ~payload:pstr_loc ->
      fmt_structure_item c ~last:true ~semisemi:false (sub_str ~ctx si)
  | _, PSig [({psig_loc; _} as si)], (Pld _ | Sig _ | Top)
    when Source.extension_using_sugar ~name:ext ~payload:psig_loc ->
      fmt_signature_item c (sub_sig ~ctx si)
  | _, PPat (({ppat_loc; _} as pat), _), (Pld _ | Top)
    when Source.extension_using_sugar ~name:ext ~payload:ppat_loc ->
      fmt_pattern c ~ext (sub_pat ~ctx pat)
  | _ ->
      let box =
        if c.conf.fmt_opts.ocp_indent_compat.v then
          match pld with
          | PStr [{pstr_desc= Pstr_eval _; _}] | PTyp _ | PPat _ ->
              hvbox c.conf.fmt_opts.extension_indent.v
          | PSig _ | PStr _ ->
              hvbox c.conf.fmt_opts.stritem_extension_indent.v
        else Fn.id
      in
      box
        (wrap (str "[") (str "]")
           ( str (Ext.Key.to_string key)
           $ fmt_str_loc c ext
           $ fmt_payload c (Pld pld) pld
           $ fmt_if (Exposed.Right.payload pld) (str " ") ) )

and fmt_extension = fmt_extension_aux ~key:Ext.Key.Regular

and fmt_item_extension = fmt_extension_aux ~key:Ext.Key.Item

and fmt_attribute c ~key {attr_name; attr_payload; attr_loc} =
  hvbox 0 @@ Cmts.fmt c attr_loc
  @@
  match (attr_name, attr_payload) with
  | ( {txt= ("ocaml.doc" | "ocaml.text") as txt; loc= {loc_ghost= true; _}}
    , PStr
        [ { pstr_desc=
              Pstr_eval
                ( { pexp_desc=
                      Pexp_constant
                        {pconst_desc= Pconst_string (doc, _, None); _}
                  ; pexp_attributes= []
                  ; _ }
                , [] )
          ; _ } ] ) ->
      fmt_or (String.equal txt "ocaml.text") space_break (str " ")
      $ wrap (str "(**") (str "*)") (str doc)
  | name, pld ->
      let indent =
        match (pld, key) with
        | (PStr _ | PSig _), Attr.Key.Floating ->
            c.conf.fmt_opts.stritem_extension_indent.v
        | _ -> c.conf.fmt_opts.extension_indent.v
      in
      hvbox indent
        (wrap (str "[") (str "]")
           ( str (Attr.Key.to_string key)
           $ fmt_str_loc c name
           $ fmt_payload c (Pld pld) pld
           $ fmt_if (Exposed.Right.payload pld) (str " ") ) )

and fmt_attributes_aux c ?pre ?suf ~key attrs =
  let num = List.length attrs in
  fmt_if (num > 0)
    ( opt pre sp
    $ hvbox_if (num > 1) 0
        ( hvbox 0 (list attrs space_break (fmt_attribute c ~key))
        $ opt suf str ) )

and fmt_attributes = fmt_attributes_aux ~key:Attr.Key.Regular

and fmt_item_attributes = fmt_attributes_aux ~key:Attr.Key.Item

and fmt_attributes_and_docstrings_aux c ~key attrs =
  let standalone, pro, space =
    match key with
    | Attr.Key.Regular | Attr.Key.Item ->
        (false, break c.conf.fmt_opts.doc_comments_padding.v 0, break 1 0)
    | Attr.Key.Floating -> (true, noop, noop)
  in
  let aux = function
    | { attr_name=
          {txt= "ocaml.doc" | "ocaml.text"; loc= {loc_ghost= true; _}}
      ; attr_payload=
          PStr
            [ { pstr_desc=
                  Pstr_eval
                    ( { pexp_desc=
                          Pexp_constant
                            {pconst_desc= Pconst_string (txt, _, None); _}
                      ; pexp_loc= loc
                      ; pexp_attributes= []
                      ; _ }
                    , [] )
              ; _ } ]
      ; _ } ->
        fmt_docstring c ~standalone ~pro (Some [({txt; loc}, standalone)])
    | attr -> space $ fmt_attribute c ~key attr
  in
  list attrs noop aux

and fmt_attributes_and_docstrings =
  fmt_attributes_and_docstrings_aux ~key:Attr.Key.Regular

and fmt_floating_attributes_and_docstrings =
  fmt_attributes_and_docstrings_aux ~key:Attr.Key.Floating

and fmt_payload c ctx pld =
  protect c (Pld pld)
  @@
  match pld with
  | PStr mex ->
      fmt_if (not (List.is_empty mex)) space_break $ fmt_structure c ctx mex
  | PSig mty ->
      str ":"
      $ fmt_if (not (List.is_empty mty)) space_break
      $ fmt_signature c ctx mty
  | PTyp typ -> str ":" $ space_break $ fmt_core_type c (sub_typ ~ctx typ)
  | PPat (pat, exp) ->
      let fmt_when exp =
        str " when " $ fmt_expression c (sub_exp ~ctx exp)
      in
      str "?" $ space_break
      $ fmt_pattern c (sub_pat ~ctx pat)
      $ opt exp fmt_when

and fmt_record_field c ?typ1 ?typ2 ?rhs lid1 =
  let field_space =
    match c.conf.fmt_opts.field_space.v with
    | `Loose | `Tight_decl -> str " "
    | `Tight -> noop
  in
  let t1 = Option.map typ1 ~f:(fun x -> str ": " $ fmt_core_type c x) in
  let t2 = Option.map typ2 ~f:(fun x -> str ":> " $ fmt_core_type c x) in
  let r = Option.map rhs ~f:(fun x -> str "=" $ break 1 2 $ cbox 0 x) in
  let fmt_type_rhs =
    match List.filter_opt [t1; t2; r] with
    | [] -> noop
    | l -> field_space $ list l space_break Fn.id
  in
  Cmts.fmt_before c lid1.loc
  $ cbox 0
      (fmt_longident_loc c lid1 $ Cmts.fmt_after c lid1.loc $ fmt_type_rhs)

and fmt_type_cstr c ?(pro = ":") ?constraint_ctx xtyp =
  let colon_before = Poly.(c.conf.fmt_opts.break_colon.v = `Before) in
  let wrap, inner_pro, box =
    match xtyp.ast.ptyp_desc with
    | (Ptyp_poly _ | Ptyp_arrow _) when colon_before ->
        let outer_pro =
          match (xtyp.ast.ptyp_desc, c.conf.fmt_opts.break_separators.v) with
          | ( (Ptyp_poly (_, {ptyp_desc= Ptyp_arrow _; _}) | Ptyp_arrow _)
            , `Before ) ->
              fits_breaks (pro ^ " ") (pro ^ "  ")
          | _ -> str pro $ str " "
        in
        let pre_break =
          if colon_before then fits_breaks " " ~hint:(1000, 0) ""
          else break 0 ~-1
        in
        let wrap x = pre_break $ hvbox 0 (outer_pro $ x) in
        (wrap, None, false)
    | _ ->
        ( (fun k ->
            fmt_or colon_before
              (fits_breaks " " ~hint:(1000, 0) "")
              (break 0 (-1))
            $ cbox_if colon_before 0 k )
        , Some pro
        , true )
  in
  wrap
    (fmt_core_type c ?pro:inner_pro ~pro_space:(not colon_before)
       ?constraint_ctx ~box xtyp )

and fmt_type_pcstr c ~ctx ?constraint_ctx cstr =
  let fmt_typ ~pro t =
    fmt_type_cstr c ~pro ?constraint_ctx (sub_typ ~ctx t)
  in
  match cstr with
  | Pconstraint t -> fmt_typ ~pro:":" t
  | Pcoerce (t1, t2) -> opt t1 (fmt_typ ~pro:":") $ fmt_typ ~pro:":>" t2

and fmt_arrow_param c ctx {pap_label= lI; pap_loc= locI; pap_type= tI} =
  let arg_label lbl =
    match lbl with
    | Nolabel -> None
    | Labelled l -> Some (str l.txt $ str ":" $ cut_break)
    | Optional l -> Some (str "?" $ str l.txt $ str ":" $ cut_break)
  in
  let xtI = sub_typ ~ctx tI in
  let arg =
    match arg_label lI with
    | None -> fmt_core_type c xtI
    | Some f -> hovbox 2 (f $ fmt_core_type c xtI)
  in
  hvbox 0 (Cmts.fmt_before c locI $ arg)

(** Format [Ptyp_arrow]. [indent] can be used to override the indentation
    added for the break-separators option. [parent_has_parens] is used to
    align arrows to parentheses. *)
and fmt_arrow_type c ~ctx ?indent ~parens ~parent_has_parens args fmt_ret_typ
    =
  let indent =
    match indent with
    | Some k -> k
    | None ->
        fmt_if
          Poly.(c.conf.fmt_opts.break_separators.v = `Before)
          (fmt_or c.conf.fmt_opts.ocp_indent_compat.v (fits_breaks "" "")
             (fits_breaks "" "   ") )
  and ret_typ =
    match fmt_ret_typ with
    | Some k -> arrow_sep c ~parens:parent_has_parens $ k
    | None -> noop
  in
  indent
  $ wrap_if parens (str "(") (str ")")
      ( list args
          (arrow_sep c ~parens:parent_has_parens)
          (fmt_arrow_param c ctx)
      $ ret_typ )

(* The context of [xtyp] refers to the RHS of the expression (namely
   Pexp_constraint) and does not give a relevant information as to whether
   [xtyp] should be parenthesized. [constraint_ctx] gives the higher context
   of the expression, i.e. if the expression is part of a `fun`
   expression. *)
and fmt_core_type c ?(box = true) ?pro ?(pro_space = true) ?constraint_ctx
    ({ast= typ; ctx= ctx0} as xtyp) =
  protect c (Typ typ)
  @@
  let {ptyp_desc; ptyp_attributes; ptyp_loc; _} = typ in
  update_config_maybe_disabled c ptyp_loc ptyp_attributes
  @@ fun c ->
  ( match pro with
  | Some pro -> fmt_constraint_sep ~pro_space c pro
  | None -> noop )
  $
  let doc, atrs = doc_atrs ptyp_attributes in
  Cmts.fmt c ptyp_loc
  @@ (fun k -> k $ fmt_docstring c ~pro:space_break doc)
  @@ ( if List.is_empty atrs then Fn.id
       else fun k ->
         hvbox 0 (Params.parens c.conf (k $ fmt_attributes c ~pre:Cut atrs))
     )
  @@
  let parens = parenze_typ xtyp in
  hvbox_if box 0
  @@ Params.parens_if
       (match typ.ptyp_desc with Ptyp_tuple _ -> false | _ -> parens)
       c.conf
  @@
  let in_type_declaration =
    match ctx0 with
    | Td {ptype_manifest= Some t; _} -> phys_equal t typ
    | _ -> false
  in
  let ctx = Typ typ in
  let parenze_constraint_ctx =
    match constraint_ctx with
    | Some `Fun when not parens -> true
    | _ -> false
  in
  match ptyp_desc with
  | Ptyp_alias (typ, str) ->
      hvbox 0
        (wrap_if parenze_constraint_ctx (Fmt.str "(") (Fmt.str ")")
           ( fmt_core_type c (sub_typ ~ctx typ)
           $ space_break $ Fmt.str "as" $ space_break
           $ Cmts.fmt c str.loc @@ fmt_type_var str.txt ) )
  | Ptyp_any -> str "_"
  | Ptyp_arrow (args, ret_typ) ->
      Cmts.relocate c.cmts ~src:ptyp_loc
        ~before:(List.hd_exn args).pap_type.ptyp_loc ~after:ret_typ.ptyp_loc ;
      let indent =
        match pro with
        | Some pro when c.conf.fmt_opts.ocp_indent_compat.v ->
            let indent =
              if Poly.(c.conf.fmt_opts.break_separators.v = `Before) then 2
              else 0
            in
            Some
              (fits_breaks ""
                 (String.make (Int.max 1 (indent - String.length pro)) ' ') )
        | _ -> None
      in
      let fmt_ret_typ = fmt_core_type c (sub_typ ~ctx ret_typ) in
      fmt_arrow_type c ~ctx ?indent ~parens:parenze_constraint_ctx
        ~parent_has_parens:parens args (Some fmt_ret_typ)
  | Ptyp_constr (lid, []) -> fmt_longident_loc c lid
  | Ptyp_constr (lid, [t1]) ->
      hvbox
        (Params.Indent.type_constr c.conf)
        ( fmt_core_type c (sub_typ ~ctx t1)
        $ space_break $ fmt_longident_loc c lid )
  | Ptyp_constr (lid, t1N) ->
      hvbox
        (Params.Indent.type_constr c.conf)
        ( wrap_fits_breaks c.conf "(" ")"
            (list t1N (Params.comma_sep c.conf)
               (sub_typ ~ctx >> fmt_core_type c) )
        $ space_break $ fmt_longident_loc c lid )
  | Ptyp_extension ext ->
      hvbox c.conf.fmt_opts.extension_indent.v (fmt_extension c ctx ext)
  | Ptyp_package (id, cnstrs, attrs) ->
      hvbox 2
        ( hovbox 0 (str "module" $ space_break $ fmt_longident_loc c id)
        $ fmt_package_type c ctx cnstrs
        $ fmt_attributes c attrs )
  | Ptyp_open (lid, typ) ->
      hvbox 2
        ( hvbox 0 (fmt_longident_loc c lid $ str ".(")
        $ break 0 0
        $ fmt_core_type c (sub_typ ~ctx typ)
        $ str ")" )
  | Ptyp_poly ([], _) ->
      impossible "produced by the parser, handled elsewhere"
  | Ptyp_poly (a1N, t) ->
      let ctx_is_value_constraint = function Vc _ -> true | _ -> false in
      let break, box_core_type =
        match
          (c.conf.fmt_opts.break_separators.v, c.conf.fmt_opts.break_colon.v)
        with
        | `Before, `Before when ctx_is_value_constraint ctx0 ->
            (* Special formatting for leading [->] in let bindings. *)
            let indent =
              match t.ptyp_desc with Ptyp_arrow _ -> 3 | _ -> 2
            in
            (break 1 indent, Some false)
        | _ -> (space_break, None)
      in
      hovbox_if box 0
        ( hovbox 0
            ( list a1N space_break (fun {txt; _} -> fmt_type_var txt)
            $ str "." )
        $ break
        $ fmt_core_type c ?box:box_core_type ~pro_space:false
            (sub_typ ~ctx t) )
  | Ptyp_tuple typs ->
      hvbox 0
        (wrap_if parenze_constraint_ctx (str "(") (str ")")
           (wrap_fits_breaks_if ~space:false c.conf parens "(" ")"
              (list typs
                 (space_break $ str "* ")
                 (sub_typ ~ctx >> fmt_core_type c) ) ) )
  | Ptyp_var s -> fmt_type_var s
  | Ptyp_variant (rfs, flag, lbls) ->
      let row_fields rfs =
        match rfs with
        | [] -> Cmts.fmt_within c ~pro:noop ptyp_loc
        | _ ->
            list rfs
              ( if
                  in_type_declaration
                  && Poly.(c.conf.fmt_opts.type_decl.v = `Sparse)
                then force_break $ str "| "
                else space_break $ str "| " )
              (fmt_row_field c ctx)
      in
      let protect_token = Exposed.Right.(list ~elt:row_field) rfs in
      let space_around = c.conf.fmt_opts.space_around_variants.v in
      let closing =
        let empty = List.is_empty rfs in
        let force =
          match c.conf.fmt_opts.type_decl.v with
          | `Sparse -> Option.some_if space_around Break
          | `Compact -> None
        in
        let nspaces = if empty then 0 else 1 in
        let space = (protect_token || space_around) && not empty in
        fits_breaks
          (if space && not empty then " ]" else "]")
          ~hint:(nspaces, 0) "]" ?force
      in
      hvbox 0
        ( match (flag, lbls, rfs) with
        | Closed, None, [{prf_desc= Rinherit _; _}] ->
            str "[ | " $ row_fields rfs $ closing
        | Closed, None, _ ->
            let opening = if space_around then "[ " else "[" in
            fits_breaks opening "[ " $ row_fields rfs $ closing
        | Open, None, _ -> str "[> " $ row_fields rfs $ closing
        | Closed, Some [], _ -> str "[< " $ row_fields rfs $ closing
        | Closed, Some ls, _ ->
            str "[< " $ row_fields rfs $ str " > "
            $ list ls space_break (variant_var c)
            $ closing
        | Open, Some _, _ -> impossible "not produced by parser" )
  | Ptyp_object ([], closed_flag) ->
      wrap
        (str "<" $ space_break)
        (str ">")
        ( match closed_flag with
        | OClosed -> Cmts.fmt_within c ~pro:noop ~epi:(str " ") ptyp_loc
        | OOpen loc -> Cmts.fmt c loc (str "..") $ space_break )
  | Ptyp_object (fields, closed_flag) ->
      let fmt_field {pof_desc; pof_attributes; pof_loc} =
        let fmt_field =
          match pof_desc with
          | Otag (lab_loc, typ) ->
              (* label loc * attributes * core_type -> object_field *)
              let field_loose =
                match c.conf.fmt_opts.field_space.v with
                | `Loose | `Tight_decl -> true
                | `Tight -> false
              in
              fmt_str_loc c lab_loc
              $ fmt_if field_loose (str " ")
              $ str ":" $ space_break
              $ fmt_core_type c (sub_typ ~ctx typ)
          | Oinherit typ -> fmt_core_type c (sub_typ ~ctx typ)
        in
        Cmts.fmt c pof_loc
        @@ hvbox 4
             ( hvbox 2 fmt_field
             $ fmt_attributes_and_docstrings c pof_attributes )
      in
      hvbox 0
        (wrap (str "< ") (str " >")
           ( list fields (space_break $ str "; ") fmt_field
           $
           match closed_flag with
           | OClosed -> noop
           | OOpen loc -> space_break $ str "; " $ Cmts.fmt c loc @@ str ".."
           ) )
  | Ptyp_class (lid, []) -> fmt_longident_loc c ~pre:"#" lid
  | Ptyp_class (lid, [t1]) ->
      fmt_core_type c (sub_typ ~ctx t1)
      $ space_break
      $ fmt_longident_loc c ~pre:"#" lid
  | Ptyp_class (lid, t1N) ->
      wrap_fits_breaks c.conf "(" ")"
        (list t1N (Params.comma_sep c.conf)
           (sub_typ ~ctx >> fmt_core_type c) )
      $ space_break
      $ fmt_longident_loc c ~pre:"#" lid

and fmt_package_type c ctx cnstrs =
  let fmt_cstr ~first ~last:_ (lid, typ) =
    fmt_or first (break 1 0) (break 1 1)
    $ hvbox 2
        ( fmt_or first (str "with type ") (str "and type ")
        $ fmt_longident_loc c lid $ str " =" $ space_break
        $ fmt_core_type c (sub_typ ~ctx typ) )
  in
  list_fl cnstrs fmt_cstr

and fmt_row_field c ctx {prf_desc; prf_attributes; prf_loc} =
  let c = update_config c prf_attributes in
  let row =
    match prf_desc with
    | Rtag (name, const, typs) ->
        variant_var c name
        $ fmt_if (not (const && List.is_empty typs)) (str " of" $ space_break)
        $ fmt_if (const && not (List.is_empty typs)) (str " & ")
        $ list typs (space_break $ str "& ") (sub_typ ~ctx >> fmt_core_type c)
    | Rinherit typ -> fmt_core_type c (sub_typ ~ctx typ)
  in
  hvbox 0
    ( Cmts.fmt c prf_loc (hvbox (Params.Indent.variant_type_arg c.conf) row)
    $ fmt_attributes_and_docstrings c prf_attributes )

and fmt_pattern_attributes c xpat k =
  match xpat.ast.ppat_attributes with
  | [] -> k
  | attrs ->
      let parens_attr =
        match xpat.ast.ppat_desc with
        | Ppat_or _ -> (
          match xpat.ctx with
          | Pat {ppat_desc= Ppat_construct _; _}
           |Pat {ppat_desc= Ppat_variant _; _} ->
              true
          | _ -> false )
        | _ -> (
          match xpat.ctx with
          | Exp {pexp_desc= Pexp_object _; _}
           |Cl {pcl_desc= Pcl_structure _; _} ->
              false
          | _ -> true )
      in
      let box =
        match (xpat.ast.ppat_desc, c.conf.fmt_opts.sequence_style.v) with
        | (Ppat_record _ | Ppat_array _ | Ppat_list _), `Terminator -> hovbox
        | _ -> hvbox
      in
      box
        (if parens_attr then 1 else 0)
        (Params.parens_if parens_attr c.conf
           (k $ fmt_attributes c ~pre:Space attrs) )

and fmt_pattern ?ext c ?pro ?parens ?(box = false)
    ({ctx= ctx0; ast= pat} as xpat) =
  protect c (Pat pat)
  @@
  let ctx = Pat pat in
  let {ppat_desc; ppat_attributes; ppat_loc; _} = pat in
  update_config_maybe_disabled c ppat_loc ppat_attributes
  @@ fun c ->
  let parens = match parens with Some b -> b | None -> parenze_pat xpat in
  (match ctx0 with Pat {ppat_desc= Ppat_tuple _; _} -> hvbox 0 | _ -> Fn.id)
  @@ ( match ppat_desc with
     | Ppat_or _ -> fun k -> Cmts.fmt c ppat_loc @@ k
     | _ -> fun k -> Cmts.fmt c ppat_loc @@ (fmt_opt pro $ k) )
  @@ hovbox_if box 0
  @@ fmt_pattern_attributes c xpat
  @@
  match ppat_desc with
  | Ppat_any -> str "_"
  | Ppat_var {txt; loc} ->
      Cmts.fmt c loc
      @@ wrap_if
           (Std_longident.String_id.is_symbol txt)
           (str "( ") (str " )") (str txt)
  | Ppat_alias (pat, {txt; loc}) ->
      let paren_pat =
        match pat.ppat_desc with
        | Ppat_or _ | Ppat_tuple _ -> Some true
        | _ -> None
      in
      hovbox 0
        (wrap_fits_breaks_if ~space:false c.conf parens "(" ")"
           (hovbox 0
              ( fmt_pattern c ?parens:paren_pat (sub_pat ~ctx pat)
              $ space_break $ str "as" $ space_break
              $ Cmts.fmt c loc
                  (wrap_if
                     (Std_longident.String_id.is_symbol txt)
                     (str "( ") (str " )") (str txt) ) ) ) )
  | Ppat_constant const -> fmt_constant c const
  | Ppat_interval (l, u) -> fmt_constant c l $ str " .. " $ fmt_constant c u
  | Ppat_tuple pats ->
      let parens =
        parens || Poly.(c.conf.fmt_opts.parens_tuple_patterns.v = `Always)
      in
      hvbox 0
        (Params.wrap_tuple ~parens ~no_parens_if_break:false c.conf
           (List.map pats ~f:(sub_pat ~ctx >> fmt_pattern c)) )
  | Ppat_construct ({txt= Lident (("()" | "[]") as txt); loc}, None) ->
      let opn = txt.[0] and cls = txt.[1] in
      Cmts.fmt c loc
        (hvbox 0
           (wrap (char opn) (char cls)
              (Cmts.fmt_within c ~pro:(str " ") ~epi:(str " ") ppat_loc) ) )
  | Ppat_construct (lid, None) -> fmt_longident_loc c lid
  | Ppat_cons lp ->
      Cmts.fmt c ppat_loc
        (hvbox 0 (fmt_pat_cons c ~parens (List.map lp ~f:(sub_pat ~ctx))))
  | Ppat_construct (lid, Some (exists, pat)) ->
      cbox
        (Params.Indent.variant c.conf ~parens)
        (Params.parens_if parens c.conf
           (hvbox 2
              ( fmt_longident_loc c lid $ space_break
              $ ( match exists with
                | [] -> noop
                | names ->
                    hvbox 0
                      (Params.parens c.conf
                         ( str "type "
                         $ list names space_break (fmt_str_loc c) ) )
                    $ space_break )
              $ fmt_pattern c (sub_pat ~ctx pat) ) ) )
  | Ppat_variant (lbl, None) -> variant_var c lbl
  | Ppat_variant (lbl, Some pat) ->
      cbox
        (Params.Indent.variant c.conf ~parens)
        (Params.parens_if parens c.conf
           (hvbox 2
              ( variant_var c lbl $ space_break
              $ fmt_pattern c (sub_pat ~ctx pat) ) ) )
  | Ppat_record (flds, closed_flag) ->
      let fmt_field (lid, typ1, pat) =
        let typ1 = Option.map typ1 ~f:(sub_typ ~ctx) in
        let rhs =
          Option.map pat ~f:(fun p -> fmt_pattern c (sub_pat ~ctx p))
        in
        hvbox 0 @@ Cmts.fmt c ppat_loc @@ fmt_record_field c ?typ1 ?rhs lid
      in
      let p = Params.get_record_pat c.conf ~ctx:ctx0 pat in
      let last_sep, fmt_underscore =
        match closed_flag with
        | OClosed -> (true, noop)
        | OOpen loc ->
            let underscore =
              p.sep_before
              $ hvbox 0 (Cmts.fmt c loc (str "_"))
              $ p.sep_after_final
            in
            (false, underscore)
      in
      let last_loc (lid, t, pat) =
        match (t, pat) with
        | _, Some pat -> pat.ppat_loc
        | Some t, _ -> t.ptyp_loc
        | _ -> lid.loc
      in
      let fmt_fields =
        fmt_elements_collection c ~last_sep p last_loc ppat_loc fmt_field
          flds
      in
      hvbox_if parens 0
        (Params.parens_if parens c.conf
           (p.box (fmt_fields $ fmt_underscore)) )
  | Ppat_array [] ->
      hvbox 0
        (wrap_fits_breaks c.conf "[|" "|]" (Cmts.fmt_within c ppat_loc))
  | Ppat_array pats ->
      let p = Params.get_array_pat c.conf ~ctx:ctx0 pat in
      p.box
        (fmt_elements_collection c p Pat.location ppat_loc
           (sub_pat ~ctx >> fmt_pattern c >> hvbox 0)
           pats )
  | Ppat_list pats ->
      let p = Params.get_list_pat c.conf ~ctx:ctx0 pat in
      p.box
        (fmt_elements_collection c p Pat.location ppat_loc
           (sub_pat ~ctx >> fmt_pattern c >> hvbox 0)
           pats )
  | Ppat_or pats ->
      Cmts.relocate c.cmts ~src:ppat_loc ~before:(List.hd_exn pats).ppat_loc
        ~after:(List.last_exn pats).ppat_loc ;
      let nested = Params.get_or_pattern_is_nested ~ctx:ctx0 pat in
      let xpats = List.map ~f:(sub_pat ~ctx) pats in
      let space p =
        match p.ppat_desc with
        | Ppat_constant
            {pconst_desc= Pconst_integer (i, _) | Pconst_float (i, _); _}
          -> (
          match i.[0] with '-' | '+' -> true | _ -> false )
        | _ -> false
      in
      let break {ast= p1; _} {ast= p2; _} =
        Poly.(c.conf.fmt_opts.break_cases.v = `Nested)
        || (not (Pat.is_simple p1))
        || (not (Pat.is_simple p2))
        || Cmts.has_after c.cmts p1.ppat_loc
      in
      let open_box =
        match c.conf.fmt_opts.break_cases.v with
        | `Fit_or_vertical | `Vertical -> open_hvbox
        | `Fit | `Nested | `Toplevel | `All -> open_hovbox
      in
      hvbox 0
        ( list_fl (List.group xpats ~break)
            (fun ~first:first_grp ~last:_ xpat_grp ->
              list_fl xpat_grp (fun ~first ~last:_ xpat ->
                  (* side effects of Cmts.fmt_before before [fmt_pattern] is
                     important *)
                  let loc = xpat.ast.ppat_loc in
                  let cmts_before = Cmts.has_before c.cmts loc in
                  let leading_cmt =
                    let pro, adj =
                      if first_grp && first then (noop, space_break)
                      else (space_break, noop)
                    in
                    Cmts.fmt_before ~pro c loc ~adj ~eol:noop
                  in
                  let pro =
                    if first_grp && first then
                      fmt_opt pro
                      $ fits_breaks
                          (if parens then "(" else "")
                          (if nested then "( " else "")
                      $ open_box (-2)
                    else if first then
                      Params.get_or_pattern_sep c.conf ~nested ~cmts_before
                      $ open_box (-2)
                    else
                      Params.get_or_pattern_sep c.conf ~nested ~cmts_before
                        ~space:(space xpat.ast)
                  in
                  leading_cmt $ fmt_pattern c ~box:true ~pro xpat )
              $ close_box )
        $ fmt_or nested
            (fits_breaks (if parens then ")" else "") ~hint:(1, 2) ")")
            (fits_breaks (if parens then ")" else "") "") )
  | Ppat_constraint (pat, typ) ->
      hvbox 2
        (Params.parens_if parens c.conf
           ( fmt_pattern c (sub_pat ~ctx pat)
           $ ( match ctx0 with
             | Exp {pexp_desc= Pexp_let _; _} -> space_break $ str ": "
             | _ -> str " :" $ space_break )
           $ fmt_core_type c (sub_typ ~ctx typ) ) )
  | Ppat_type lid -> fmt_longident_loc c ~pre:"#" lid
  | Ppat_lazy pat ->
      cbox 2
        (Params.parens_if parens c.conf
           ( str "lazy"
           $ fmt_extension_suffix c ext
           $ space_break
           $ fmt_pattern c (sub_pat ~ctx pat) ) )
  | Ppat_unpack (name, pt) ->
      let fmt_constraint_opt pt k =
        match pt with
        | Some (id, cnstrs, attrs) ->
            hovbox 0
              (Params.parens_if parens c.conf
                 (hvbox 1
                    ( hovbox 0
                        (k $ space_break $ str ": " $ fmt_longident_loc c id)
                    $ fmt_package_type c ctx cnstrs
                    $ fmt_attributes c attrs ) ) )
        | None -> wrap_fits_breaks_if ~space:false c.conf parens "(" ")" k
      in
      fmt_constraint_opt pt
        ( str "module"
        $ fmt_extension_suffix c ext
        $ char ' ' $ fmt_str_loc_opt c name )
  | Ppat_exception pat ->
      cbox 2
        (Params.parens_if parens c.conf
           ( str "exception"
           $ fmt_extension_suffix c ext
           $ space_break
           $ fmt_pattern c (sub_pat ~ctx pat) ) )
  | Ppat_effect (pat1, pat2) ->
      cbox 2
        (Params.parens_if parens c.conf
           ( str "effect"
           $ fmt_extension_suffix c ext
           $ space_break
           $ fmt_pattern c (sub_pat ~ctx pat1)
           $ str ", "
           $ fmt_pattern c (sub_pat ~ctx pat2) ) )
  | Ppat_extension
      ( ext
      , PPat
          ( ( { ppat_desc=
                  ( Ppat_lazy _ | Ppat_unpack _ | Ppat_exception _
                  | Ppat_effect _ )
              ; ppat_loc
              ; ppat_attributes= []
              ; _ } as pat )
          , _ ) )
    when Source.extension_using_sugar ~name:ext ~payload:ppat_loc ->
      hvbox 0 (fmt_pattern ~ext c ~box (sub_pat ~ctx pat))
  | Ppat_extension ext ->
      hvbox c.conf.fmt_opts.extension_indent.v (fmt_extension c ctx ext)
  | Ppat_open (lid, pat) ->
      let can_skip_parens =
        match pat.ppat_desc with
        | Ppat_array _ | Ppat_list _ | Ppat_record _ -> true
        | Ppat_tuple _ ->
            Poly.(c.conf.fmt_opts.parens_tuple_patterns.v = `Always)
        | Ppat_construct ({txt= Lident "[]"; _}, None) -> true
        | _ -> false
      in
      let opn, cls = if can_skip_parens then (".", "") else (".(", ")") in
      cbox 0
        ( fmt_longident_loc c lid
        $ wrap (str opn) (str cls)
            (break 0 2 $ fmt_pattern c (sub_pat ~ctx pat)) )

and fmt_param_val c ctx : pparam_val -> _ = function
  | ( ((Labelled l | Optional l) as lbl)
    , None
    , ( { ppat_desc=
            ( Ppat_var {txt; loc= _}
            | Ppat_constraint
                ( {ppat_desc= Ppat_var {txt; loc= _}; ppat_attributes= []; _}
                , _ ) )
        ; ppat_attributes= []
        ; _ } as pat ) )
    when String.equal l.txt txt ->
      let symbol = match lbl with Labelled _ -> "~" | _ -> "?" in
      let xpat = sub_pat ~ctx pat in
      cbox 0 (str symbol $ fmt_pattern ~box:true c xpat)
  | (Optional _ as lbl), None, pat ->
      let xpat = sub_pat ~ctx pat in
      let has_attr = not (List.is_empty pat.ppat_attributes) in
      let outer_parens, inner_parens =
        match pat.ppat_desc with
        | Ppat_any | Ppat_var _ -> (false, false)
        | Ppat_unpack _ -> (not has_attr, true)
        | Ppat_tuple _ -> (false, true)
        | Ppat_or _ -> (has_attr, true)
        | _ -> (not has_attr, false)
      in
      cbox 2
        ( fmt_label lbl (str ":" $ cut_break)
        $ hovbox 0
          @@ Params.parens_if outer_parens c.conf
               (fmt_pattern ~parens:inner_parens c xpat) )
  | ((Labelled _ | Nolabel) as lbl), None, pat ->
      let xpat = sub_pat ~ctx pat in
      cbox 2 (fmt_label lbl (str ":" $ cut_break) $ fmt_pattern c xpat)
  | ( Optional l
    , Some exp
    , ({ppat_desc= Ppat_var {txt; loc= _}; ppat_attributes= []; _} as pat) )
    when String.equal l.txt txt ->
      let xexp = sub_exp ~ctx exp in
      let xpat = sub_pat ~ctx pat in
      cbox 0
        (wrap (str "?(") (str ")")
           ( fmt_pattern c ~box:true xpat
           $ str " =" $ break 1 2
           $ hovbox 2 (fmt_expression c xexp) ) )
  | ( Optional l
    , Some exp
    , ( { ppat_desc=
            Ppat_constraint ({ppat_desc= Ppat_var {txt; loc= _}; _}, _)
        ; ppat_attributes= []
        ; _ } as pat ) )
    when String.equal l.txt txt ->
      let xexp = sub_exp ~ctx exp in
      let xpat = sub_pat ~ctx pat in
      cbox 0
        (wrap (str "?(") (str ")")
           ( fmt_pattern c ~parens:false ~box:true xpat
           $ str " =" $ break 1 2 $ fmt_expression c xexp ) )
  | Optional l, Some exp, pat ->
      let xexp = sub_exp ~ctx exp in
      let xpat = sub_pat ~ctx pat in
      let parens =
        match xpat.ast.ppat_desc with
        | Ppat_unpack _ -> None
        | _ -> Some false
      in
      cbox 2
        ( str "?" $ str l.txt
        $ wrap
            (str ":" $ cut_break $ str "(")
            (str ")")
            ( fmt_pattern c ?parens ~box:true xpat
            $ str " =" $ break 1 2 $ fmt_expression c xexp ) )
  | (Labelled _ | Nolabel), Some _, _ -> impossible "not accepted by parser"

and fmt_param_newtype c = function
  | [] -> impossible "not accepted by parser"
  | names ->
      cbox 0
        (Params.parens c.conf
           (str "type " $ list names space_break (fmt_str_loc c)) )

and fmt_expr_fun_arg c fp =
  let ctx = Fpe fp in
  Cmts.fmt c fp.pparam_loc
  @@
  match fp.pparam_desc with
  | Pparam_val x -> fmt_param_val c ctx x
  | Pparam_newtype x -> fmt_param_newtype c x

and fmt_class_fun_arg c fp =
  let ctx = Fpc fp in
  Cmts.fmt c fp.pparam_loc @@ fmt_param_val c ctx fp.pparam_desc

and fmt_expr_fun_args c args = list args space_break (fmt_expr_fun_arg c)

and fmt_class_fun_args c args = list args space_break (fmt_class_fun_arg c)

and fmt_indexop_access c ctx ~fmt_atrs ~has_attr ~parens x =
  let {pia_lhs; pia_kind; pia_paren; pia_rhs} = x in
  let wrap_paren =
    match pia_paren with
    | Paren -> wrap (str "(") (str ")")
    | Bracket -> wrap (str "[") (str "]")
    | Brace -> wrap (str "{") (str "}")
  in
  let inner_wrap = has_attr && Option.is_some pia_rhs in
  Params.parens_if parens c.conf
    (hovbox 0
       ( Params.parens_if inner_wrap c.conf
           ( fmt_expression c (sub_exp ~ctx pia_lhs)
           $ str "."
           $ ( match pia_kind with
             | Builtin idx ->
                 wrap_paren (fmt_expression c (sub_exp ~ctx idx))
             | Dotop (path, op, [idx]) ->
                 opt path (fun x -> fmt_longident_loc c x $ str ".")
                 $ str op
                 $ wrap_paren (fmt_expression c (sub_exp ~ctx idx))
             | Dotop (path, op, idx) ->
                 opt path (fun x -> fmt_longident_loc c x $ str ".")
                 $ str op
                 $ wrap_paren
                     (list idx
                        (str ";" $ space_break)
                        (sub_exp ~ctx >> fmt_expression c) ) )
           $ opt pia_rhs (fun e ->
                 fmt_assign_arrow c $ fmt_expression c (sub_exp ~ctx e) ) )
       $ fmt_atrs ) )

(** Format a [Pexp_function]. [wrap_intro] wraps up to after the [->] and is
    responsible for breaking. *)
and fmt_function ?(last_arg = false) ?force_closing_paren ~ctx ~ctx0
    ~wrap_intro ?box:(should_box = true) ~label ?(parens = false) ?ext ~attrs
    ~loc c (args, typ, body) =
  let should_box =
    should_box
    ||
    match (args, typ, body) with
    | _ :: _, _, Pfunction_cases _ -> true
    | _ -> false
  in
  let has_label = match label with Nolabel -> false | _ -> true in
  (* Make sure the comment is placed after the eventual label but not into
     the inner box if no label is present. Side effects of Cmts.fmt c.cmts
     before Sugar.fun_ is important. *)
  let has_cmts_outer, cmts_outer, cmts_inner =
    let eol = if has_label then Some cut_break else None in
    let has_cmts = Cmts.has_before c.cmts loc in
    let cmts = Cmts.fmt_before ?eol c loc in
    if has_label then (false, noop, cmts) else (has_cmts, cmts, noop)
  in
  let break_fun = Params.Exp.break_fun_kw c.conf ~ctx ~ctx0 ~last_arg in
  let (label_sep : t) =
    (* Break between the label and the fun to avoid ocp-indent's alignment.
       If a label is present, arguments should be indented more than the
       arrow and the eventually breaking [fun] keyword. *)
    if c.conf.fmt_opts.ocp_indent_compat.v then str ":" $ cut_break
    else str ":"
  in
  let fmt_typ typ = fmt_type_pcstr c ~ctx ~constraint_ctx:`Fun typ in
  let fmt_fun_args_typ args typ =
    let kw =
      str "fun"
      $ fmt_extension_suffix c ext
      $ fmt_attributes c ~pre:Blank attrs
      $ break_fun
    and args = fmt_expr_fun_args c args
    and annot = Option.map ~f:fmt_typ typ
    and epi =
      Params.Exp.break_fun_decl_args c.conf ~ctx:ctx0 ~last_arg $ str "->"
    in
    Params.Exp.box_fun_decl_args ~last_arg ~ctx ~ctx0 c.conf ~parens ~kw
      ~args ~annot ~epi
  in
  let lead_with_function_kw =
    match (args, body) with [], Pfunction_cases _ -> true | _ -> false
  in
  (* [head] is [fun args ->] or [function]. [body] is an expression or the
     cases. *)
  let head, body, box, closing_paren_offset =
    match (args, typ, body) with
    | _ :: _, _, Pfunction_body body ->
        (* Only [fun]. *)
        let head = fmt_fun_args_typ args typ in
        let body ~pro = pro $ fmt_expression c (sub_exp ~ctx body) in
        let box, closing_paren_offset =
          Params.Exp.box_fun_expr c.conf ~source:c.source ~ctx0 ~ctx
        in
        let closing_paren_offset =
          if should_box then closing_paren_offset else ~-2
        in
        (head, body, box, closing_paren_offset)
    | [], _, Pfunction_body _ -> assert false
    | args, typ, Pfunction_cases (cs, function_loc, cs_attrs) ->
        (* [fun _ -> function] or [function]. [spilled_attrs] are extra attrs
           to add to the [function] keyword. *)
        let fun_, spilled_attrs, box =
          match (args, typ) with
          | [], None ->
              ( noop
              , attrs
              , hvbox (Params.Indent.function_ c.conf ~ctx ~ctx0 ~parens) )
          | [], Some _ -> assert false
          | args, typ ->
              ( fmt_fun_args_typ args typ $ space_break
              , []
              , hvbox
                  (Params.Indent.docked_function_after_fun c.conf ~parens
                     ~ctx0 ~ctx ) )
        in
        let function_ =
          let pre =
            if Params.Exp.function_attrs_sp c.conf ~ctx0 ~ctx then Some Blank
            else None
          in
          hvbox_if
            (Cmts.has_before c.cmts function_loc)
            0
            (Cmts.fmt_before c function_loc $ str "function")
          $ fmt_extension_suffix c ext
          $ fmt_attributes ?pre c spilled_attrs
          $ fmt_attributes ?pre c cs_attrs
        in
        let box_cases ~pro cases =
          let pro_inner, pro_outer, indent =
            (* Formatting of if-then-else relies on the ~box argument. *)
            match (args, should_box) with
            | [], true -> (pro, noop, None)
            | _ -> (noop, pro, Some 0)
          in
          pro_outer
          $ Params.Exp.box_function_cases c.conf ?indent ~ctx ~ctx0 ~parens
              (pro_inner $ cases)
        in
        let box, cases =
          match cs with
          | [{pc_lhs; pc_guard= _; pc_rhs}]
            when Params.Exp.single_line_function ~ctx ~ctx0 ~args ->
              ( hovbox 4
              , hvbox 0
                  ( fmt_pattern c ~pro:(if_newline "| ") (sub_pat ~ctx pc_lhs)
                  $ space_break $ str "->" )
                $ space_break
                $ cbox 0 (fmt_expression c (sub_exp ~ctx pc_rhs)) )
          | _ -> (box, fmt_cases c ctx cs)
        in
        (fun_ $ function_, box_cases cases, box, 0)
  in
  let space_opn_parens, space_cls_parens =
    match ctx0 with
    | Exp {pexp_desc= Pexp_infix _; _}
      when lead_with_function_kw
           && not c.conf.fmt_opts.break_infix_before_func.v ->
        (str " ", true)
    | _ -> (noop, false)
  in
  let opn_paren, cls_paren =
    if parens then
      ( str "(" $ space_opn_parens
      , closing_paren c ~force_space:space_cls_parens
          ?force:force_closing_paren ~offset:closing_paren_offset )
    else (noop, noop)
  in
  (* When the option disambiguate_non_breaking_match is set, if the function
     fits on one line it has to have parens. [fit_breaks] is used for that.
     It cannot be used directly with [opn_paren] because its deep inside
     other boxes that will not be broken. Because of this we wrap the whole
     with another pair of parens, although only if the regular one are
     absent. *)
  let disambiguate_parens_wrap =
    if (not parens) && c.conf.fmt_opts.disambiguate_non_breaking_match.v then
      wrap (fits_breaks "(" "") (fits_breaks ")" "")
    else Fn.id
  in
  let body =
    let pro =
      wrap_intro
        (hvbox_if has_cmts_outer 0
           ( cmts_outer
           $ Params.Exp.box_fun_decl ~ctx0 c.conf
               (fmt_label label label_sep $ cmts_inner $ opn_paren $ head) ) )
    in
    body ~pro $ cls_paren
  in
  let box k = if should_box then box k else k in
  box (disambiguate_parens_wrap body) $ Cmts.fmt_after c loc

and fmt_label_arg ?(box = true) ?eol c (lbl, ({ast= arg; _} as xarg)) =
  match (lbl, arg.pexp_desc) with
  | (Labelled l | Optional l), Pexp_ident {txt= Lident i; loc}
    when String.equal l.txt i && List.is_empty arg.pexp_attributes ->
      Cmts.fmt c loc @@ Cmts.fmt c ?eol arg.pexp_loc @@ fmt_label lbl noop
  | ( (Labelled l | Optional l)
    , Pexp_constraint ({pexp_desc= Pexp_ident {txt= Lident i; _}; _}, _) )
    when String.equal l.txt i
         && List.is_empty arg.pexp_attributes
         && Ocaml_version.(
              compare c.conf.opr_opts.ocaml_version.v Releases.v4_14_0 >= 0 )
    ->
      let lbl =
        match lbl with
        | Labelled _ -> str "~"
        | Optional _ -> str "?"
        | Nolabel -> noop
      in
      lbl $ fmt_expression c ~box xarg
  | (Labelled _ | Optional _), _ when Cmts.has_after c.cmts xarg.ast.pexp_loc
    ->
      let cmts_after = Cmts.fmt_after c xarg.ast.pexp_loc in
      hvbox_if box 2
        ( hvbox_if box 0
            (fmt_expression c
               ~pro:(fmt_label lbl (str ":" $ break 0 2))
               ~box xarg )
        $ cmts_after )
  | (Labelled _ | Optional _), Pexp_function (args, typ, body) ->
      let wrap_intro x = hovbox 2 x $ space_break in
      fmt_function ~box ~ctx:(Exp arg) ~wrap_intro ~ctx0:xarg.ctx ~label:lbl
        ~parens:true ~attrs:arg.pexp_attributes ~loc:arg.pexp_loc c
        (args, typ, body)
  | _ ->
      let label_sep : t =
        if box || c.conf.fmt_opts.wrap_fun_args.v then str ":" $ cut_break
        else str ":"
      in
      fmt_label lbl label_sep $ fmt_expression c ~box xarg

and expression_width c xe =
  String.length
    (Cmts.preserve ~cache_key:(Expression xe.ast)
       (fun () -> fmt_expression c xe)
       c.cmts )

and fmt_args_grouped ?epi:(global_epi = noop) c ctx args =
  let fmt_arg c ~first:_ ~last (lbl, arg) =
    let ({ast; _} as xarg) = sub_exp ~ctx arg in
    let box =
      match ast.pexp_desc with Pexp_function _ -> Some false | _ -> None
    in
    let break_after =
      match (ast.pexp_desc, c.conf.fmt_opts.break_string_literals.v) with
      | Pexp_constant _, `Auto when not last ->
          fits_breaks "" ~hint:(1000, -2) ""
      | _ -> noop
    in
    hovbox
      (Params.Indent.fun_args_group c.conf ~lbl ast)
      (fmt_label_arg c ?box (lbl, xarg) $ break_after)
    $ fmt_if (not last) (break_unless_newline 1 0)
  in
  let fmt_args ~first ~last args =
    hovbox
      (if first then 2 else 0)
      (list_fl args (fmt_arg c) $ fmt_if last global_epi)
    $ fmt_if (not last) (break 1 0)
  in
  let is_simple (lbl, x) =
    let xexp = sub_exp ~ctx x in
    let output =
      Cmts.preserve
        ~cache_key:(Arg (lbl, x))
        (fun () ->
          let cmts = Cmts.drop_before c.cmts x.pexp_loc in
          fmt_arg ~first:false ~last:false {c with cmts} (lbl, x) )
        c.cmts
    in
    let breaks = String.(rstrip output |> is_substring ~substring:"\n   ") in
    is_simple c.conf (expression_width c) xexp && not breaks
  in
  let break x y =
    Cmts.has_after c.cmts (snd x).pexp_loc || not (is_simple x && is_simple y)
  in
  let groups =
    if c.conf.fmt_opts.wrap_fun_args.v then List.group args ~break
    else List.map args ~f:(fun x -> [x])
  in
  list_fl groups fmt_args

and fmt_sequence c ?ext ~has_attr parens width xexp fmt_atrs =
  let fmt_sep c ?(force_break = false) xe1 ext xe2 =
    let break =
      let l1 = xe1.ast.pexp_loc and l2 = xe2.ast.pexp_loc in
      if sequence_blank_line c l1 l2 then str "\n" $ Fmt.force_break
      else if c.conf.fmt_opts.break_sequences.v || force_break then
        Fmt.force_break
      else if parens && Poly.(c.conf.fmt_opts.sequence_style.v = `Before)
      then break 1 (-2)
      else break 1 0
    in
    match c.conf.fmt_opts.sequence_style.v with
    | `Before ->
        break $ str ";"
        $ fmt_extension_suffix c ext
        $ fmt_or (Option.is_some ext)
            (fmt_or parens space_break (Fmt.break 1 2))
            (str " ")
    | `Separator -> str " ;" $ fmt_extension_suffix c ext $ break
    | `Terminator -> str ";" $ fmt_extension_suffix c ext $ break
  in
  let is_simple x = is_simple c.conf width x in
  let break (_, xexp1) (_, xexp2) =
    not (is_simple xexp1 && is_simple xexp2)
  in
  let elts = Sugar.sequence c.cmts xexp in
  ( match elts with
  | (None, _) :: (first_ext, _) :: _ ->
      let compare {txt= x; _} {txt= y; _} = String.compare x y in
      assert (Option.compare compare first_ext ext = 0)
  | _ -> impossible "at least two elements" ) ;
  let grps = List.group elts ~break in
  let fmt_seq ~prev (ext, curr) ~next:_ =
    let f (_, prev) = fmt_sep c prev ext curr in
    opt prev f $ fmt_expression c curr
  in
  let fmt_seq_list ~prev x ~next:_ =
    let f prev =
      let prev = snd (List.last_exn prev) in
      let ext, curr = List.hd_exn x in
      fmt_sep c ~force_break:true prev ext curr
    in
    opt prev f $ list_pn x fmt_seq
  in
  hvbox 0
    (Params.Exp.wrap c.conf ~parens
       ( Params.parens_if has_attr c.conf
           (hvbox_if (parens || has_attr) 0 @@ list_pn grps fmt_seq_list)
       $ fmt_atrs ) )

and fmt_infix_op_args c ~parens xexp op_args =
  let op_prec = prec_ast (Exp xexp.ast) in
  let op_prec_higher_than_apply =
    match op_prec with Some p -> Prec.compare p Apply > 0 | None -> false
  in
  let groups =
    let width xe = expression_width c xe in
    let not_simple arg = not (is_simple c.conf width arg) in
    let break (cmts_before1, _, (_, arg1)) (_, _, (_, arg2)) =
      Option.is_some cmts_before1 || not_simple arg1 || not_simple arg2
    in
    let break_infix =
      match c.conf.fmt_opts.break_infix.v with
      | `Wrap -> `Wrap
      | `Fit_or_vertical -> `Fit_or_vertical
      | `Wrap_or_vertical -> (
        match op_prec with
        | Some p when Prec.compare p InfixOp1 < 0 -> `Fit_or_vertical
        | Some _ ->
            if
              List.exists op_args ~f:(fun (_, _, (_, {ast= arg; _})) ->
                  match Ast.prec_ast (Exp arg) with
                  | Some p when Prec.compare p Apply <= 0 -> true
                  | Some _ -> false
                  | None -> false )
            then `Fit_or_vertical
            else `Wrap
        | None -> impossible "Pexp_apply expressions always have a prec" )
    in
    match break_infix with
    | `Wrap -> List.group op_args ~break
    | `Fit_or_vertical -> List.map ~f:(fun x -> [x]) op_args
  in
  let is_not_indented {ast= exp; _} =
    match exp.pexp_desc with
    | Pexp_letop _ | Pexp_letexception _ | Pexp_letmodule _ ->
        (* In 0.25.1 and before, these used to not break and were aligned at
           the end of the operator. Preserve the break to avoid introducing
           large diffs while allowing consistent formatting. *)
        Source.begins_line c.source exp.pexp_loc
    | Pexp_ifthenelse _ | Pexp_let _ | Pexp_match _ | Pexp_sequence _
     |Pexp_try _ | Pexp_letopen _ ->
        true
    | _ -> false
  in
  let fmt_arg ~pro ~very_last xarg =
    let parens =
      ((not very_last) && exposed_right_exp Ast.Non_apply xarg.ast)
      || parenze_exp xarg
    in
    if Params.Exp.Infix_op_arg.dock c.conf xarg then
      (* Indentation of docked fun or function start before the operator. *)
      hovbox 2 (fmt_expression c ~parens ~box:false ~pro xarg)
    else
      match xarg.ast.pexp_desc with
      | Pexp_function _ -> hvbox 0 (fmt_expression c ~pro ~parens xarg)
      | _ ->
          hvbox 0
            ( pro
            $ hovbox_if (not very_last) 2 (fmt_expression c ~parens xarg) )
  in
  let fmt_op_arg_group ~first:first_grp ~last:last_grp args =
    let indent = if first_grp && parens then -2 else 0 in
    hovbox indent
      (list_fl args
         (fun ~first ~last (cmts_before, cmts_after, (op, xarg)) ->
           let very_first = first_grp && first in
           let very_last = last_grp && last in
           let pro, before_arg =
             let break =
               if very_last && is_not_indented xarg then space_break
               else
                 fmt_if
                   ((not very_first) && not op_prec_higher_than_apply)
                   (str " ")
             in
             match cmts_after with
             | Some c -> (noop, hovbox 0 (op $ space_break $ c))
             | None -> (op $ break, noop)
           in
           fmt_opt cmts_before $ before_arg
           $ fmt_arg ~pro ~very_last xarg
           $ fmt_if ((not last) && not op_prec_higher_than_apply) (break 1 0) )
      )
    $ fmt_if ((not last_grp) && not op_prec_higher_than_apply) (break 1 0)
  in
  Params.Exp.Infix_op_arg.wrap c.conf ~parens
    ~parens_nested:(Ast.parenze_nested_exp xexp)
    (Params.Align.infix_op c.conf (list_fl groups fmt_op_arg_group))

and fmt_pat_cons c ~parens args =
  let groups =
    let not_simple arg = not (Pat.is_simple arg.ast) in
    let break args1 args2 = not_simple args1 || not_simple args2 in
    (* [break-infix = wrap-or-vertical] is not applicable for patterns as
       there are no infix operators allowed besides [::], falling back on
       [fit-or-vertical] is arbitrary. *)
    match c.conf.fmt_opts.break_infix.v with
    | `Wrap -> List.group args ~break
    | `Fit_or_vertical | `Wrap_or_vertical -> List.map ~f:(fun x -> [x]) args
  in
  let fmt_op_arg_group ~first:first_grp ~last:last_grp args =
    let indent = if first_grp && parens then -2 else 0 in
    hovbox indent
      (list_fl args (fun ~first ~last xarg ->
           let very_first = first_grp && first in
           let very_last = last_grp && last in
           hvbox 0
             ( fmt_if (not very_first) (str ":: ")
             $ hovbox_if (not very_last) 2 (fmt_pattern c ~box:true xarg) )
           $ fmt_if (not last) (break 1 0) ) )
    $ fmt_if (not last_grp) (break 1 0)
  in
  Params.Exp.Infix_op_arg.wrap c.conf ~parens ~parens_nested:false
    (list_fl groups fmt_op_arg_group)

and fmt_match c ?pro ~parens ?ext ctx xexp cs e0 keyword =
  let ctx0 = xexp.ctx in
  let indent = Params.match_indent c.conf ~parens ~ctx:ctx0 in
  hvbox indent
    ( fmt_opt pro
    $ Params.Exp.wrap c.conf ~parens ~disambiguate:true
      @@ Params.Align.match_ c.conf ~xexp
      @@ ( hvbox 0
             ( str keyword
             $ fmt_extension_suffix c ext
             $ fmt_attributes c xexp.ast.pexp_attributes
             $ break 1 2
             $ fmt_expression c (sub_exp ~ctx e0)
             $ space_break $ str "with" )
         $ space_break $ fmt_cases c ctx cs ) )

and fmt_expression c ?(box = true) ?(pro = noop) ?eol ?parens
    ?(indent_wrap = 0) ?ext ({ast= exp; ctx= ctx0} as xexp) =
  protect c (Exp exp)
  @@
  let {pexp_desc; pexp_loc; pexp_attributes; _} = exp in
  update_config_maybe_disabled c pexp_loc pexp_attributes
  @@ fun c ->
  Cmts.relocate_wrongfully_attached_cmts c.cmts c.source exp ;
  let pro =
    (* Some expressions format the 'pro' and comments differently. *)
    let cmts_in_pro =
      match exp.pexp_desc with
      | Pexp_function _ -> noop
      | _ -> Cmts.fmt_before c ?eol pexp_loc
    in
    cmts_in_pro $ pro
  in
  let fmt_cmts_after k = k $ Cmts.fmt_after c pexp_loc in
  let fmt_atrs = fmt_attributes c ~pre:Space pexp_attributes in
  let has_attr = not (List.is_empty pexp_attributes) in
  let parens = Option.value parens ~default:(parenze_exp xexp) in
  let ctx = Exp exp in
  let fmt_args_grouped ?epi e0 a1N =
    fmt_args_grouped c ctx ?epi ((Nolabel, e0) :: a1N)
  in
  hvbox_if box 0 ~name:"expr"
  @@ fmt_cmts_after
  @@
  match pexp_desc with
  | Pexp_apply (_, []) -> impossible "not produced by parser"
  | Pexp_sequence
      ( { pexp_desc=
            Pexp_extension
              ( name
              , PStr
                  [ ( { pstr_desc=
                          Pstr_eval
                            ( ( { pexp_desc=
                                    Pexp_function
                                      (args, typ, (Pfunction_body _ as body))
                                ; _ } as call )
                            , [] )
                      ; pstr_loc= _ } as _pld ) ] )
        ; _ }
      , e2 ) ->
      let is_simple x = is_simple c.conf (expression_width c) x in
      let break xexp1 xexp2 = not (is_simple xexp1 && is_simple xexp2) in
      let grps =
        List.group
          (List.map ~f:snd (Sugar.sequence c.cmts (sub_exp ~ctx e2)))
          ~break
      in
      let fmt_grp grp =
        list grp (str " ;" $ space_break) (fmt_expression c)
      in
      pro
      $ hvbox 0
          (Params.parens_if parens c.conf
             ( hvbox c.conf.fmt_opts.extension_indent.v
                 (wrap (str "[") (str "]")
                    (fmt_function ~ctx:(Exp call) ~ctx0
                       ~wrap_intro:(fun x ->
                         str "%"
                         $ hovbox 2 (fmt_str_loc c name $ space_break $ x)
                         $ space_break )
                       ~label:Nolabel ~parens:false
                       ~attrs:call.pexp_attributes ~loc:call.pexp_loc c
                       (args, typ, body) ) )
             $ space_break $ str ";" $ space_break
             $ list grps (str " ;" $ force_break) fmt_grp ) )
  | Pexp_infix
      ( {txt= "|>"; loc}
      , e0
      , { pexp_desc=
            Pexp_extension
              ( name
              , PStr
                  [ ( { pstr_desc=
                          Pstr_eval
                            ( ( { pexp_desc=
                                    Pexp_function
                                      (args, typ, (Pfunction_body _ as body))
                                ; _ } as retn )
                            , [] )
                      ; pstr_loc= _ } as _pld ) ] )
        ; _ } ) ->
      pro
      $ hvbox 0
          (Params.Exp.wrap c.conf ~parens
             ( fmt_expression c (sub_exp ~ctx e0)
             $ force_newline
             $ Cmts.fmt c loc (str "|>" $ force_newline)
             $ hvbox c.conf.fmt_opts.extension_indent.v
                 (wrap (str "[") (str "]")
                    (fmt_function ~ctx:(Exp retn) ~ctx0
                       ~wrap_intro:(fun x ->
                         str "%"
                         $ hovbox 2 (fmt_str_loc c name $ space_break $ x)
                         $ space_break )
                       ~label:Nolabel ~parens:false
                       ~attrs:retn.pexp_attributes ~loc:retn.pexp_loc c
                       (args, typ, body) ) ) ) )
  | Pexp_infix ({txt= ":="; loc}, r, v)
    when is_simple c.conf (expression_width c) (sub_exp ~ctx r) ->
      let bol_indent = Params.Indent.assignment_operator_bol c.conf in
      let has_cmts_before = Cmts.has_before c.cmts loc in
      let cmts_before =
        let indent =
          (* Use the same break for comment and operator. Comments are placed
             according to indentation. *)
          match c.conf.fmt_opts.assignment_operator.v with
          | `Begin_line -> bol_indent
          | `End_line -> 0
        in
        Cmts.fmt_before c loc ~pro:(break 1 indent) ~epi:noop ~adj:noop
      in
      let cmts_after = Cmts.fmt_after c loc ~pro:noop ~epi:noop in
      pro
      $ Params.parens_if parens c.conf
          (hovbox 0
             ( match c.conf.fmt_opts.assignment_operator.v with
             | `Begin_line ->
                 hvbox 0 (fmt_expression c (sub_exp ~ctx r) $ cmts_before)
                 $ break 1 bol_indent $ str ":= " $ cmts_after
                 $ hvbox 2 (fmt_expression c (sub_exp ~ctx v))
             | `End_line ->
                 let break_infix =
                   if has_cmts_before then break 1 0 else str " "
                 in
                 hvbox 0
                   ( fmt_expression c (sub_exp ~ctx r)
                   $ cmts_before $ break_infix $ str ":=" )
                 $ break 1 2 $ cmts_after
                 $ hvbox 2 (fmt_expression c (sub_exp ~ctx v)) ) )
  | Pexp_prefix ({txt= ("~-" | "~-." | "~+" | "~+.") as op; loc}, e1) ->
      let op =
        if Location.width loc = String.length op - 1 then
          String.sub op ~pos:1 ~len:(String.length op - 1)
        else op
      in
      let spc = fmt_if (Exp.exposed_left e1) space_break in
      pro
      $ Params.parens_if parens c.conf
          ( Cmts.fmt c pexp_loc
            @@ hvbox 2 (str op $ spc $ fmt_expression c (sub_exp ~ctx e1))
          $ fmt_atrs )
  | Pexp_infix (({txt= id; _} as op), l, ({pexp_desc= Pexp_ident _; _} as r))
    when Std_longident.String_id.is_hash_getter id ->
      pro
      $ Params.parens_if parens c.conf
          ( fmt_expression c (sub_exp ~ctx l)
          $ hvbox 0 (fmt_str_loc c op)
          $ fmt_expression c (sub_exp ~ctx r) )
  | Pexp_infix (op, l, ({pexp_desc= Pexp_function (args, typ, body); _} as r))
    when not c.conf.fmt_opts.break_infix_before_func.v ->
      let xr = sub_exp ~ctx r in
      let parens_r = parenze_exp xr in
      let indent_wrap = if parens then -2 else 0 in
      let followed_by_infix_op =
        match body with
        | Pfunction_body
            {pexp_desc= Pexp_infix (_, _, {pexp_desc= Pexp_function _; _}); _}
          ->
            true
        | _ -> false
      in
      pro
      $ wrap_fits_breaks_if c.conf parens "(" ")"
          ( fmt_function ~ctx:(Exp r)
              ~ctx0:ctx (*~box:false to fix regression on infix *)
              ~parens:parens_r
              ~wrap_intro:(fun intro ->
                hvbox indent_wrap
                  ( fmt_if has_attr (str "(")
                  $ fmt_expression ~indent_wrap c (sub_exp ~ctx l)
                  $ space_break
                  $ hovbox 0 (fmt_str_loc c op $ space_break $ intro) )
                $ fmt_or followed_by_infix_op force_break space_break )
              ~label:Nolabel ~attrs:r.pexp_attributes ~loc:r.pexp_loc c
              (args, typ, body)
          $ fmt_if has_attr (str ")")
          $ fmt_atrs )
  | Pexp_infix _ ->
      let op_args = Sugar.Exp.infix c.cmts (prec_ast (Exp exp)) xexp in
      let inner_wrap = parens || has_attr in
      let outer_wrap =
        match ctx0 with
        (* infix operator used to build a function *)
        | Exp {pexp_desc= Pexp_apply (f, _); _} when phys_equal f exp ->
            has_attr && parens
        | Exp
            { pexp_desc=
                Pexp_apply ({pexp_desc= Pexp_ident {txt= id; loc= _}; _}, _)
            ; _ }
          when not (Std_longident.is_infix id) ->
            has_attr && parens
        | Lb {pvb_body= Pfunction_body body; _} when phys_equal body exp ->
            has_attr && parens
        | _ -> has_attr && not parens
      in
      let infix_op_args =
        List.map op_args ~f:(fun (op, arg) ->
            match op with
            | Some op ->
                (* side effects of Cmts.fmt_before before fmt_expression is
                   important *)
                let adj = force_break in
                let fmt_before_cmts =
                  if Cmts.has_before c.cmts op.loc then
                    Some (Cmts.fmt_before ~adj c op.loc)
                  else None
                in
                (* The comments before the first arg are put there, so that
                   they are printed after the operator and the box is
                   correctly broken before the following arguments. Keeping
                   the comments in the arg box would not break properly the
                   current box. OTOH, relocating the comments would put them
                   before the operator in some cases and make the formatting
                   unstable. *)
                let fmt_after_cmts =
                  if
                    Cmts.has_after c.cmts op.loc
                    || Cmts.has_before c.cmts arg.ast.pexp_loc
                  then
                    Some
                      ( Cmts.fmt_after c ~epi:adj op.loc
                      $ Cmts.fmt_before ~adj ~epi:adj c arg.ast.pexp_loc )
                  else None
                in
                let fmt_op = fmt_str_loc c op in
                (fmt_before_cmts, fmt_after_cmts, (fmt_op, arg))
            | None -> (None, None, (noop, arg)) )
      in
      pro
      $ hvbox_if outer_wrap 0
          (Params.parens_if outer_wrap c.conf
             (hvbox indent_wrap
                ( fmt_infix_op_args ~parens:inner_wrap c xexp infix_op_args
                $ fmt_atrs ) ) )
  | Pexp_prefix (op, e) ->
      let has_cmts = Cmts.has_before c.cmts e.pexp_loc in
      pro
      $ hvbox 2
          (Params.Exp.wrap c.conf ~parens
             ( fmt_str_loc c op $ fmt_if has_cmts cut_break
             $ fmt_expression c ~box (sub_exp ~ctx e)
             $ fmt_atrs ) )
  | Pexp_apply (e0, e1N1) -> (
      let wrap =
        if c.conf.fmt_opts.wrap_fun_args.v then hovbox 2 else hvbox 2
      in
      let (lbl, last_arg), args_before =
        match List.rev e1N1 with
        | [] -> assert false
        | hd :: tl -> (hd, List.rev tl)
      in
      let intro_epi, expr_epi =
        (* [intro_epi] should be placed inside the inner most box but before
           anything. [expr_epi] is placed in the outermost box, outside of
           parenthesis. *)
        let dock_fun_arg =
          (* Do not dock the arguments when there's more than one. *)
          (not c.conf.fmt_opts.ocp_indent_compat.v)
          || Location.line_difference e0.pexp_loc last_arg.pexp_loc = 0
        in
        if parens || not dock_fun_arg then (noop, pro) else (pro, noop)
      in
      match last_arg.pexp_desc with
      | Pexp_function (largs, ltyp, lbody)
        when List.for_all args_before ~f:(fun (_, eI) ->
                 is_simple c.conf (fun _ -> 0) (sub_exp ~ctx eI) ) ->
          let inner_ctx = Exp last_arg in
          let inner_parens, outer_parens =
            (* Don't disambiguate parentheses in some cases, also affect
               indentation. *)
            match lbody with
            | Pfunction_cases _ when not c.conf.fmt_opts.ocp_indent_compat.v
              ->
                (parens, false)
            | _ -> (false, parens)
          in
          let args =
            let wrap_intro x =
              fmt_if inner_parens (str "(")
              $ hvbox 0
                  ( intro_epi
                  $ wrap
                      ( fmt_args_grouped e0 args_before
                      $ break 1 0 $ hvbox 0 x ) )
              $ break 1 0
            in
            let force_closing_paren =
              if Location.is_single_line pexp_loc c.conf.fmt_opts.margin.v
              then Fit
              else Break
            in
            fmt_function ~last_arg:true ~force_closing_paren ~ctx:inner_ctx
              ~ctx0:ctx ~wrap_intro ~label:lbl ~parens:true
              ~attrs:last_arg.pexp_attributes ~loc:last_arg.pexp_loc c
              (largs, ltyp, lbody)
          in
          hvbox_if has_attr 0
            ( expr_epi
            $ Params.parens_if outer_parens c.conf
                (args $ fmt_atrs $ fmt_if inner_parens (str ")")) )
      | _ ->
          let fmt_atrs =
            fmt_attributes c ~pre:(Break (1, -2)) pexp_attributes
          in
          let force =
            if Location.is_single_line pexp_loc c.conf.fmt_opts.margin.v then
              Fit
            else Break
          in
          pro
          $ fmt_if parens (str "(")
          $ hvbox 2
              ( fmt_args_grouped ~epi:fmt_atrs e0 e1N1
              $ fmt_if parens (closing_paren c ~force ~offset:(-3)) ) )
  | Pexp_array [] ->
      pro
      $ hvbox 0
          (Params.parens_if parens c.conf
             ( wrap_fits_breaks c.conf "[|" "|]" (Cmts.fmt_within c pexp_loc)
             $ fmt_atrs ) )
  | Pexp_array e1N ->
      let p = Params.get_array_expr c.conf in
      pro
      $ hvbox_if has_attr 0
          (Params.parens_if parens c.conf
             ( p.box
                 (fmt_expressions c (expression_width c) (sub_exp ~ctx) e1N
                    (sub_exp ~ctx >> fmt_expression c)
                    p pexp_loc )
             $ fmt_atrs ) )
  | Pexp_list e1N ->
      let p = Params.get_list_expr c.conf in
      let offset =
        if c.conf.fmt_opts.dock_collection_brackets.v then 0 else 2
      in
      let cmt_break = break 1 offset in
      pro
      $ hvbox_if has_attr 0
          (Params.parens_if parens c.conf
             ( p.box
                 (fmt_expressions c (expression_width c) (sub_exp ~ctx) e1N
                    (fun e ->
                      let fmt_cmts = Cmts.fmt c ~eol:cmt_break e.pexp_loc in
                      fmt_cmts @@ (sub_exp ~ctx >> fmt_expression c) e )
                    p pexp_loc )
             $ fmt_atrs ) )
  | Pexp_assert e0 ->
      let paren_body =
        if Exp.is_symbol e0 || Exp.is_monadic_binding e0 then
          not (List.is_empty e0.pexp_attributes)
        else parenze_exp (sub_exp ~ctx e0)
      in
      pro
      $ hovbox 0
          (Params.parens_if parens c.conf
             (hvbox 0
                ( hvbox 2
                    ( str "assert"
                    $ fmt_extension_suffix c ext
                    $ fmt_or paren_body (str " (" $ cut_break) space_break
                    $ fmt_expression c ~parens:false (sub_exp ~ctx e0) )
                $ fmt_if paren_body (closing_paren c)
                $ fmt_atrs ) ) )
  | Pexp_constant const ->
      pro
      $ Params.parens_if
          (parens || not (List.is_empty pexp_attributes))
          c.conf
          (fmt_constant c const $ fmt_atrs)
  | Pexp_constraint (e, t) ->
      pro
      $ hvbox
          (Params.Indent.exp_constraint c.conf)
          (Params.parens_if parens c.conf
             ( wrap_fits_breaks ~space:false c.conf "(" ")"
                 ( fmt_expression c (sub_exp ~ctx e)
                 $ space_break $ str ": "
                 $ fmt_core_type c (sub_typ ~ctx t) )
             $ fmt_atrs ) )
  | Pexp_construct ({txt= Lident (("()" | "[]") as txt); loc}, None) ->
      let opn = char txt.[0] and cls = char txt.[1] in
      pro
      $ Cmts.fmt c loc
        @@ hvbox 0
             (Params.parens_if parens c.conf
                ( wrap opn cls
                    (Cmts.fmt_within c ~pro:(str " ") ~epi:(str " ") pexp_loc)
                $ fmt_atrs ) )
  | Pexp_construct (lid, None) ->
      pro
      $ Params.parens_if parens c.conf (fmt_longident_loc c lid $ fmt_atrs)
  | Pexp_cons l ->
      pro
      $ Cmts.fmt c pexp_loc
          ( hvbox indent_wrap
              (fmt_infix_op_args c ~parens xexp
                 (List.mapi l ~f:(fun i e ->
                      ( None
                      , None
                      , (fmt_if (i > 0) (str "::"), sub_exp ~ctx e) ) ) ) )
          $ fmt_atrs )
  | Pexp_construct (lid, Some arg) ->
      pro
      $ Params.parens_if parens c.conf
          ( hvbox 2
              ( fmt_longident_loc c lid $ space_break
              $ fmt_expression c (sub_exp ~ctx arg) )
          $ fmt_atrs )
  | Pexp_variant (s, arg) ->
      pro
      $ Params.parens_if parens c.conf
          (hvbox
             (Params.Indent.variant c.conf ~parens)
             ( variant_var c s
             $ opt arg (space_break >$ (sub_exp ~ctx >> fmt_expression c))
             $ fmt_atrs ) )
  | Pexp_field (exp, lid) ->
      pro
      $ hvbox 2
          (Params.parens_if parens c.conf
             ( fmt_expression c (sub_exp ~ctx exp)
             $ cut_break $ str "." $ fmt_longident_loc c lid $ fmt_atrs ) )
  | Pexp_function (args, typ, body) ->
      let wrap_intro intro =
        hovbox ~name:"fmt_expression | Pexp_function" 2 (pro $ intro)
        $ space_break
      in
      fmt_function ~wrap_intro ~box ~ctx ~ctx0 ~label:Nolabel ~parens ?ext
        ~attrs:pexp_attributes ~loc:pexp_loc c (args, typ, body)
  | Pexp_ident {txt; loc} ->
      let outer_parens = has_attr && parens in
      pro
      $ Cmts.fmt c loc
        @@ wrap_if outer_parens (str "(") (str ")")
        @@ (fmt_longident txt $ Cmts.fmt_within c loc $ fmt_atrs)
  | Pexp_ifthenelse (if_branches, else_) ->
      let last_loc =
        match else_ with
        | Some (e, _) -> e.pexp_loc
        | None -> (List.last_exn if_branches).if_body.pexp_loc
      in
      Cmts.relocate c.cmts ~src:pexp_loc ~before:pexp_loc ~after:last_loc ;
      let parens_prev_bch = ref false in
      let cnd_exps =
        let with_conds =
          List.map if_branches ~f:(fun x ->
              ( Some (sub_exp ~ctx x.if_cond)
              , x.if_loc_then
              , sub_exp ~ctx x.if_body
              , x.if_attrs ) )
        in
        match else_ with
        | Some (x, loc_else) ->
            List.rev
              ((None, loc_else, sub_exp ~ctx x, []) :: List.rev with_conds)
        | None -> with_conds
      in
      pro
      $ hvbox 0
          ( Params.Exp.wrap c.conf ~parens:(parens || has_attr)
              (hvbox 0
                 (list_fl cnd_exps
                    (fun
                      ~first
                      ~last
                      (xcond, keyword_loc, xbch, pexp_attributes)
                    ->
                      let symbol_parens = Exp.is_symbol xbch.ast in
                      let parens_bch =
                        parenze_exp xbch && not symbol_parens
                      in
                      let cmts_before_kw = Cmts.fmt_before c keyword_loc in
                      let cmts_after_kw =
                        if Cmts.has_after c.cmts keyword_loc then
                          Some (Cmts.fmt_after c keyword_loc)
                        else None
                      in
                      let p =
                        Params.get_if_then_else c.conf ~first ~last
                          ~parens_bch ~parens_prev_bch:!parens_prev_bch
                          ~xcond ~xbch ~expr_loc:pexp_loc
                          ~fmt_extension_suffix:
                            (Option.map ext ~f:(fun _ ->
                                 fmt_extension_suffix c ext ) )
                          ~fmt_attributes:
                            (fmt_attributes c ~pre:Blank pexp_attributes)
                          ~fmt_cond:(fmt_expression ~box:false c)
                          ~cmts_before_kw ~cmts_after_kw
                      in
                      parens_prev_bch := parens_bch ;
                      p.box_branch
                        ( p.cond
                        $ p.box_keyword_and_expr
                            ( p.branch_pro
                            $ p.wrap_parens
                                ( fmt_expression c ?box:p.box_expr
                                    ~parens:false ?pro:p.expr_pro
                                    ?eol:p.expr_eol p.branch_expr
                                $ p.break_end_branch ) ) )
                      $ fmt_if (not last) p.space_between_branches ) ) )
          $ fmt_atrs )
  | Pexp_let (lbs, body, loc_in) ->
      let bindings =
        Sugar.Let_binding.of_let_bindings ~ctx lbs.pvbs_bindings
      in
      let fmt_expr = fmt_expression c (sub_exp ~ctx body) in
      pro
      $ fmt_let_bindings c ~ctx0:ctx ~parens ~fmt_atrs ~fmt_expr ~has_attr
          ~loc_in lbs.pvbs_rec bindings body
  | Pexp_letop {let_; ands; body; loc_in} ->
      let bd = Sugar.Let_binding.of_binding_ops (let_ :: ands) in
      let fmt_expr = fmt_expression c (sub_exp ~ctx body) in
      pro
      $ fmt_let_bindings c ~ctx0:ctx ~parens ~fmt_atrs ~fmt_expr ~has_attr
          ~loc_in Nonrecursive bd body
  | Pexp_letexception (ext_cstr, exp) ->
      let pre =
        str "let exception" $ fmt_extension_suffix c ext $ space_break
      in
      pro
      $ hvbox 0
          ( Params.parens_if
              (parens || not (List.is_empty pexp_attributes))
              c.conf
              ( hvbox 0
                  ( hvbox 2
                      (hvbox 2
                         (pre $ fmt_extension_constructor c ctx ext_cstr) )
                  $ space_break $ str "in" )
              $ force_break
              $ fmt_expression c (sub_exp ~ctx exp) )
          $ fmt_atrs )
  | Pexp_letmodule (name, args, pmod, exp) ->
      let keyword = "let module" in
      let xbody = sub_mod ~ctx pmod in
      let xbody, xmty =
        match xbody.ast with
        | { pmod_desc= Pmod_constraint (body_me, body_mt)
          ; pmod_loc
          ; pmod_attributes= [] } ->
            Cmts.relocate c.cmts ~src:pmod_loc ~before:body_me.pmod_loc
              ~after:body_mt.pmty_loc ;
            (sub_mod ~ctx body_me, Some (sub_mty ~ctx body_mt))
        | _ -> (xbody, None)
      in
      let can_sparse =
        match xbody.ast.pmod_desc with
        | Pmod_apply _ | Pmod_apply_unit _ -> true
        | _ -> false
      in
      pro
      $ hvbox 0
          ( Params.parens_if
              (parens || not (List.is_empty pexp_attributes))
              c.conf
              ( hvbox 2
                  (fmt_module c ctx keyword ~eqty:":" name args (Some xbody)
                     xmty
                     ~attrs:(Ast_helper.Attr.ext_attrs ?ext ())
                     ~epi:(str "in") ~can_sparse ~rec_flag:false )
              $ force_break
              $ fmt_expression c (sub_exp ~ctx exp) )
          $ fmt_atrs )
  | Pexp_open (lid, e0) ->
      let can_skip_parens =
        (not (Cmts.has_before c.cmts e0.pexp_loc))
        && (not (Cmts.has_after c.cmts e0.pexp_loc))
        &&
        match e0.pexp_desc with
        | (Pexp_array _ | Pexp_list _ | Pexp_record _)
          when List.is_empty e0.pexp_attributes ->
            true
        | Pexp_tuple _ -> Poly.(c.conf.fmt_opts.parens_tuple.v = `Always)
        | Pexp_construct ({txt= Lident "[]"; _}, None) -> true
        | _ -> false
      in
      let outer_parens = has_attr && parens in
      let inner_parens = not can_skip_parens in
      pro
      $ hovbox 0
          (Params.parens_if outer_parens c.conf
             ( hvbox 0
                 ( hvbox 0
                     ( fmt_longident_loc c lid $ str "."
                     $ fmt_if inner_parens (str "(") )
                 $ break 0 2
                 $ fmt_expression c (sub_exp ~ctx e0)
                 $ fmt_if inner_parens (closing_paren c) )
             $ fmt_atrs ) )
  | Pexp_letopen
      ( { popen_override= flag
        ; popen_expr
        ; popen_attributes= attributes
        ; popen_loc }
      , e0 ) ->
      let override = is_override flag in
      let outer_parens = has_attr && parens in
      let inner_parens = has_attr || parens in
      (* [let open] extensions are external for now. *)
      (* let ext = attributes.attrs_extension in *)
      pro
      $ hovbox 0
          (Params.Exp.wrap c.conf ~parens:outer_parens ~fits_breaks:false
             ( hvbox 0
                 (Params.Exp.wrap c.conf ~parens:inner_parens
                    ~fits_breaks:false
                    (vbox 0
                       ( hvbox 0
                           ( fmt_module_statement c ~attributes
                               ~keyword:
                                 (hvbox 0
                                    ( str "let" $ break 1 0
                                    $ Cmts.fmt_before c popen_loc
                                    $ fmt_or override (str "open!")
                                        (str "open")
                                    $ opt ext (fun _ ->
                                          fmt_if override (str " ") )
                                    $ fmt_extension_suffix c ext ) )
                               (sub_mod ~ctx popen_expr)
                           $ Cmts.fmt_after c popen_loc
                           $ str " in" )
                       $ force_break
                       $ fmt_expression c (sub_exp ~ctx e0) ) ) )
             $ fmt_atrs ) )
  | Pexp_try (e0, [{pc_lhs; pc_guard; pc_rhs}])
    when Poly.(
           c.conf.fmt_opts.single_case.v = `Compact
           && c.conf.fmt_opts.break_cases.v <> `All
           && c.conf.fmt_opts.break_cases.v <> `Vertical ) ->
      (* side effects of Cmts.fmt_before before [fmt_pattern] is important *)
      let xpc_rhs = sub_exp ~ctx pc_rhs in
      let leading_cmt = Cmts.fmt_before c pc_lhs.ppat_loc in
      let parens_here, parens_for_exp =
        if c.conf.fmt_opts.leading_nested_match_parens.v then (false, None)
        else (parenze_exp xpc_rhs, Some false)
      in
      pro
      $ Params.Exp.wrap c.conf ~parens ~disambiguate:true
          (hvbox 2
             ( hvbox 0
                 ( str "try"
                 $ fmt_extension_suffix c ext
                 $ fmt_attributes c pexp_attributes
                 $ break 1 2
                 $ fmt_expression c (sub_exp ~ctx e0) )
             $ break 1 (-2)
             $ hvbox 0
                 ( hvbox 0
                     ( str "with" $ space_break $ leading_cmt
                     $ hvbox 0
                         ( fmt_pattern c ~pro:(if_newline "| ")
                             (sub_pat ~ctx pc_lhs)
                         $ opt pc_guard (fun g ->
                               space_break $ str "when "
                               $ fmt_expression c (sub_exp ~ctx g) )
                         $ space_break $ str "->"
                         $ fmt_if parens_here (str " (") ) )
                 $ break 1 2
                 $ cbox 0 (fmt_expression c ?parens:parens_for_exp xpc_rhs)
                 )
             $ fmt_if parens_here
                 ( match c.conf.fmt_opts.indicate_multiline_delimiters.v with
                 | `No -> str ")"
                 | `Space -> str " )"
                 | `Closing_on_separate_line -> break 1000 (-2) $ str ")" )
             ) )
  | Pexp_match (e0, cs) ->
      fmt_match c ~pro ~parens ?ext ctx xexp cs e0 "match"
  | Pexp_try (e0, cs) -> fmt_match c ~pro ~parens ?ext ctx xexp cs e0 "try"
  | Pexp_pack (me, pt) ->
      let outer_pro = pro in
      let outer_parens = parens && has_attr in
      let blk = fmt_module_expr c (sub_mod ~ctx me) in
      let align = Params.Align.module_pack c.conf ~me in
      let opn_paren =
        match c.conf.fmt_opts.indicate_multiline_delimiters.v with
        | `No | `Closing_on_separate_line -> str "("
        | `Space ->
            let level =
              if Option.is_none pt then 1
              else if Option.is_some blk.opn then 3
              else 2
            in
            fits_breaks ~level "(" "( "
      and cls_paren = closing_paren c ~offset:(-2) in
      let pro =
        fmt_if (not align) opn_paren
        $ str "module"
        $ fmt_extension_suffix c ext
        $ char ' '
      and epi = cls_paren in
      let fmt_mod m =
        match pt with
        | Some (id, cnstrs, attrs) ->
            hvbox 2
              ( hovbox 0 (m $ space_break $ str ": " $ fmt_longident_loc c id)
              $ fmt_package_type c ctx cnstrs
              $ fmt_attributes c attrs )
        | None -> m
      in
      outer_pro
      $ hvbox 0
          (Params.parens_if outer_parens c.conf
             ( fmt_if align opn_paren
             $ compose_module ~pro ~epi blk ~f:fmt_mod
             $ fmt_atrs ) )
  | Pexp_record (flds, default) ->
      let fmt_field (lid, tc, exp) =
        let typ1, typ2 =
          match tc with
          | Some (Pconstraint t1) -> (Some t1, None)
          | Some (Pcoerce (t1, t2)) -> (t1, Some t2)
          | None -> (None, None)
        in
        let typ1 = Option.map typ1 ~f:(sub_typ ~ctx) in
        let typ2 = Option.map typ2 ~f:(sub_typ ~ctx) in
        let rhs =
          Option.map exp ~f:(fun e -> fmt_expression c (sub_exp ~ctx e))
        in
        hvbox 0 @@ fmt_record_field c ?typ1 ?typ2 ?rhs lid
      in
      let p1, p2 = Params.get_record_expr c.conf in
      let last_loc (lid, tc, e) =
        match (tc, e) with
        | _, Some e -> e.pexp_loc
        | Some (Pcoerce (_, t2)), None -> t2.ptyp_loc
        | Some (Pconstraint t1), None -> t1.ptyp_loc
        | None, None -> lid.loc
      in
      let fmt_fields =
        fmt_elements_collection c p1 last_loc pexp_loc fmt_field flds
          ~pro:(break 1 2)
      in
      pro
      $ hvbox_if has_attr 0
          ( p1.box
              ( opt default (fun d ->
                    hvbox 2 (fmt_expression c (sub_exp ~ctx d) $ break 1 (-2))
                    $ str "with" $ p2.break_after_with )
              $ fmt_fields )
          $ fmt_atrs )
  | Pexp_extension
      ( ext
      , PStr
          [ { pstr_desc=
                Pstr_eval
                  ( ( {pexp_desc= Pexp_sequence _; pexp_attributes= []; _} as
                      e1 )
                  , _ )
            ; pstr_loc= _ } ] )
    when Source.extension_using_sugar ~name:ext ~payload:e1.pexp_loc
         && List.length (Sugar.sequence c.cmts xexp) > 1 ->
      pro
      $ fmt_sequence ~has_attr c parens (expression_width c) xexp fmt_atrs
          ~ext
  | Pexp_sequence _ ->
      pro
      $ fmt_sequence ~has_attr c parens (expression_width c) xexp fmt_atrs
          ?ext
  | Pexp_setfield (e1, lid, e2) ->
      pro
      $ hvbox 0
          (Params.Exp.wrap c.conf ~parens
             ( Params.parens_if has_attr c.conf
                 ( fmt_expression c (sub_exp ~ctx e1)
                 $ str "." $ fmt_longident_loc c lid $ fmt_assign_arrow c
                 $ fmt_expression c (sub_exp ~ctx e2) )
             $ fmt_atrs ) )
  | Pexp_tuple es ->
      let parens =
        match xexp.ctx with
        | Str {pstr_desc= Pstr_eval _; pstr_loc= _} -> false
        | Exp {pexp_desc= Pexp_indexop_access {pia_kind= Builtin idx; _}; _}
          when phys_equal exp idx ->
            false
        | Exp
            { pexp_desc= Pexp_indexop_access {pia_kind= Dotop (_, _, idx); _}
            ; _ }
          when List.exists idx ~f:(phys_equal exp) ->
            false
        | _ -> parens || Poly.(c.conf.fmt_opts.parens_tuple.v = `Always)
      in
      let no_parens_if_break =
        match xexp.ctx with
        | Exp {pexp_desc= Pexp_extension _; _} -> true
        | Pld _ -> true
        | Str {pstr_desc= Pstr_eval _; _} -> true
        | _ -> false
      in
      let outer_wrap = has_attr && parens in
      let inner_wrap = has_attr || parens in
      pro
      $ hvbox_if outer_wrap 0
          (Params.parens_if outer_wrap c.conf
             ( hvbox 0
                 (Params.wrap_tuple ~parens:inner_wrap ~no_parens_if_break
                    c.conf
                    (List.map es ~f:(sub_exp ~ctx >> fmt_expression c)) )
             $ fmt_atrs ) )
  | Pexp_lazy e ->
      pro
      $ hvbox 2
          (Params.Exp.wrap c.conf ~parens
             ( str "lazy"
             $ fmt_extension_suffix c ext
             $ space_break
             $ fmt_expression c (sub_exp ~ctx e)
             $ fmt_atrs ) )
  | Pexp_extension
      ( ext
      , PStr
          [ ( { pstr_desc=
                  Pstr_eval
                    ( ( { pexp_desc=
                            ( Pexp_while _ | Pexp_for _ | Pexp_match _
                            | Pexp_try _ | Pexp_let _ | Pexp_ifthenelse _
                            | Pexp_new _ | Pexp_letmodule _ | Pexp_object _
                            | Pexp_function _ | Pexp_letexception _
                            | Pexp_open _ | Pexp_assert _ | Pexp_lazy _
                            | Pexp_pack _ | Pexp_beginend _ | Pexp_letopen _
                              )
                        ; pexp_attributes= []
                        ; _ } as e1 )
                    , _ )
              ; pstr_loc= _ } as str ) ] )
    when Source.extension_using_sugar ~name:ext ~payload:e1.pexp_loc ->
      let outer_parens = has_attr && parens in
      let inner_parens = has_attr || parens in
      pro
      $ hvbox 0
          (Params.parens_if outer_parens c.conf
             ( fmt_expression c ~box ?eol ~parens:inner_parens ~ext
                 (sub_exp ~ctx:(Str str) e1)
             $ fmt_atrs ) )
  | Pexp_extension
      ( ext
      , PStr
          [ ( { pstr_desc=
                  Pstr_eval
                    ( ( {pexp_desc= Pexp_infix _; pexp_attributes= []; _} as
                        e1 )
                    , _ )
              ; pstr_loc= _ } as str ) ] )
    when List.is_empty pexp_attributes
         && Source.extension_using_sugar ~name:ext ~payload:e1.pexp_loc ->
      pro
      $ hvbox 0
          ( fmt_expression c ~box ?eol ~parens ~ext
              (sub_exp ~ctx:(Str str) e1)
          $ fmt_atrs )
  | Pexp_extension ext ->
      pro
      $ hvbox 0
          (Params.Exp.wrap c.conf ~parens
             ( hvbox c.conf.fmt_opts.extension_indent.v
                 (fmt_extension c ctx ext)
             $ fmt_atrs ) )
  | Pexp_for (p1, e1, e2, dir, e3) ->
      pro
      $ hvbox 0
          (Params.Exp.wrap c.conf ~parens
             ( hovbox 0
                 ( hvbox 2
                     ( hvbox 0
                         ( str "for"
                         $ fmt_extension_suffix c ext
                         $ break 1 2
                         $ hovbox 0
                             ( fmt_pattern c (sub_pat ~ctx p1)
                             $ space_break $ str "=" $ break 1 2
                             $ fmt_expression c (sub_exp ~ctx e1)
                             $ fmt_direction_flag dir
                             $ fmt_expression c (sub_exp ~ctx e2) )
                         $ space_break $ str "do" )
                     $ force_break
                     $ fmt_expression c (sub_exp ~ctx e3) )
                 $ force_break $ str "done" )
             $ fmt_atrs ) )
  | Pexp_coerce (e1, t1, t2) ->
      pro
      $ hvbox 2
          (Params.parens_if (parens && has_attr) c.conf
             ( wrap_fits_breaks ~space:false c.conf "(" ")"
                 ( fmt_expression c (sub_exp ~ctx e1)
                 $ opt t1
                     ( space_break $ str ": "
                     >$ (sub_typ ~ctx >> fmt_core_type c) )
                 $ space_break $ str ":> "
                 $ fmt_core_type c (sub_typ ~ctx t2) )
             $ fmt_atrs ) )
  | Pexp_while (e1, e2) ->
      pro
      $ hvbox 0
          (Params.Exp.wrap c.conf ~parens
             ( hovbox 0
                 ( hvbox 2
                     ( hvbox 0
                         ( str "while"
                         $ fmt_extension_suffix c ext
                         $ break 1 2
                         $ fmt_expression c (sub_exp ~ctx e1)
                         $ space_break $ str "do" )
                     $ force_break
                     $ fmt_expression c (sub_exp ~ctx e2) )
                 $ force_break $ str "done" )
             $ fmt_atrs ) )
  | Pexp_unreachable -> pro $ str "."
  | Pexp_send (exp, meth) ->
      pro
      $ hvbox 2
          (Params.parens_if parens c.conf
             ( fmt_expression c (sub_exp ~ctx exp)
             $ cut_break $ str "#" $ fmt_str_loc c meth $ fmt_atrs ) )
  | Pexp_new {txt; loc} ->
      pro
      $ Cmts.fmt c loc
        @@ hvbox 2
             (Params.parens_if parens c.conf
                ( str "new"
                $ fmt_extension_suffix c ext
                $ space_break $ fmt_longident txt $ fmt_atrs ) )
  | Pexp_object {pcstr_self; pcstr_fields} ->
      pro
      $ hvbox 0
          (Params.parens_if parens c.conf
             ( fmt_class_structure c ~ctx ?ext pcstr_self pcstr_fields
             $ fmt_atrs ) )
  | Pexp_override l -> (
      let fmt_field ({txt; loc}, f) =
        let eol = break 1 3 in
        let txt = Longident.lident txt in
        match f.pexp_desc with
        | Pexp_ident {txt= txt'; loc}
          when Std_longident.field_alias ~field:txt txt'
               && List.is_empty f.pexp_attributes ->
            Cmts.fmt c ~eol loc @@ fmt_longident txt'
        | _ ->
            Cmts.fmt c ~eol loc @@ fmt_longident txt
            $ str " = "
            $ fmt_expression c (sub_exp ~ctx f)
      in
      match l with
      | [] ->
          pro
          $ Params.parens_if parens c.conf
              ( wrap (str "{<") (str ">}") (Cmts.fmt_within c pexp_loc)
              $ fmt_atrs )
      | _ ->
          pro
          $ hvbox 0
              (Params.parens_if parens c.conf
                 ( wrap_fits_breaks ~space:false c.conf "{<" ">}"
                     (list l (break 0 1 $ str "; ") fmt_field)
                 $ fmt_atrs ) ) )
  | Pexp_setinstvar (name, expr) ->
      pro
      $ hvbox 0
          (Params.Exp.wrap c.conf ~parens
             ( Params.parens_if has_attr c.conf
                 ( fmt_str_loc c name $ fmt_assign_arrow c
                 $ hvbox 2 (fmt_expression c (sub_exp ~ctx expr)) )
             $ fmt_atrs ) )
  | Pexp_indexop_access x ->
      pro $ fmt_indexop_access c ctx ~fmt_atrs ~has_attr ~parens x
  | Pexp_hole -> pro $ hvbox 0 (fmt_hole () $ fmt_atrs)
  | Pexp_beginend e ->
      let wrap_beginend k =
        let opn =
          hvbox 0 (str "begin" $ fmt_extension_suffix c ext $ fmt_atrs)
        and cls = str "end" in
        hvbox 0 (wrap opn cls (wrap (break 1 2) force_break k))
      in
      pro
      $ wrap_beginend
          (fmt_expression c ~box ?eol ~parens:false ~indent_wrap ?ext
             (sub_exp ~ctx e) )
  | Pexp_parens e ->
      pro
      $ hvbox 0
          (fmt_expression c ~box ?eol ~parens:true ~indent_wrap ?ext
             (sub_exp ~ctx e) )
      $ fmt_atrs

and fmt_let_bindings c ~ctx0 ~parens ~has_attr ~fmt_atrs ~fmt_expr ~loc_in
    rec_flag bindings body =
  let indent_after_in =
    match body.pexp_desc with
    | Pexp_let _ | Pexp_letmodule _
     |Pexp_extension
        ( _
        , PStr
            [ { pstr_desc=
                  Pstr_eval
                    ( { pexp_desc= Pexp_let _ | Pexp_letmodule _
                      ; pexp_attributes= []
                      ; _ }
                    , _ )
              ; pstr_loc= _ } ] ) ->
        0
    | _ -> c.conf.fmt_opts.indent_after_in.v
  in
  fmt_let c ~ctx0 ~rec_flag ~bindings ~parens ~has_attr ~fmt_atrs ~fmt_expr
    ~loc_in ~body_loc:body.pexp_loc ~indent_after_in

and fmt_class_structure c ~ctx ?ext self_ fields =
  let update_config c i =
    match i.pcf_desc with
    | Pcf_attribute atr -> update_config c [atr]
    | _ -> c
  in
  let self_ =
    opt self_ (fun self_ ->
        space_break
        $ Params.parens c.conf
            (fmt_pattern c ~parens:false (sub_pat ~ctx self_)) )
  in
  let fmt_item c ctx ~prev:_ ~next:_ i = fmt_class_field c (sub_cf ~ctx i) in
  let ast x = Clf x in
  hvbox 2
    ( hvbox 0 (str "object" $ fmt_extension_suffix c ext $ self_)
    $ ( match fields with
      | {pcf_desc= Pcf_attribute a; _} :: _ when Attr.is_doc a -> str "\n"
      | _ -> noop )
    $ fmt_or (List.is_empty fields)
        (Cmts.fmt_within ~epi:noop c (Ast.location ctx))
        force_break
    $ fmt_item_list c ctx update_config ast fmt_item fields )
  $ fmt_or (List.is_empty fields) space_break force_break
  $ str "end"

(** [epi] is a function to ensure ordered access to comments. *)
and fmt_class_signature c ~ctx ~pro ~epi ?ext self_ fields =
  let update_config c i =
    match i.pctf_desc with
    | Pctf_attribute atr -> update_config c [atr]
    | _ -> c
  in
  let self_ =
    opt self_ (fun self_ ->
        let no_attr typ = List.is_empty typ.ptyp_attributes in
        space_break
        $ Params.parens_if (no_attr self_) c.conf
            (fmt_core_type c (sub_typ ~ctx self_)) )
  in
  let fmt_item c ctx ~prev:_ ~next:_ i =
    fmt_class_type_field c (sub_ctf ~ctx i)
  in
  let ast x = Ctf x in
  let cmts_within =
    if List.is_empty fields then (* Side effect order is important. *)
      Cmts.fmt_within ~pro:noop c (Ast.location ctx)
    else noop
  in
  hvbox 2
    ( hvbox 2 (pro $ str "object" $ fmt_extension_suffix c ext $ self_)
    $ space_break $ cmts_within
    $ fmt_item_list c ctx update_config ast fmt_item fields
    $ fmt_if (not (List.is_empty fields)) (break 1000 (-2))
    $ hvbox 0 (str "end" $ epi ()) )

and fmt_class_type ?(pro = noop) c ({ast= typ; _} as xtyp) =
  protect c (Cty typ)
  @@
  let {pcty_desc; pcty_loc; pcty_attributes} = typ in
  update_config_maybe_disabled_k c pcty_loc pcty_attributes
    ~if_disabled:(fun fmt -> vbox 2 (pro $ fmt) )
  @@ fun c ->
  let doc, atrs = doc_atrs pcty_attributes in
  let parens = parenze_cty xtyp in
  let ctx = Cty typ in
  let pro ~cmt =
    pro
    $ (if cmt then Cmts.fmt_before c pcty_loc else noop)
    $ fmt_if parens (str "(")
  and epi ~attrs =
    fmt_if parens (str ")")
    $ (if attrs then fmt_attributes c atrs else noop)
    $ Cmts.fmt_after c pcty_loc
    $ fmt_docstring c ~pro:space_break doc
  in
  match pcty_desc with
  | Pcty_constr (name, params) ->
      let params = List.map params ~f:(fun x -> (x, [])) in
      hvbox 2
        ( pro ~cmt:false
        $ Cmts.fmt_before c pcty_loc
        $ hovbox 0
            ( fmt_class_params c ctx params
            $ fmt_longident_loc c name $ epi ~attrs:true ) )
  | Pcty_signature {pcsig_self; pcsig_fields} ->
      let pro = pro ~cmt:true in
      let epi () = epi ~attrs:true in
      fmt_class_signature c ~ctx ~pro ~epi pcsig_self pcsig_fields
  | Pcty_arrow (args, rhs) ->
      Cmts.relocate c.cmts ~src:pcty_loc
        ~before:(List.hd_exn args).pap_type.ptyp_loc ~after:rhs.pcty_loc ;
      let pro =
        pro ~cmt:true
        $ fmt_arrow_type c ~ctx ~parens:false ~parent_has_parens:parens args
            None
        $ Params.Pcty.arrow c.conf ~rhs
      in
      fmt_class_type c ~pro (sub_cty ~ctx rhs) $ epi ~attrs:true
  | Pcty_extension ext ->
      hvbox 2 (pro ~cmt:true $ fmt_extension c ctx ext $ epi ~attrs:true)
  | Pcty_open (popen, cl) ->
      let pro =
        pro ~cmt:true
        $ fmt_open_description c ~keyword:"let open" ~kw_attributes:atrs
            popen
        $ str " in"
        $ Params.Pcty.break_let_open c.conf ~rhs:cl
      in
      fmt_class_type c ~pro (sub_cty ~ctx cl) $ epi ~attrs:false

and fmt_class_expr c ({ast= exp; ctx= ctx0} as xexp) =
  protect c (Cl exp)
  @@
  let {pcl_desc; pcl_loc; pcl_attributes} = exp in
  update_config_maybe_disabled c pcl_loc pcl_attributes
  @@ fun c ->
  let parens = parenze_cl xexp in
  let ctx = Cl exp in
  let fmt_args_grouped e0 a1N =
    (* TODO: consider [e0] when grouping *)
    fmt_class_expr c (sub_cl ~ctx e0)
    $ space_break
    $ fmt_args_grouped c ctx a1N
  in
  let fmt_cmts = Cmts.fmt c pcl_loc in
  let fmt_atrs = fmt_attributes c ~pre:Space pcl_attributes in
  hvbox 0 @@ fmt_cmts
  @@
  match pcl_desc with
  | Pcl_constr (name, params) ->
      let params = List.map params ~f:(fun x -> (x, [])) in
      fmt_class_params c ctx params $ fmt_longident_loc c name $ fmt_atrs
  | Pcl_structure {pcstr_fields; pcstr_self} ->
      hvbox 0
        (Params.parens_if parens c.conf
           ( fmt_class_structure c ~ctx ?ext:None pcstr_self pcstr_fields
           $ fmt_atrs ) )
  | Pcl_fun (xargs, body) ->
      let indent =
        match ctx0 with
        | Cl {pcl_desc= Pcl_fun _; _} -> 0
        | Cl _ -> 3
        | _ -> 0
      in
      hvbox indent
        (Params.parens_if parens c.conf
           ( hovbox 2
               ( box_fun_decl_args c 0
                   ( str "fun "
                   $ fmt_attributes c pcl_attributes ~suf:" "
                   $ wrap_fun_decl_args c (fmt_class_fun_args c xargs)
                   $ space_break )
               $ str "->" )
           $ space_break
           $ fmt_class_expr c (sub_cl ~ctx body) ) )
  | Pcl_apply (e0, e1N1) ->
      Params.parens_if parens c.conf
        (hvbox 2 (fmt_args_grouped e0 e1N1) $ fmt_atrs)
  | Pcl_let (lbs, body, loc_in) ->
      let indent_after_in =
        match body.pcl_desc with
        | Pcl_let _ -> 0
        | _ -> c.conf.fmt_opts.indent_after_in.v
      in
      let bindings =
        Sugar.Let_binding.of_let_bindings ~ctx lbs.pvbs_bindings
      in
      let fmt_expr = fmt_class_expr c (sub_cl ~ctx body) in
      let has_attr = not (List.is_empty pcl_attributes) in
      fmt_let c ~ctx0:ctx ~rec_flag:lbs.pvbs_rec ~bindings ~parens ~loc_in
        ~has_attr ~fmt_atrs ~fmt_expr ~body_loc:body.pcl_loc ~indent_after_in
  | Pcl_constraint (e, t) ->
      hvbox 2
        (wrap_fits_breaks ~space:false c.conf "(" ")"
           ( fmt_class_expr c (sub_cl ~ctx e)
           $ str " :" $ space_break
           $ fmt_class_type c (sub_cty ~ctx t) ) )
      $ fmt_atrs
  | Pcl_extension ext -> fmt_extension c ctx ext $ fmt_atrs
  | Pcl_open (popen, cl) ->
      hvbox 0
        ( fmt_open_description c ~keyword:"let open"
            ~kw_attributes:pcl_attributes popen
        $ str " in" $ force_break
        $ fmt_class_expr c (sub_cl ~ctx cl) )

and fmt_type_constraint c ctx = function
  | Pconstraint t1 ->
      space_break $ str ": " $ fmt_core_type c (sub_typ ~ctx t1)
  | Pcoerce (t1, t2) ->
      opt t1 (fun t ->
          space_break $ str ": " $ fmt_core_type c (sub_typ ~ctx t) )
      $ str " :> "
      $ fmt_core_type c (sub_typ ~ctx t2)

and fmt_class_field_virtual c ctx typ =
  (space_break $ str ": " $ fmt_core_type c (sub_typ ~ctx typ), noop, noop)

and fmt_class_field_value_kind c ctx = function
  | Cfk_virtual typ -> fmt_class_field_virtual c ctx typ
  | Cfk_concrete (_, tc, e) ->
      ( opt tc (fmt_type_constraint c ctx)
      , break 1 2 $ str "="
      , space_break $ fmt_expression c (sub_exp ~ctx e) )

and fmt_class_field_method_kind c ctx = function
  | Cfk_virtual typ -> fmt_class_field_virtual c ctx typ
  | Cfk_concrete (_, (args, t), e) ->
      let fmt_newtypes, fmt_cstr = fmt_value_constraint c t in
      ( fmt_if (not (List.is_empty args)) space_break
        $ wrap_fun_decl_args c (fmt_expr_fun_args c args)
        $ fmt_newtypes $ fmt_cstr
      , break 1 2 $ str "="
      , space_break $ fmt_expression c (sub_exp ~ctx e) )

and fmt_class_field c {ast= cf; _} =
  protect c (Clf cf)
  @@
  let fmt_cmts_before = Cmts.Toplevel.fmt_before c cf.pcf_loc in
  let fmt_cmts_after = Cmts.Toplevel.fmt_after c cf.pcf_loc in
  let doc_before, doc_after, atrs =
    fmt_docstring_around_item ~fit:true c cf.pcf_attributes
  in
  let fmt_atrs = fmt_item_attributes c ~pre:(Break (1, 0)) atrs in
  let ctx = Clf cf in
  (fun k ->
    fmt_cmts_before
    $ hvbox 0 ~name:"clf"
        (hvbox 0 (doc_before $ k $ fmt_atrs $ doc_after) $ fmt_cmts_after) )
  @@
  match cf.pcf_desc with
  | Pcf_inherit (override, cl, parent) ->
      hovbox 2
        ( str "inherit"
        $ fmt_if (is_override override) (str "!")
        $ space_break
        $ ( fmt_class_expr c (sub_cl ~ctx cl)
          $ opt parent (fun p -> str " as " $ fmt_str_loc c p) ) )
  | Pcf_method (name, pv, kind) ->
      let typ, eq, expr = fmt_class_field_method_kind c ctx kind in
      hvbox 2
        ( hovbox 2
            ( hovbox 4
                (box_fun_decl_args c
                   (Params.Indent.fun_args c.conf)
                   (box_fun_sig_args c
                      (Params.Indent.fun_type_annot c.conf)
                      ( str "method" $ virtual_or_override kind
                      $ fmt_private_virtual_flag c pv
                      $ str " " $ fmt_str_loc c name $ typ ) ) )
            $ eq )
        $ expr )
  | Pcf_val (name, mv, kind) ->
      let typ, eq, expr = fmt_class_field_value_kind c ctx kind in
      hvbox 2
        ( hovbox 2
            ( hovbox 4
                (box_fun_decl_args c 4
                   (box_fun_sig_args c 4
                      ( str "val" $ virtual_or_override kind
                      $ fmt_mutable_virtual_flag c mv
                      $ str " " $ fmt_str_loc c name $ typ ) ) )
            $ eq )
        $ expr )
  | Pcf_constraint (t1, t2) ->
      str "constraint" $ space_break
      $ fmt_core_type c (sub_typ ~ctx t1)
      $ str " = "
      $ fmt_core_type c (sub_typ ~ctx t2)
  | Pcf_initializer e ->
      str "initializer" $ break 1 2 $ fmt_expression c (sub_exp ~ctx e)
  | Pcf_attribute attr -> fmt_floating_attributes_and_docstrings c [attr]
  | Pcf_extension ext -> fmt_item_extension c ctx ext

and fmt_class_type_field c {ast= cf; _} =
  protect c (Ctf cf)
  @@
  let fmt_cmts_before = Cmts.Toplevel.fmt_before c cf.pctf_loc in
  let fmt_cmts_after = Cmts.Toplevel.fmt_after c cf.pctf_loc in
  let doc_before, doc_after, atrs =
    fmt_docstring_around_item ~is_val:true ~fit:true c cf.pctf_attributes
  in
  let fmt_atrs = fmt_item_attributes c ~pre:(Break (1, 0)) atrs in
  let ctx = Ctf cf in
  (fun k ->
    fmt_cmts_before
    $ hvbox 0 ~name:"ctf"
        ( hvbox 0 (doc_before $ hvbox 0 k $ fmt_atrs $ doc_after)
        $ fmt_cmts_after ) )
  @@
  match cf.pctf_desc with
  | Pctf_inherit ct ->
      hovbox 2
        (str "inherit" $ space_break $ fmt_class_type c (sub_cty ~ctx ct))
  | Pctf_method (name, pv, ty) ->
      box_fun_sig_args c 2
        ( hovbox 4
            ( str "method"
            $ fmt_private_virtual_flag c pv
            $ space_break $ fmt_str_loc c name )
        $ str " :" $ space_break
        $ fmt_core_type c (sub_typ ~ctx ty) )
  | Pctf_val (name, mv, ty) ->
      box_fun_sig_args c 2
        ( hovbox 4
            ( str "val"
            $ fmt_mutable_virtual_flag c mv
            $ space_break $ fmt_str_loc c name )
        $ str " :" $ space_break
        $ fmt_core_type c (sub_typ ~ctx ty) )
  | Pctf_constraint (t1, t2) ->
      str "constraint" $ space_break
      $ fmt_core_type c (sub_typ ~ctx t1)
      $ str " = "
      $ fmt_core_type c (sub_typ ~ctx t2)
  | Pctf_attribute attr -> fmt_floating_attributes_and_docstrings c [attr]
  | Pctf_extension ext -> fmt_item_extension c ctx ext

and fmt_cases c ctx cs = list_fl cs (fmt_case c ctx)

and fmt_case c ctx ~first ~last case =
  let {pc_lhs; pc_guard; pc_rhs} = case in
  let xrhs = sub_exp ~ctx pc_rhs in
  (* side effects of Cmts.fmt_before before [fmt_lhs] is important *)
  let leading_cmt = Cmts.fmt_before c pc_lhs.ppat_loc in
  let xlhs = sub_pat ~ctx pc_lhs in
  let paren_lhs =
    match pc_lhs.ppat_desc with
    | Ppat_or _ when Option.is_some pc_guard -> true
    | _ -> parenze_pat xlhs
  in
  let cmts_before = Cmts.has_before c.cmts pc_rhs.pexp_loc in
  let p =
    Params.get_cases c.conf ~ctx ~first ~last ~cmts_before ~xbch:xrhs
  in
  p.leading_space $ leading_cmt
  $ p.box_all
      ( p.box_pattern_arrow
          ( hvbox 0
              ( fmt_pattern c ~pro:p.bar ~parens:paren_lhs xlhs
              $ opt pc_guard (fun g ->
                    break 1 2 $ str "when "
                    $ fmt_expression c (sub_exp ~ctx g) ) )
          $ p.break_before_arrow $ str "->" $ p.break_after_arrow
          $ p.open_paren_branch )
      $ p.break_after_opening_paren
      $ hovbox 0
          ( fmt_expression ?eol:p.expr_eol c ?parens:p.expr_parens
              p.branch_expr
          $ p.close_paren_branch ) )

and fmt_value_description c ctx vd =
  let {pval_name= {txt; loc}; pval_type; pval_prim; pval_attributes; pval_loc}
      =
    vd
  in
  update_config_maybe_disabled_attrs c pval_loc pval_attributes
  @@ fun c ->
  let pre = if List.is_empty pval_prim then "val" else "external" in
  let doc_before, doc_after, attrs_before, attrs_after =
    fmt_docstring_around_item_attrs ~is_val:true c pval_attributes
  in
  let ext = pval_attributes.attrs_extension in
  let fmt_val_prim {txt= s; loc} =
    hvbox 0 @@ Cmts.fmt c loc
    @@
    if String.exists s ~f:(function ' ' | '\n' -> true | _ -> false) then
      wrap (str "{|") (str "|}") (str s)
    else wrap (str "\"") (str "\"") (str (String.escaped s))
  in
  hvbox 0
    ( doc_before
    $ box_fun_sig_args c 2
        ( str pre
        $ fmt_extension_suffix c ext
        $ fmt_attributes c ~pre:(Break (1, 0)) attrs_before
        $ str " "
        $ Cmts.fmt c loc
            (wrap_if
               (Std_longident.String_id.is_symbol txt)
               (str "( ") (str " )") (str txt) )
        $ fmt_core_type c ~pro:":"
            ~box:
              (not
                 ( c.conf.fmt_opts.ocp_indent_compat.v
                 && is_arrow_or_poly pval_type ) )
            (sub_typ ~ctx pval_type)
        $ fmt_if (not (List.is_empty pval_prim)) (space_break $ str "= ")
        $ hvbox_if (List.length pval_prim > 1) 0
          @@ list pval_prim space_break fmt_val_prim )
    $ fmt_item_attributes c ~pre:(Break (1, 0)) attrs_after
    $ doc_after )

and fmt_tydcl_params c ctx params =
  fmt_if
    (not (List.is_empty params))
    ( wrap_fits_breaks_if ~space:false c.conf
        (List.length params > 1)
        "(" ")"
        (list params (Params.comma_sep c.conf) (fun (ty, vc) ->
             fmt_variance_injectivity c vc
             $ fmt_core_type c (sub_typ ~ctx ty) ) )
    $ space_break )

and fmt_class_params c ctx params =
  let fmt_param ~first ~last (ty, vc) =
    fmt_if (first && Exposed.Left.core_type ty) (str " ")
    $ fmt_if (not first) (Params.comma_sep c.conf)
    $ fmt_variance_injectivity c vc
    $ fmt_core_type c (sub_typ ~ctx ty)
    $ fmt_if (last && Exposed.Right.core_type ty) (str " ")
  in
  fmt_if
    (not (List.is_empty params))
    (hvbox 0
       ( wrap_fits_breaks c.conf "[" "]" (list_fl params fmt_param)
       $ space_break ) )

and fmt_type_declaration c ?(pre = "") ?name ?(eq = "=") {ast= decl; _} =
  protect c (Td decl)
  @@
  let { ptype_name= {txt; loc}
      ; ptype_params
      ; ptype_cstrs
      ; ptype_kind
      ; ptype_private= priv
      ; ptype_manifest= m
      ; ptype_attributes
      ; ptype_loc } =
    decl
  in
  update_config_maybe_disabled_attrs c ptype_loc ptype_attributes
  @@ fun c ->
  let ctx = Td decl in
  let fmt_abstract_manifest = function
    | Some m ->
        str " " $ str eq $ fmt_private_flag c priv $ space_break
        $ fmt_core_type c (sub_typ ~ctx m)
    | None -> noop
  in
  let fmt_manifest = function
    | Some m ->
        str " " $ str eq $ break 1 4
        $ fmt_core_type c (sub_typ ~ctx m)
        $ str " =" $ fmt_private_flag c priv
    | None -> str " " $ str eq $ fmt_private_flag c priv
  in
  let ext = ptype_attributes.attrs_extension in
  (* Docstring cannot be placed after variant declarations *)
  let force_before =
    match ptype_kind with Ptype_variant _ -> true | _ -> false
  in
  let doc_before, doc_after, attrs_before, attrs_after =
    let fit = Tyd.is_simple decl in
    fmt_docstring_around_item_attrs ~force_before ~fit c ptype_attributes
  in
  let box_manifest k =
    hvbox c.conf.fmt_opts.type_decl_indent.v
      ( str pre
      $ fmt_extension_suffix c ext
      $ fmt_attributes c attrs_before
      $ str " "
      $ hvbox_if
          (not (List.is_empty ptype_params))
          0
          ( fmt_tydcl_params c ctx ptype_params
          $ Option.value_map name ~default:(str txt) ~f:(fmt_longident_loc c)
          )
      $ k )
  in
  let fmt_manifest_kind =
    match ptype_kind with
    | Ptype_abstract -> box_manifest (fmt_abstract_manifest m)
    | Ptype_variant [] ->
        box_manifest (fmt_manifest m) $ space_break $ str "|"
    | Ptype_variant ctor_decls ->
        box_manifest (fmt_manifest m)
        $ space_break
        $ list_fl ctor_decls (fmt_constructor_declaration c ctx)
    | Ptype_record lbl_decls ->
        let p = Params.get_record_type c.conf in
        let fmt_decl ~first ~last x =
          fmt_if (not first) p.sep_before
          $ fmt_label_declaration c ctx x ~last
          $ fmt_if
              ( last && (not p.box_spaced)
              && Exposed.Right.label_declaration x )
              (str " ")
          $ fmt_if (not last) p.sep_after
        in
        box_manifest (fmt_manifest m $ p.docked_before)
        $ p.break_before
        $ p.box_record (list_fl lbl_decls fmt_decl)
        $ p.break_after $ p.docked_after
    | Ptype_open -> box_manifest (fmt_manifest m $ str " ..")
  in
  let fmt_cstr (t1, t2, loc) =
    Cmts.fmt c loc
      (hvbox 2
         ( str "constraint" $ space_break
         $ fmt_core_type c (sub_typ ~ctx t1)
         $ str " =" $ space_break
         $ fmt_core_type c (sub_typ ~ctx t2) ) )
  in
  let fmt_cstrs cstrs =
    fmt_if
      (not (List.is_empty cstrs))
      (space_break $ hvbox 0 (list cstrs space_break fmt_cstr))
  in
  Cmts.fmt c loc @@ Cmts.fmt c ptype_loc
  @@ hvbox 0
       ( doc_before
       $ hvbox 0
           ( hvbox c.conf.fmt_opts.type_decl_indent.v
               (fmt_manifest_kind $ fmt_cstrs ptype_cstrs)
           $ fmt_item_attributes c ~pre:(Break (1, 0)) attrs_after )
       $ doc_after )

and fmt_label_declaration c ctx ?(last = false) decl =
  let {pld_mutable; pld_name; pld_type; pld_loc; pld_attributes} = decl in
  update_config_maybe_disabled c pld_loc pld_attributes
  @@ fun c ->
  let doc, atrs = doc_atrs pld_attributes in
  let cmt_after_type = Cmts.fmt_after c pld_type.ptyp_loc in
  let field_loose =
    match c.conf.fmt_opts.field_space.v with
    | `Loose -> true
    | `Tight_decl | `Tight -> false
  in
  let fmt_semicolon =
    match c.conf.fmt_opts.break_separators.v with
    | `Before -> noop
    | `After ->
        fmt_or last
          (fmt_if c.conf.fmt_opts.dock_collection_brackets.v
             (fits_breaks ~level:5 "" ";") )
          (str ";")
  in
  let indent_cmts = Params.Indent.record_docstring c.conf in
  hovbox 0
    ( Cmts.fmt_before c pld_loc
    $ hvbox indent_cmts
        ( hvbox 3
            ( hvbox indent_cmts
                ( hvbox 2
                    ( hovbox 2
                        ( fmt_mutable_flag ~pro:noop ~epi:space_break c
                            pld_mutable
                        $ fmt_str_loc c pld_name
                        $ fmt_if field_loose (str " ")
                        $ str ":" )
                    $ space_break
                    $ fmt_core_type c (sub_typ ~ctx pld_type)
                    $ fmt_semicolon )
                $ cmt_after_type )
            $ fmt_attributes c ~pre:(Break (1, 1)) atrs )
        $ fmt_docstring_padded c doc
        $ Cmts.fmt_after c pld_loc ) )

and fmt_constructor_declaration c ctx ~first ~last:_ cstr_decl =
  let { pcd_name= {txt; loc}
      ; pcd_vars
      ; pcd_args
      ; pcd_res
      ; pcd_attributes
      ; pcd_loc } =
    cstr_decl
  in
  update_config_maybe_disabled c pcd_loc pcd_attributes
  @@ fun c ->
  let has_cmt_before = Cmts.has_before c.cmts pcd_loc in
  let sparse = Poly.( = ) c.conf.fmt_opts.type_decl.v `Sparse in
  (* Force break if comment before pcd_loc, it would interfere with an
     eventual comment placed after the previous constructor *)
  fmt_if (not first)
    (fmt_or (sparse || has_cmt_before) force_break space_break)
  $ Cmts.fmt_before ~epi:force_break c pcd_loc
  $ hvbox ~name:"constructor_decl" 2
      ( hovbox
          (Params.Indent.constructor_docstring c.conf)
          ( hvbox 2
              ( fmt_or first (if_newline "| ") (str "| ")
              $ Cmts.fmt_before c loc
              $ hvbox 2
                  ( hovbox ~name:"constructor_decl_name" 2
                      ( wrap_if
                          (Std_longident.String_id.is_symbol txt)
                          (str "( ") (str " )") (str txt)
                      $ Cmts.fmt_after c loc )
                  $ fmt_constructor_arguments_result c ctx pcd_vars pcd_args
                      pcd_res ) )
          $ fmt_attributes_and_docstrings c pcd_attributes )
      $ Cmts.fmt_after c pcd_loc )

and fmt_constructor_arguments ?vars c ctx ~pre = function
  | Pcstr_tuple typs ->
      let vars =
        match vars with Some vars -> space_break $ vars | None -> noop
      and typs =
        match typs with
        | [] -> noop
        | _ :: _ ->
            space_break
            $ hvbox 0
                (list typs
                   (space_break $ str "* ")
                   (sub_typ ~ctx >> fmt_core_type c) )
      in
      pre $ vars $ typs
  | Pcstr_record (loc, lds) ->
      let p = Params.get_record_type c.conf in
      let fmt_ld ~first ~last x =
        fmt_if (not first) p.sep_before
        $ fmt_label_declaration c ctx x ~last
        $ fmt_if
            (last && (not p.box_spaced) && Exposed.Right.label_declaration x)
            (str " ")
        $ fmt_if (not last) p.sep_after
      in
      pre
      $ Cmts.fmt c loc ~pro:(break 1 0) ~epi:noop
        @@ wrap p.docked_before p.docked_after
        @@ wrap p.break_before p.break_after
        @@ p.box_record @@ list_fl lds fmt_ld

and fmt_constructor_arguments_result c ctx vars args res =
  let before_type, pre =
    match (args, res) with
    | Pcstr_tuple [], Some _ -> (noop, str " :")
    | Pcstr_tuple [], None -> (noop, noop)
    | _ -> (str "-> ", fmt_or (Option.is_none res) (str " of") (str " :"))
  in
  let fmt_type typ =
    space_break $ before_type $ fmt_core_type c (sub_typ ~ctx typ)
  in
  let fmt_vars =
    match vars with
    | [] -> None
    | _ ->
        Some
          ( hvbox 0 (list vars space_break (fun {txt; _} -> fmt_type_var txt))
          $ str "." )
  in
  fmt_constructor_arguments c ctx ~pre ?vars:fmt_vars args $ opt res fmt_type

and fmt_type_extension c ctx
    { ptyext_attributes
    ; ptyext_params
    ; ptyext_path
    ; ptyext_constructors
    ; ptyext_private
    ; ptyext_loc } =
  let c = update_config_attrs c ptyext_attributes in
  let ext = ptyext_attributes.attrs_extension in
  let doc, _doc_after, attrs_before, attrs_after =
    fmt_docstring_around_item_attrs ~force_before:true c ptyext_attributes
  in
  let fmt_ctor ctor = hvbox 0 (fmt_extension_constructor c ctx ctor) in
  Cmts.fmt c ptyext_loc
  @@ hvbox 2
       ( doc
       $ hvbox c.conf.fmt_opts.type_decl_indent.v
           ( str "type"
           $ fmt_extension_suffix c ext
           $ fmt_attributes c ~pre:(Break (1, 0)) attrs_before
           $ str " "
           $ hvbox_if
               (not (List.is_empty ptyext_params))
               0
               (fmt_tydcl_params c ctx ptyext_params)
           $ fmt_longident_loc c ptyext_path
           $ str " +="
           $ fmt_private_flag c ptyext_private
           $ list_fl ptyext_constructors (fun ~first ~last:_ x ->
                 let bar_fits = if first then "" else "| " in
                 cbreak ~fits:("", 1, bar_fits) ~breaks:("", 0, "| ")
                 $ fmt_ctor x ) )
       $ fmt_item_attributes c ~pre:(Break (1, 0)) attrs_after )

and fmt_type_exception ~pre c ctx
    {ptyexn_attributes= item_attrs; ptyexn_constructor; ptyexn_loc} =
  let {pext_attributes= cons_attrs; _} = ptyexn_constructor in
  (* Here, the order is very important. We need the attrs_after to be at the
     end of the list. *)
  (* On 4.08 the doc is attached to the constructor *)
  let docs, cons_attrs = extract_doc_attrs [] cons_attrs in
  let docs, attrs_after = extract_doc_attrs docs item_attrs.attrs_after in
  let docs, attrs_before = extract_doc_attrs docs item_attrs.attrs_before in
  let doc1, doc2 =
    match docs with
    | [] -> (None, None)
    | [elt] -> (Some elt, None)
    | [doc1; doc2] -> (Some doc1, Some doc2)
    | _ -> assert false
  in
  let doc_before, doc_after = fmt_docstring_around_item' c doc1 doc2 in
  let ptyexn_constructor =
    {ptyexn_constructor with pext_attributes= cons_attrs}
  in
  let ext = item_attrs.attrs_extension in
  Cmts.fmt c ptyexn_loc
    (hvbox 0
       ( doc_before
       $ hvbox 2
           ( pre
           $ fmt_extension_suffix c ext
           $ fmt_attributes c ~pre:(Break (1, 0)) attrs_before
           $ space_break
           $ fmt_extension_constructor c ctx ptyexn_constructor )
       $ fmt_item_attributes c ~pre:(Break (1, 0)) attrs_after
       $ doc_after ) )

and fmt_extension_constructor c ctx ec =
  let {pext_name; pext_kind; pext_attributes; pext_loc} = ec in
  update_config_maybe_disabled c pext_loc pext_attributes
  @@ fun c ->
  let sep =
    match pext_kind with
    | Pext_decl (_, _, Some _) -> str " :" $ space_break
    | Pext_decl (_, _, None) | Pext_rebind _ -> str " of" $ space_break
  in
  Cmts.fmt c pext_loc
  @@ hvbox 4
       ( hvbox 2
           ( fmt_str_loc c pext_name
           $
           match pext_kind with
           | Pext_decl (_, (Pcstr_tuple [] | Pcstr_record (_, [])), None) ->
               noop
           | Pext_decl (_, (Pcstr_tuple [] | Pcstr_record (_, [])), Some res)
             ->
               sep $ fmt_core_type c (sub_typ ~ctx res)
           | Pext_decl (vars, args, res) ->
               fmt_constructor_arguments_result c ctx vars args res
           | Pext_rebind lid -> str " = " $ fmt_longident_loc c lid )
       $ fmt_attributes_and_docstrings c pext_attributes )

and fmt_functor_param c ctx {loc; txt= arg} =
  match arg with
  | Unit -> Cmts.fmt c loc (wrap (str "(") (str ")") (Cmts.fmt_within c loc))
  | Named (name, mt) ->
      let xmt = sub_mty ~ctx mt in
      hvbox 0
        (Cmts.fmt c loc
           (wrap (str "(") (str ")")
              (hovbox 0
                 ( hovbox 0 (fmt_str_loc_opt c name $ space_break $ str ": ")
                 $ compose_module (fmt_module_type c xmt) ~f:Fn.id ) ) ) )

and fmt_module_type c ?(rec_ = false) ({ast= mty; _} as xmty) =
  let ctx = Mty mty in
  let {pmty_desc; pmty_loc; pmty_attributes} = mty in
  update_config_maybe_disabled_block c pmty_loc pmty_attributes
  @@ fun c ->
  let parens =
    parenze_mty xmty
    || match pmty_desc with Pmty_with _ when rec_ -> true | _ -> false
  in
  match pmty_desc with
  | Pmty_ident lid ->
      { empty with
        bdy= fmt_longident_loc c lid
      ; epi= Some (fmt_attributes c pmty_attributes ~pre:(Break (1, 0))) }
  | Pmty_signature s ->
      let empty = List.is_empty s && not (Cmts.has_within c.cmts pmty_loc) in
      let before = Cmts.fmt_before c pmty_loc in
      let within = Cmts.fmt_within c ~pro:noop pmty_loc in
      let after = Cmts.fmt_after c pmty_loc in
      { opn= None
      ; pro= Some (before $ str "sig" $ fmt_if empty (str " "))
      ; psp= fmt_if (not empty) (break 1000 2)
      ; bdy= (within $ if empty then noop else fmt_signature c ctx s)
      ; cls= noop
      ; esp= fmt_if (not empty) force_break
      ; epi=
          Some
            ( str "end" $ after
            $ fmt_attributes_and_docstrings c pmty_attributes ) }
  | Pmty_functor (args, mt, short) ->
      let keyword =
        if short && List.is_empty pmty_attributes then noop
        else
          str "functor"
          $ fmt_attributes c ~pre:Blank pmty_attributes
          $ break 1 2
      in
      let blk = fmt_module_type c (sub_mty ~ctx mt) in
      { blk with
        pro=
          Some
            ( Cmts.fmt_before c pmty_loc
            $ keyword
            $ list args (break 1 2) (fmt_functor_param c ctx)
            $ break 1 2 $ str "->"
            $ opt blk.pro (fun pro -> str " " $ pro) )
      ; epi= Some (fmt_opt blk.epi $ Cmts.fmt_after c pmty_loc)
      ; psp=
          fmt_or (Option.is_none blk.pro)
            (fits_breaks " " ~hint:(1, 2) "")
            blk.psp }
  | Pmty_with _ ->
      let wcs, mt = Sugar.mod_with (sub_mty ~ctx mty) in
      let fmt_cstr ~first ~last:_ wc =
        let pre = if first then "with" else " and" in
        fmt_or first space_break cut_break
        $ fmt_with_constraint c ctx ~pre wc
      in
      let fmt_cstrs ~first:_ ~last:_ (wcs_and, loc, attr) =
        Cmts.fmt c loc
          ( list_fl wcs_and fmt_cstr
          $ fmt_attributes c ~pre:(Break (1, -1)) attr )
      in
      let {pro; psp; bdy; esp; epi; opn= _; cls= _} = fmt_module_type c mt in
      { empty with
        pro=
          Option.map pro ~f:(fun pro ->
              open_hvbox 0 $ fmt_if parens (str "(") $ pro )
      ; psp
      ; bdy=
          fmt_if (Option.is_none pro)
            ( open_hvbox (Params.Indent.mty_with c.conf)
            $ fmt_if parens (str "(") )
          $ hvbox 0 bdy
          $ fmt_if (Option.is_some epi) esp
          $ fmt_opt epi $ list_fl wcs fmt_cstrs
          $ fmt_if parens (str ")")
          $ close_box
      ; esp= fmt_if (Option.is_none epi) esp
      ; epi= Some (Cmts.fmt_after c pmty_loc) }
  | Pmty_typeof me -> (
      let blk = fmt_module_expr c (sub_mod ~ctx me) in
      let epi =
        fmt_opt blk.epi $ Cmts.fmt_after c pmty_loc
        $ fmt_if parens (str ")")
        $ fmt_attributes c pmty_attributes ~pre:(Break (1, 0))
      in
      match blk.pro with
      | Some pro ->
          { blk with
            pro=
              Some
                ( Cmts.fmt_before c pmty_loc
                $ fmt_if parens (str "(")
                $ str "module type of " $ pro )
          ; epi= Some epi }
      | _ ->
          { blk with
            bdy=
              Cmts.fmt c pmty_loc
              @@ hvbox 2
                   ( fmt_if parens (str "(")
                   $ str "module type of" $ space_break $ blk.bdy )
          ; epi= Some epi } )
  | Pmty_extension ext ->
      { empty with
        bdy= fmt_extension c ctx ext
      ; epi= Some (fmt_attributes c pmty_attributes ~pre:(Break (1, 0))) }
  | Pmty_alias lid ->
      { empty with
        bdy= fmt_longident_loc c lid
      ; epi= Some (fmt_attributes c pmty_attributes ~pre:(Break (1, 0))) }

and fmt_signature c ctx itms =
  let update_config c i =
    match i.psig_desc with
    | Psig_attribute atr -> update_config c [atr]
    | _ -> c
  in
  let fmt_item c ctx ~prev:_ ~next:_ i =
    fmt_signature_item c (sub_sig ~ctx i)
  in
  let ast x = Sig x in
  fmt_item_list c ctx update_config ast fmt_item itms

and fmt_signature_item c {ast= si; _} =
  protect c (Sig si)
  @@
  let fmt_cmts_before = Cmts.Toplevel.fmt_before c si.psig_loc in
  let fmt_cmts_after = Cmts.Toplevel.fmt_after c si.psig_loc in
  (fun k -> fmt_cmts_before $ hvbox 0 (k $ fmt_cmts_after))
  @@
  let ctx = Sig si in
  match si.psig_desc with
  | Psig_attribute attr -> fmt_floating_attributes_and_docstrings c [attr]
  | Psig_exception exc ->
      let pre = str "exception" in
      hvbox 2 (fmt_type_exception ~pre c ctx exc)
  | Psig_extension (ext, atrs) ->
      let doc_before, doc_after, atrs = fmt_docstring_around_item c atrs in
      let box =
        match snd ext with
        | PTyp _ | PPat _ | PStr [_] | PSig [_] -> true
        | PStr _ | PSig _ -> false
      in
      hvbox_if box c.conf.fmt_opts.stritem_extension_indent.v
        ( doc_before
        $ hvbox_if (not box) 0 (fmt_item_extension c ctx ext)
        $ fmt_item_attributes c ~pre:(Break (1, 0)) atrs
        $ doc_after )
  | Psig_include {pincl_mod; pincl_attributes; pincl_loc} ->
      update_config_maybe_disabled_attrs c pincl_loc pincl_attributes
      @@ fun c ->
      let ext = pincl_attributes.attrs_extension in
      let doc_before, doc_after, attrs_before, attrs_after =
        let force_before = not (Mty.is_simple pincl_mod) in
        fmt_docstring_around_item_attrs c ~force_before ~fit:true
          pincl_attributes
      in
      let keyword, ({pro; psp; bdy; esp; epi; _} as blk) =
        let kwd = str "include" $ fmt_extension_suffix c ext in
        match pincl_mod with
        | {pmty_desc= Pmty_typeof me; pmty_loc; pmty_attributes= _} ->
            ( kwd
              $ Cmts.fmt c ~pro:(str " ") ~epi:noop pmty_loc
                  (space_break $ str "module type of")
            , fmt_module_expr c (sub_mod ~ctx me) )
        | _ -> (kwd, fmt_module_type c (sub_mty ~ctx pincl_mod))
      in
      let box = blk_box blk in
      hvbox 0
        ( doc_before
        $ hvbox 0
            ( box
                ( hvbox 2
                    ( keyword
                    $ fmt_attributes c ~pre:(Break (1, 0)) attrs_before
                    $ opt pro (fun pro -> str " " $ pro) )
                $ fmt_or (Option.is_some pro) psp (break 1 2)
                $ bdy )
            $ esp $ fmt_opt epi
            $ fmt_item_attributes c ~pre:(Break (1, 0)) attrs_after )
        $ doc_after )
  | Psig_modtype mtd -> fmt_module_type_declaration c ctx mtd
  | Psig_modtypesubst mtd -> fmt_module_type_declaration ~eqty:":=" c ctx mtd
  | Psig_module md ->
      hvbox 0
        (fmt_module_declaration c ~rec_flag:false ~first:true
           (sub_md ~ctx md) )
  | Psig_modsubst ms -> hvbox 0 (fmt_module_substitution c ctx ms)
  | Psig_open od -> fmt_open_description c ~kw_attributes:[] od
  | Psig_recmodule mds ->
      fmt_recmodule c ctx mds fmt_module_declaration (fun x -> Md x) sub_md
  | Psig_type (rec_flag, decls) -> fmt_type c rec_flag decls ctx
  | Psig_typext te -> fmt_type_extension c ctx te
  | Psig_value vd -> fmt_value_description c ctx vd
  | Psig_class cl -> fmt_class_types c ~pre:"class" ~sep:":" cl
  | Psig_class_type cl -> fmt_class_types c ~pre:"class type" ~sep:"=" cl
  | Psig_typesubst decls -> fmt_type c ~eq:":=" Recursive decls ctx

and fmt_class_types c ~pre ~sep cls =
  list_fl cls (fun ~first ~last:_ cl ->
      update_config_maybe_disabled_attrs c cl.pci_loc cl.pci_attributes
      @@ fun c ->
      let ctx = Ctd cl in
      let doc_before, doc_after, attrs_before, attrs_after =
        let force_before = not (Cty.is_simple cl.pci_expr) in
        fmt_docstring_around_item_attrs ~force_before c cl.pci_attributes
      in
      let ext = cl.pci_attributes.attrs_extension in
      let class_types =
        let pro =
          hovbox 2
            ( str (if first then pre else "and")
            $ fmt_if first (fmt_extension_suffix c ext)
            $ fmt_attributes c ~pre:(Break (1, 0)) attrs_before
            $ fmt_virtual_flag c cl.pci_virt
            $ space_break
            $ fmt_class_params c ctx cl.pci_params
            $ fmt_str_loc c cl.pci_name $ str " " $ str sep )
          $ space_break
        in
        hovbox 2
          ( fmt_class_type c ~pro (sub_cty ~ctx cl.pci_expr)
          $ fmt_item_attributes c ~pre:(Break (1, 0)) attrs_after )
      in
      fmt_if (not first) (str "\n" $ force_break)
      $ hovbox 0
        @@ Cmts.fmt c cl.pci_loc (doc_before $ class_types $ doc_after) )

and fmt_class_exprs c cls =
  hvbox 0
  @@ list_fl cls (fun ~first ~last:_ cl ->
         update_config_maybe_disabled_attrs c cl.pci_loc cl.pci_attributes
         @@ fun c ->
         let ctx = Cd cl in
         let xargs = cl.pci_args in
         let ext = cl.pci_attributes.attrs_extension in
         let doc_before, doc_after, attrs_before, attrs_after =
           let force_before = not (Cl.is_simple cl.pci_expr) in
           fmt_docstring_around_item_attrs ~force_before c cl.pci_attributes
         in
         let class_expr =
           let pro =
             box_fun_decl_args c 2
               ( hovbox 2
                   ( str (if first then "class" else "and")
                   $ fmt_if first (fmt_extension_suffix c ext)
                   $ fmt_attributes c ~pre:(Break (1, 0)) attrs_before
                   $ fmt_virtual_flag c cl.pci_virt
                   $ space_break
                   $ fmt_class_params c ctx cl.pci_params
                   $ fmt_str_loc c cl.pci_name )
               $ fmt_if (not (List.is_empty xargs)) space_break
               $ wrap_fun_decl_args c (fmt_class_fun_args c xargs) )
           in
           let intro =
             match cl.pci_constraint with
             | Some ty ->
                 fmt_class_type c
                   ~pro:(pro $ str " :" $ space_break)
                   (sub_cty ~ctx ty)
             | None -> pro
           in
           hovbox 2
             ( hovbox 2 (intro $ space_break $ str "=")
             $ space_break
             $ fmt_class_expr c (sub_cl ~ctx cl.pci_expr) )
           $ fmt_item_attributes c ~pre:(Break (1, 0)) attrs_after
         in
         fmt_if (not first) (str "\n" $ force_break)
         $ hovbox 0
           @@ Cmts.fmt c cl.pci_loc (doc_before $ class_expr $ doc_after) )

and fmt_module c ctx ?rec_ ?epi ?(can_sparse = false) keyword ?(eqty = "=")
    name xargs xbody xmty ~attrs ~rec_flag =
  let ext = attrs.attrs_extension in
  let blk_t =
    Option.value_map xmty ~default:empty ~f:(fun xmty ->
        let blk = fmt_module_type ?rec_ c xmty in
        { blk with
          pro=
            Some (str " " $ str eqty $ opt blk.pro (fun pro -> str " " $ pro))
        ; psp= fmt_if (Option.is_none blk.pro) (break 1 2) $ blk.psp } )
  in
  let blk_b = Option.value_map xbody ~default:empty ~f:(fmt_module_expr c) in
  let args_p = Params.Mod.get_args c.conf xargs in
  let fmt_name_and_mt ~pro ~loc name mt =
    let xmt = sub_mty ~ctx mt in
    let blk = fmt_module_type c ?rec_ xmt in
    let align_opn, align_cls =
      if args_p.align then (open_hvbox 0, close_box) else (noop, noop)
    in
    let pro =
      hovbox 1
        ( pro
        $ Cmts.fmt_before c ~epi:space_break loc
        $ str "(" $ align_opn $ fmt_str_loc_opt c name $ str " :" )
      $ fmt_or (Option.is_some blk.pro) (str " ") (break 1 2)
    and epi = str ")" $ Cmts.fmt_after c loc $ align_cls in
    compose_module' ~box:false ~pro ~epi blk
  in
  (* Carry the [epi] to be placed in the next argument's box. *)
  let fmt_arg ~pro {loc; txt} =
    let pro = pro $ args_p.arg_psp in
    match txt with
    | Unit ->
        ( pro
          $ Cmts.fmt c loc (wrap (str "(") (str ")") (Cmts.fmt_within c loc))
        , noop )
    | Named (name, mt) ->
        if args_p.dock then
          (* All signatures, put the [epi] into the box of the next arg and
             don't break. *)
          fmt_name_and_mt ~pro ~loc name mt
        else
          let bdy, epi = fmt_name_and_mt ~pro:noop ~loc name mt in
          let bdy_indent = if args_p.align then 1 else 0 in
          (pro $ hvbox bdy_indent bdy $ epi, noop)
  in
  let rec fmt_args ~pro = function
    | [] -> pro
    | hd :: tl ->
        let bdy, epi = fmt_arg ~pro hd in
        bdy $ fmt_args ~pro:epi tl
  in
  let single_line =
    Option.for_all xbody ~f:(fun x -> Mod.is_simple x.ast)
    && Option.for_all xmty ~f:(fun x -> Mty.is_simple x.ast)
    && List.for_all xargs ~f:(function {txt= Unit; _} -> true | _ -> false)
  in
  let doc_before, doc_after, attrs_before, attrs_after =
    fmt_docstring_around_item_attrs c ~force_before:(not single_line)
      ~fit:true attrs
  in
  let intro =
    hvbox 2
      ( str keyword
      $ fmt_extension_suffix c ext
      $ fmt_attributes c ~pre:(Break (1, 0)) attrs_before
      $ fmt_if rec_flag (str " rec")
      $ space_break $ fmt_str_loc_opt c name )
  in
  let compact =
    Poly.(c.conf.fmt_opts.let_module.v = `Compact) || not can_sparse
  in
  let fmt_pro = opt blk_b.pro (fun pro -> space_break $ pro) in
  hvbox
    (if compact then 0 else 2)
    ( doc_before
    $ blk_box blk_b
        ( (if Option.is_some blk_t.epi then hovbox else hvbox)
            0
            ( blk_box blk_t
                ( hvbox args_p.indent
                    (fmt_args ~pro:intro xargs $ fmt_opt blk_t.pro)
                $ blk_t.psp $ blk_t.bdy )
            $ blk_t.esp $ fmt_opt blk_t.epi
            $ fmt_if (Option.is_some xbody) (str " =")
            $ fmt_if compact fmt_pro )
        $ fmt_if (not compact) fmt_pro
        $ blk_b.psp
        $ fmt_if
            (Option.is_none blk_b.pro && Option.is_some xbody)
            space_break
        $ blk_b.bdy )
    $ blk_b.esp $ fmt_opt blk_b.epi
    $ fmt_item_attributes c ~pre:(Break (1, 0)) attrs_after
    $ doc_after
    $ opt epi (fun epi ->
          fmt_or compact
            (fmt_or
               ( Option.is_some blk_b.epi
               && not c.conf.fmt_opts.ocp_indent_compat.v )
               (str " ") space_break )
            (break 1 (-2))
          $ epi ) )

and fmt_module_declaration c ~rec_flag ~first {ast= pmd; _} =
  protect c (Md pmd)
  @@
  let {pmd_name; pmd_args; pmd_type; pmd_ext_attrs= attrs; pmd_loc} = pmd in
  update_config_maybe_disabled_attrs c pmd_loc attrs
  @@ fun c ->
  let ctx = Md pmd in
  let keyword = if first then "module" else "and" in
  let xmty = sub_mty ~ctx pmd_type in
  let eqty =
    match xmty.ast.pmty_desc with Pmty_alias _ -> None | _ -> Some ":"
  in
  Cmts.fmt c pmd_loc
    (fmt_module ~rec_:rec_flag c ctx keyword pmd_name pmd_args None ?eqty
       (Some xmty) ~rec_flag:(rec_flag && first) ~attrs )

and fmt_module_substitution c ctx pms =
  let {pms_name; pms_manifest; pms_ext_attrs= attrs; pms_loc} = pms in
  update_config_maybe_disabled_attrs c pms_loc attrs
  @@ fun c ->
  let xmty =
    (* TODO: improve *)
    sub_mty ~ctx
      { pmty_desc= Pmty_ident pms_manifest
      ; pmty_loc= pms_loc
      ; pmty_attributes= [] }
  in
  let pms_name = {pms_name with txt= Some pms_name.txt} in
  Cmts.fmt c pms_loc
    (fmt_module c ctx "module" ~eqty:":=" pms_name [] None (Some xmty) ~attrs
       ~rec_flag:false )

and fmt_module_type_declaration ?eqty c ctx pmtd =
  let {pmtd_name; pmtd_type; pmtd_ext_attrs= attrs; pmtd_loc} = pmtd in
  update_config_maybe_disabled_attrs c pmtd_loc attrs
  @@ fun c ->
  let pmtd_name = {pmtd_name with txt= Some pmtd_name.txt} in
  fmt_module ?eqty c ctx "module type" pmtd_name [] None ~rec_flag:false
    (Option.map pmtd_type ~f:(sub_mty ~ctx))
    ~attrs

and fmt_open_description c ?(keyword = "open") ~kw_attributes
    {popen_expr= popen_lid; popen_override; popen_attributes; popen_loc} =
  update_config_maybe_disabled_attrs c popen_loc popen_attributes
  @@ fun c ->
  let ext = popen_attributes.attrs_extension in
  let doc_before, doc_after, attrs_before, attrs_after =
    fmt_docstring_around_item_attrs ~fit:true c popen_attributes
  in
  let keyword =
    fmt_or
      (is_override popen_override)
      ( str keyword $ str "!"
      $ opt ext (fun _ -> str " " $ fmt_extension_suffix c ext) )
      (str keyword $ fmt_extension_suffix c ext)
  in
  hovbox 0
    ( doc_before $ keyword
    $ Cmts.fmt c popen_loc
        ( fmt_attributes c kw_attributes
        $ fmt_attributes c ~pre:(Break (1, 0)) attrs_before
        $ str " "
        $ fmt_longident_loc c popen_lid
        $ fmt_item_attributes c ~pre:Blank attrs_after )
    $ doc_after )

(** TODO: merge with `fmt_module_declaration` *)
and fmt_module_statement c ~attributes ?keyword mod_expr =
  let blk = fmt_module_expr c mod_expr in
  let force_before = not (Mod.is_simple mod_expr.ast) in
  let doc_before, doc_after, attrs_before, attrs_after =
    fmt_docstring_around_item_attrs ~force_before ~fit:true c attributes
  in
  let has_kwd = Option.is_some keyword in
  let kwd_and_pro = Option.is_some blk.pro && has_kwd in
  doc_before
  $ blk_box blk
      (hvbox_if (Option.is_none blk.pro) 2
         ( hvbox_if kwd_and_pro 2
             ( fmt_opt keyword
             $ fmt_extension_suffix c attributes.attrs_extension
             $ fmt_attributes c ~pre:(Break (1, 0)) attrs_before
             $ space_break $ fmt_opt blk.pro )
         $ blk.psp $ blk.bdy ) )
  $ blk.esp $ fmt_opt blk.epi
  $ fmt_item_attributes c ~pre:Blank attrs_after
  $ doc_after

and fmt_with_constraint c ctx ~pre = function
  | Pwith_type (lid, td) ->
      fmt_type_declaration ~pre:(pre ^ " type") c ~name:lid (sub_td ~ctx td)
  | Pwith_module (m1, m2) ->
      str pre $ str " module " $ fmt_longident_loc c m1 $ str " = "
      $ fmt_longident_loc c m2
  | Pwith_typesubst (lid, td) ->
      fmt_type_declaration ~pre:(pre ^ " type") c ~eq:":=" ~name:lid
        (sub_td ~ctx td)
  | Pwith_modsubst (m1, m2) ->
      str pre $ str " module " $ fmt_longident_loc c m1 $ str " := "
      $ fmt_longident_loc c m2
  | Pwith_modtype (m1, m2) ->
      let m1 = {m1 with txt= Some (str_longident m1.txt)} in
      let m2 = Some (sub_mty ~ctx m2) in
      str pre $ break 1 2
      $ fmt_module c ctx "module type" m1 [] None ~rec_flag:false m2
          ~attrs:Ast_helper.Attr.empty_ext_attrs
  | Pwith_modtypesubst (m1, m2) ->
      let m1 = {m1 with txt= Some (str_longident m1.txt)} in
      let m2 = Some (sub_mty ~ctx m2) in
      str pre $ break 1 2
      $ fmt_module c ctx ~eqty:":=" "module type" m1 [] None ~rec_flag:false
          m2 ~attrs:Ast_helper.Attr.empty_ext_attrs

and fmt_mod_apply c ctx loc attrs ~parens ~dock_struct me_f arg =
  match me_f.pmod_desc with
  | Pmod_ident _ -> (
    match arg with
    | `Unit x ->
        { empty with
          bdy=
            Cmts.fmt c loc
              ( hvbox 2
                  ( compose_module
                      (fmt_module_expr c (sub_mod ~ctx me_f))
                      ~f:Fn.id
                  $ break 1 0 $ x )
              $ fmt_attributes_and_docstrings c attrs ) }
    | `Block (blk_a, arg_is_simple) ->
        let ocp_indent_compat blk =
          if c.conf.fmt_opts.ocp_indent_compat.v && not arg_is_simple then
            (* Indent body of docked struct. *)
            { blk with
              opn= Some (open_hvbox 2)
            ; psp= break 1000 2
            ; cls= close_box
            ; bdy= blk.bdy $ blk.esp $ fmt_opt blk.epi
            ; esp= noop
            ; epi= None }
          else blk
        in
        let fmt_rator =
          let break_struct =
            c.conf.fmt_opts.break_struct.v && (not dock_struct)
            && not arg_is_simple
          in
          compose_module (fmt_module_expr c (sub_mod ~ctx me_f)) ~f:Fn.id
          $ break (if break_struct then 1000 else 1) 0
          $ str "("
        in
        let epi =
          fmt_opt blk_a.epi $ str ")"
          $ fmt_attributes_and_docstrings c attrs
          $ Cmts.fmt_after c loc
        in
        if Option.is_some blk_a.pro then
          ocp_indent_compat
            { blk_a with
              pro=
                Some
                  ( Cmts.fmt_before c loc $ hvbox 2 fmt_rator
                  $ fmt_opt blk_a.pro )
            ; epi= Some epi }
        else
          { blk_a with
            opn= Some (open_hvbox 2 $ fmt_opt blk_a.opn)
          ; bdy= Cmts.fmt_before c loc $ open_hvbox 2 $ fmt_rator $ blk_a.bdy
          ; cls= close_box $ blk_a.cls $ close_box
          ; epi= Some epi } )
  | _ ->
      let blk_f = fmt_module_expr ~dock_struct:false c (sub_mod ~ctx me_f) in
      let has_epi = Cmts.has_after c.cmts loc || not (List.is_empty attrs) in
      { empty with
        opn= Some (fmt_opt blk_f.opn $ open_hvbox 2)
      ; bdy=
          hvbox 2
            ( Cmts.fmt_before c loc
            $ wrap_if parens (str "(") (str ")")
                (fmt_opt blk_f.pro $ blk_f.psp $ blk_f.bdy $ blk_f.esp)
            $ fmt_opt blk_f.epi $ break 1 0
            $
            match arg with
            | `Unit x -> x
            | `Block (x, _) ->
                wrap (str "(") (str ")") (compose_module x ~f:Fn.id) )
      ; cls= close_box $ blk_f.cls
      ; epi=
          Option.some_if has_epi
            (Cmts.fmt_after c loc $ fmt_attributes_and_docstrings c attrs) }

and fmt_module_expr ?(dock_struct = true) c ({ast= m; _} as xmod) =
  let ctx = Mod m in
  let {pmod_desc; pmod_loc; pmod_attributes} = m in
  update_config_maybe_disabled_block c pmod_loc pmod_attributes
  @@ fun c ->
  let parens = parenze_mod xmod in
  match pmod_desc with
  | Pmod_apply_unit (me, loc) ->
      let arg =
        Cmts.fmt c loc @@ hvbox 0
        @@ wrap (str "(") (str ")")
        @@ Cmts.fmt_within c loc
      in
      fmt_mod_apply c ctx ~parens ~dock_struct pmod_loc pmod_attributes me
        (`Unit arg)
  | Pmod_apply (me_f, me_a) ->
      let dock_struct =
        match me_f.pmod_desc with
        | Pmod_apply _ -> false
        | Pmod_ident _ -> dock_struct
        | _ -> true
      in
      let blk_a = fmt_module_expr c (sub_mod ~ctx me_a) in
      fmt_mod_apply c ctx ~parens ~dock_struct pmod_loc pmod_attributes me_f
        (`Block (blk_a, Mod.is_simple me_a))
  | Pmod_constraint (me, mt) ->
      let blk_e = fmt_module_expr c (sub_mod ~ctx me) in
      let blk_t = fmt_module_type c (sub_mty ~ctx mt) in
      let has_epi =
        Cmts.has_after c.cmts pmod_loc || not (List.is_empty pmod_attributes)
      in
      { opn=
          Some
            ( fmt_opt blk_t.opn $ fmt_opt blk_e.opn
            $ open_hovbox (Params.Indent.mod_constraint c.conf ~lhs:me) )
      ; pro= Some (Cmts.fmt_before c pmod_loc $ str "(")
      ; psp= cut_break
      ; bdy=
          hvbox 0
            ( fmt_opt blk_e.pro $ blk_e.psp $ blk_e.bdy $ blk_e.esp
            $ fmt_opt blk_e.epi $ str " :"
            $ Params.Mod.break_constraint c.conf ~rhs:mt
            $ hvbox 0
                ( fmt_opt blk_t.pro $ blk_t.psp $ blk_t.bdy $ blk_t.esp
                $ fmt_opt blk_t.epi ) )
          $ closing_paren c ~offset:(-2)
      ; cls= close_box $ blk_e.cls $ blk_t.cls
      ; esp= noop
      ; epi=
          Option.some_if has_epi
            ( Cmts.fmt_after c pmod_loc
            $ fmt_attributes_and_docstrings c pmod_attributes ) }
  | Pmod_functor (args, me) ->
      let doc, atrs = doc_atrs pmod_attributes in
      { empty with
        bdy=
          Cmts.fmt c pmod_loc
            ( fmt_docstring c ~epi:cut_break doc
            $ hvbox 0
                (wrap_if parens (str "(") (str ")")
                   ( str "functor"
                   $ fmt_attributes c ~pre:Blank atrs
                   $ break 1 2
                   $ list args (break 1 2) (fmt_functor_param c ctx)
                   $ break 1 2 $ str "->" $ break 1 2
                   $ compose_module
                       (fmt_module_expr c (sub_mod ~ctx me))
                       ~f:(hvbox 0) ) ) ) }
  | Pmod_ident lid ->
      { empty with
        opn= Some (open_hvbox 2)
      ; bdy=
          Cmts.fmt c pmod_loc
            ( fmt_longident_loc c lid
            $ fmt_attributes_and_docstrings c pmod_attributes )
      ; cls= close_box }
  | Pmod_structure sis ->
      let empty =
        List.is_empty sis && not (Cmts.has_within c.cmts pmod_loc)
      in
      let before = Cmts.fmt_before c pmod_loc in
      let within = Cmts.fmt_within c ~pro:noop pmod_loc in
      let after = Cmts.fmt_after c pmod_loc in
      { opn= None
      ; pro= Some (before $ str "struct" $ fmt_if empty (str " "))
      ; psp=
          fmt_if (not empty)
            (fmt_or c.conf.fmt_opts.break_struct.v (break 1000 2) (break 1 2))
      ; bdy= within $ fmt_structure c ctx sis
      ; cls= noop
      ; esp=
          fmt_if (not empty)
            (fmt_or c.conf.fmt_opts.break_struct.v force_break (break 1 0))
      ; epi=
          Some
            ( hovbox_if (not empty) 0
                (str "end" $ fmt_attributes_and_docstrings c pmod_attributes)
            $ after ) }
  | Pmod_unpack (e, ty1, ty2) ->
      let package_type sep (lid, cstrs, attrs) =
        break 1 (Params.Indent.mod_unpack_annot c.conf)
        $ hovbox 0
            ( hovbox 0 (str sep $ fmt_longident_loc c lid)
            $ fmt_package_type c ctx cstrs
            $ fmt_attributes c attrs )
      in
      { empty with
        opn= Some (open_hvbox 2)
      ; cls= close_box
      ; bdy=
          Cmts.fmt c pmod_loc
            ( hvbox 2
                (wrap_fits_breaks ~space:false c.conf "(" ")"
                   ( str "val "
                   $ fmt_expression c (sub_exp ~ctx e)
                   $ opt ty1 (package_type ": ")
                   $ opt ty2 (package_type ":> ") ) )
            $ fmt_attributes_and_docstrings c pmod_attributes ) }
  | Pmod_extension x1 ->
      { empty with
        opn= Some (open_hvbox 2)
      ; cls= close_box
      ; bdy=
          Cmts.fmt c pmod_loc
            ( fmt_extension c ctx x1
            $ fmt_attributes_and_docstrings c pmod_attributes ) }
  | Pmod_hole ->
      { empty with
        opn= Some (open_hvbox 2)
      ; cls= close_box
      ; bdy=
          Cmts.fmt c pmod_loc
            (fmt_hole () $ fmt_attributes_and_docstrings c pmod_attributes)
      }

and fmt_structure c ctx itms =
  let update_config c i =
    match i.pstr_desc with
    | Pstr_attribute atr -> update_config c [atr]
    | _ -> c
  in
  let fmt_item c ctx ~prev:_ ~next i =
    let semisemi =
      match next with
      | Some ({pstr_desc= Pstr_eval _; _}, _) -> true
      | _ -> false
    in
    fmt_structure_item c ~last:(Option.is_none next) ~semisemi
      (sub_str ~ctx i)
  in
  let ast x = Str x in
  fmt_item_list c ctx update_config ast fmt_item itms

and fmt_type c ?eq rec_flag decls ctx =
  let update_config c td = update_config_attrs c td.ptype_attributes in
  let is_rec = Asttypes.is_recursive rec_flag in
  let fmt_decl c ctx ~prev ~next:_ decl =
    let first = Option.is_none prev in
    let pre =
      if first then if is_rec then "type" else "type nonrec" else "and"
    in
    fmt_type_declaration c ~pre ?eq (sub_td ~ctx decl)
  in
  let ast x = Td x in
  fmt_item_list c ctx update_config ast fmt_decl decls

and fmt_structure_item c ~last:last_item ~semisemi {ctx= parent_ctx; ast= si}
    =
  protect c (Str si)
  @@
  let ctx = Str si in
  let fmt_cmts_before = Cmts.Toplevel.fmt_before c si.pstr_loc in
  let fmt_cmts_after = Cmts.Toplevel.fmt_after c si.pstr_loc in
  (fun k ->
    fmt_cmts_before
    $ hvbox 0 ~name:"stri"
        (box_semisemi c ~parent_ctx semisemi (k $ fmt_cmts_after)) )
  @@
  match si.pstr_desc with
  | Pstr_attribute attr -> fmt_floating_attributes_and_docstrings c [attr]
  | Pstr_eval (exp, atrs) ->
      let doc, atrs = doc_atrs atrs in
      fmt_docstring c doc
      $ cbox 0 ~name:"eval" (fmt_expression c (sub_exp ~ctx exp))
      $ fmt_item_attributes c ~pre:Space atrs
  | Pstr_exception extn_constr ->
      let pre = str "exception" in
      hvbox 2 ~name:"exn" (fmt_type_exception ~pre c ctx extn_constr)
  | Pstr_include {pincl_mod; pincl_attributes= attributes; pincl_loc} ->
      update_config_maybe_disabled_attrs c pincl_loc attributes
      @@ fun c ->
      let keyword = str "include" in
      fmt_module_statement c ~attributes ~keyword (sub_mod ~ctx pincl_mod)
  | Pstr_module mb ->
      fmt_module_binding c ~rec_flag:false ~first:true (sub_mb ~ctx mb)
  | Pstr_open
      {popen_expr; popen_override; popen_attributes= attributes; popen_loc}
    ->
      update_config_maybe_disabled_attrs c popen_loc attributes
      @@ fun c ->
      let keyword =
        fmt_or
          (is_override popen_override)
          ( str "open!"
          $ fmt_if (Option.is_some attributes.attrs_extension) space_break )
          (str "open")
      in
      fmt_module_statement c ~attributes ~keyword (sub_mod ~ctx popen_expr)
  | Pstr_primitive vd -> fmt_value_description c ctx vd
  | Pstr_recmodule mbs ->
      fmt_recmodule c ctx mbs fmt_module_binding (fun x -> Mb x) sub_mb
  | Pstr_type (rec_flag, decls) -> fmt_type c rec_flag decls ctx
  | Pstr_typext te -> fmt_type_extension c ctx te
  | Pstr_value {pvbs_rec= rec_flag; pvbs_bindings= bindings} ->
      let update_config c i =
        update_config ~quiet:true c
          (i.pvb_attributes.attrs_before @ i.pvb_attributes.attrs_after)
      in
      let ast x = Lb x in
      let fmt_item c ctx ~prev ~next b =
        let first = Option.is_none prev in
        let last = Option.is_none next in
        let b = Sugar.Let_binding.of_let_binding ~ctx ~first b in
        let epi =
          match c.conf.fmt_opts.let_binding_spacing.v with
          | `Compact -> None
          | `Sparse when last && last_item -> None
          | `Sparse -> Some (fits_breaks "" "\n")
          | `Double_semicolon ->
              Option.some_if (last && not semisemi)
                (fits_breaks "" ~hint:(1000, 0) ";;")
        in
        let rec_flag = first && Asttypes.is_recursive rec_flag in
        fmt_value_binding c ~ctx0:ctx ~rec_flag ?epi b
      in
      fmt_item_list c ctx update_config ast fmt_item bindings
  | Pstr_modtype mtd -> fmt_module_type_declaration c ctx mtd
  | Pstr_extension (ext, atrs) ->
      let doc_before, doc_after, atrs = fmt_docstring_around_item c atrs in
      let box =
        match snd ext with
        | PTyp _ | PPat _ | PStr [_] | PSig [_] -> true
        | PStr _ | PSig _ -> false
      in
      hvbox_if box c.conf.fmt_opts.stritem_extension_indent.v ~name:"ext1"
        ( doc_before
        $ hvbox_if (not box) 0 ~name:"ext2" (fmt_item_extension c ctx ext)
        $ fmt_item_attributes c ~pre:Space atrs
        $ doc_after )
  | Pstr_class_type cl -> fmt_class_types c ~pre:"class type" ~sep:"=" cl
  | Pstr_class cls -> fmt_class_exprs c cls

and fmt_let c ~ctx0 ~rec_flag ~bindings ~parens ~fmt_atrs ~fmt_expr ~loc_in
    ~body_loc ~has_attr ~indent_after_in =
  let fmt_in indent =
    match c.conf.fmt_opts.break_before_in.v with
    | `Fit_or_vertical -> break 1 (-indent) $ str "in"
    | `Auto -> fits_breaks " in" ~hint:(1, -indent) "in"
  in
  let fmt_binding ~first ~last binding =
    let in_ =
      if last then Some {txt= (fun indent -> fmt_in indent); loc= loc_in}
      else None
    in
    let rec_flag = first && Asttypes.is_recursive rec_flag in
    fmt_value_binding c ~ctx0 ~rec_flag ?in_ binding
    $ fmt_if (not last)
        ( match c.conf.fmt_opts.let_and.v with
        | `Sparse -> force_break
        | `Compact -> space_break )
  in
  let blank_line_after_in = sequence_blank_line c loc_in body_loc in
  Params.Exp.wrap c.conf ~parens ~fits_breaks:false
    ( Params.Exp.wrap c.conf ~parens:has_attr ~fits_breaks:false
        (vbox 0
           ( hvbox 0 (list_fl bindings fmt_binding)
           $ ( if blank_line_after_in then str "\n" $ cut_break
               else break 1000 indent_after_in )
           $ hvbox 0 fmt_expr ) )
    $ fmt_atrs )

and fmt_value_constraint c vc_opt =
  match vc_opt with
  | Some vc -> (
      let ctx = Vc vc in
      match vc with
      | Pvc_constraint {locally_abstract_univars= []; typ} ->
          (noop, fmt_type_cstr c (sub_typ ~ctx typ))
      | Pvc_constraint {locally_abstract_univars= pvars; typ} -> (
        match c.conf.fmt_opts.break_colon.v with
        | `Before ->
            ( noop
            , fmt_constraint_sep c ":"
              $ hvbox 0
                  ( str "type "
                  $ list pvars (str " ") (fmt_str_loc c)
                  $ str "." $ space_break
                  $ fmt_core_type c (sub_typ ~ctx typ) ) )
        | `After ->
            ( fmt_constraint_sep c ":"
              $ hvbox 0
                  ( str "type "
                  $ list pvars (str " ") (fmt_str_loc c)
                  $ str "." )
            , space_break $ fmt_core_type c (sub_typ ~ctx typ) ) )
      | Pvc_coercion {ground; coercion} ->
          ( noop
          , opt ground (fun ty ->
                fmt_constraint_sep c ":" $ fmt_core_type c (sub_typ ~ctx ty) )
            $ fmt_constraint_sep c ":>"
            $ fmt_core_type c (sub_typ ~ctx coercion) ) )
  | None -> (noop, noop)

and fmt_value_binding c ~ctx0 ~rec_flag ?in_ ?epi
    {lb_op; lb_pat; lb_args; lb_typ; lb_body; lb_attrs; lb_loc; lb_pun} =
  let in_, loc_in =
    match in_ with
    | None -> (None, None)
    | Some {txt; loc} -> (Some txt, Some loc)
  in
  let loc_with_in =
    match loc_in with
    | None -> lb_loc
    | Some loc_in ->
        { loc_start= lb_loc.loc_start
        ; loc_end= loc_in.loc_end
        ; loc_ghost= lb_loc.loc_ghost || loc_in.loc_ghost }
  in
  update_config_maybe_disabled_attrs c loc_with_in lb_attrs
  @@ fun c ->
  let lb_pun =
    Ocaml_version.(
      compare c.conf.opr_opts.ocaml_version.v Releases.v4_13_0 >= 0 )
    && lb_pun
  in
  let doc1, doc2, at_attrs, at_at_attrs =
    fmt_docstring_around_item_attrs ~force_before:true c lb_attrs
  in
  let fmt_newtypes, fmt_cstr = fmt_value_constraint c lb_typ in
  let indent, intro_as_pro =
    match (lb_args, lb_body.ast) with
    | _, Pfunction_cases _
     |( []
      , Pfunction_body
          { pexp_attributes= []
          ; pexp_desc= Pexp_function ([], None, Pfunction_cases _)
          ; _ } ) ->
        (c.conf.fmt_opts.function_indent.v, true)
    | _, Pfunction_body {pexp_desc= Pexp_function (_, _, _); _}
      when c.conf.fmt_opts.let_binding_deindent_fun.v ->
        (max (c.conf.fmt_opts.let_binding_indent.v - 1) 0, false)
    | _ -> (c.conf.fmt_opts.let_binding_indent.v, false)
  in
  let toplevel, in_, epi, cmts_before, cmts_after =
    match in_ with
    | Some in_ ->
        ( false
        , fmt_item_attributes c ~pre:(Break (1, 2)) at_at_attrs $ in_ indent
        , fmt_opt epi
        , Cmts.fmt_before c lb_loc
        , Cmts.fmt_after c lb_loc ~pro:force_break )
    | None ->
        let epi =
          fmt_item_attributes c ~pre:(Break (1, 0)) at_at_attrs $ fmt_opt epi
        in
        ( true
        , noop
        , epi
        , Cmts.Toplevel.fmt_before c lb_loc
        , Cmts.Toplevel.fmt_after c lb_loc )
  in
  let ext = lb_attrs.attrs_extension in
  let should_break_after_keyword =
    Cmts.has_before c.cmts lb_pat.ast.ppat_loc
    || Option.is_some ext
       &&
       match lb_pat.ast with
       | {ppat_desc= Ppat_record _ | Ppat_list _ | Ppat_array _; _}
         when c.conf.fmt_opts.dock_collection_brackets.v ->
           false
       | _ -> true
  in
  let decl =
    let decl =
      fmt_str_loc c lb_op
      $ fmt_extension_suffix c ext
      $ fmt_attributes c at_attrs
      $ fmt_if rec_flag (str " rec")
      $ fmt_or should_break_after_keyword space_break (str " ")
    and pattern = fmt_pattern c lb_pat
    and args =
      fmt_if
        (not (List.is_empty lb_args))
        (space_break $ wrap_fun_decl_args c (fmt_expr_fun_args c lb_args))
      $ fmt_newtypes
    in
    let decl_args =
      box_fun_decl_args c 4
        (Params.Align.fun_decl c.conf ~decl ~pattern ~args)
    in
    hovbox (Params.Indent.fun_type_annot c.conf) (decl_args $ fmt_cstr)
  in
  let decl_and_body =
    if lb_pun then decl
    else
      let fmt_body ?pro ?box {ast; ctx} =
        match ast with
        | Pfunction_cases _ as body ->
            let wrap_intro intro =
              hovbox 2 (fmt_opt pro $ intro) $ space_break
            in
            fmt_function ~ctx ~ctx0 ~wrap_intro ?box ~label:Nolabel ~attrs:[]
              ~loc:lb_loc c ([], None, body)
        | Pfunction_body body ->
            fmt_expression c ?pro ?box (sub_exp ~ctx body)
      in
      let pro =
        if c.conf.fmt_opts.ocp_indent_compat.v then
          let box =
            match lb_body.ast with Pfunction_cases _ -> false | _ -> true
          in
          hvbox_if box 2 (decl $ fits_breaks " =" ~hint:(1000, 0) "=")
          $ space_break
        else hovbox 2 (decl $ break 1 2 $ str "=") $ space_break
      in
      if intro_as_pro then fmt_body ~pro ~box:false lb_body
      else pro $ fmt_body lb_body
  in
  doc1 $ cmts_before
  $ hvbox 0
      ( hvbox indent
          ( hvbox_if toplevel 0
              ( hvbox_if toplevel indent decl_and_body
              $ cmts_after
              $ opt loc_in
                  (Cmts.fmt_before c ~pro:force_break ~epi:noop ~eol:noop) )
          $ in_ )
      $ opt loc_in (Cmts.fmt_after ~pro:force_break c)
      $ epi )
  $ doc2

and fmt_module_binding c ~rec_flag ~first {ast= pmb; _} =
  let {pmb_name; pmb_ext_attrs= attrs; _} = pmb in
  protect c (Mb pmb)
  @@ update_config_maybe_disabled_attrs c pmb.pmb_loc attrs
  @@ fun c ->
  let ctx = Mb pmb in
  let keyword = if first then "module" else "and" in
  let xbody = sub_mod ~ctx pmb.pmb_expr in
  let xbody, xmty =
    match xbody.ast with
    | { pmod_desc= Pmod_constraint (body_me, body_mt)
      ; pmod_loc
      ; pmod_attributes= [] } ->
        Cmts.relocate c.cmts ~src:pmod_loc ~before:body_me.pmod_loc
          ~after:body_mt.pmty_loc ;
        (sub_mod ~ctx body_me, Some (sub_mty ~ctx body_mt))
    | _ -> (xbody, None)
  in
  Cmts.fmt c pmb.pmb_loc
    (fmt_module ~rec_:rec_flag c ctx keyword ~rec_flag:(rec_flag && first)
       ~eqty:":" pmb_name pmb.pmb_args (Some xbody) xmty ~attrs )

let fmt_toplevel_directive c ~semisemi dir =
  let fmt_dir_arg = function
    | Pdir_string s -> str (Printf.sprintf "%S" s)
    | Pdir_int (lit, Some m) -> str (Printf.sprintf "%s%c" lit m)
    | Pdir_int (lit, None) -> str lit
    | Pdir_ident longident -> fmt_longident longident
    | Pdir_bool bool -> str (Bool.to_string bool)
  in
  let {pdir_name= name; pdir_arg; pdir_loc} = dir in
  let name = fmt_str_loc c name ~pre:"#" in
  let args =
    match pdir_arg with
    | None -> noop
    | Some {pdira_desc; pdira_loc; _} ->
        str " "
        $ Cmts.fmt_before ~epi:(str " ") c pdira_loc
        $ fmt_dir_arg pdira_desc
        $ Cmts.fmt_after c pdira_loc
  in
  Cmts.fmt c pdir_loc (box_semisemi c ~parent_ctx:Top semisemi (name $ args))

let flatten_ptop =
  List.concat_map ~f:(function
    | Ptop_def items -> List.map items ~f:(fun i -> `Item i)
    | Ptop_dir d -> [`Directive d] )

let fmt_toplevel ?(force_semisemi = false) c ctx itms =
  let itms = flatten_ptop itms in
  let update_config c = function
    | `Item {pstr_desc= Pstr_attribute atr; _} -> update_config c [atr]
    | _ -> c
  in
  let fmt_item c ctx ~prev:_ ~next itm =
    let last = Option.is_none next in
    let semisemi =
      match (itm, next) with
      | _, Some (`Item {pstr_desc= Pstr_eval _; _}, _) -> true
      | `Item {pstr_desc= Pstr_attribute _; _}, _ -> false
      | `Item _, Some (`Directive _, _) -> true
      | _ -> force_semisemi && last
    in
    match itm with
    | `Item i -> fmt_structure_item c ~last ~semisemi (sub_str ~ctx i)
    | `Directive d -> fmt_toplevel_directive c ~semisemi d
  in
  let ast x = Tli x in
  fmt_item_list c ctx update_config ast fmt_item itms

let fmt_repl_phrase c ctx {prepl_phrase; prepl_output} =
  str "# "
  $ fmt_toplevel ~force_semisemi:true c ctx [prepl_phrase]
  $ fmt_if
      (not (String.is_empty prepl_output))
      (force_break $ str prepl_output)

let fmt_repl_file c _ itms =
  vbox 0 @@ list itms force_break @@ fmt_repl_phrase c Rep

(** Entry points *)

module Chunk = struct
  open Chunk

  let fmt_item (type a) (fg : a list item) : c -> Ast.t -> a list -> Fmt.t =
    match fg with
    | Structure -> fmt_structure
    | Signature -> fmt_signature
    | Use_file -> fmt_toplevel ?force_semisemi:None

  let update_conf c state = {c with conf= Conf.update_state c.conf state}

  let fmt fg c ctx chunks =
    List.foldi chunks ~init:(c, noop, [])
      ~f:(fun i (c, output, locs) chunk ->
        let c = update_conf c chunk.state in
        let output, locs =
          match chunk.state with
          | `Disable ->
              let output =
                output
                $ Cmts.fmt_before c chunk.attr_loc
                    ~eol:(str "\n" $ force_break)
                $ fmt_if (i > 0) (str "\n" $ force_break)
                $ str
                    (String.strip
                       (Source.string_at c.source chunk.chunk_loc) )
              in
              (output, chunk.chunk_loc :: locs)
          | `Enable ->
              let output =
                output
                $ fmt_if (i > 0) force_break
                $ fmt_item fg c ctx chunk.items
              in
              (output, locs)
        in
        (c, output, locs) )
    |> fun ((_ : c), output, locs) ->
    List.iter locs ~f:(Cmts.drop_inside c.cmts) ;
    output

  let split_and_fmt fg c ctx l =
    let state = if c.conf.opr_opts.disable.v then `Disable else `Enable in
    fmt fg c ctx @@ split fg l ~state
end

let fmt_file (type a) ~ctx ~fmt_code ~debug (fragment : a Extended_ast.t)
    source cmts conf (itms : a) =
  let c = {source; cmts; conf; debug; fmt_code} in
  match (fragment, itms) with
  | Structure, [] | Signature, [] | Use_file, [] ->
      Cmts.fmt_after ~pro:noop c Location.none
  | Structure, l -> Chunk.split_and_fmt Structure c ctx l
  | Signature, l -> Chunk.split_and_fmt Signature c ctx l
  | Use_file, l -> Chunk.split_and_fmt Use_file c ctx l
  | Core_type, ty -> fmt_core_type c (sub_typ ~ctx:(Pld (PTyp ty)) ty)
  | Module_type, mty ->
      compose_module ~f:Fn.id
        (fmt_module_type c (sub_mty ~ctx:(Mty mty) mty))
  | Expression, e ->
      fmt_expression c (sub_exp ~ctx:(Str (Ast_helper.Str.eval e)) e)
  | Pattern, p -> fmt_pattern c (sub_pat ~ctx:(Pld (PPat (p, None))) p)
  | Repl_file, l -> fmt_repl_file c ctx l
  | Documentation, d ->
      (* TODO: [source] and [cmts] should have never been computed when
         formatting doc. *)
      Fmt_odoc.fmt_ast c.conf ~fmt_code:c.fmt_code d

let fmt_parse_result conf ~debug ast_kind ast source comments
    ~set_margin:set_margin_p ~fmt_code =
  let cmts = Cmts.init ast_kind ~debug source ast comments in
  let ctx = Top in
  let code =
    fmt_if set_margin_p (set_margin conf.Conf.fmt_opts.margin.v)
    $ fmt_file ~ctx ~debug ast_kind source cmts conf ast ~fmt_code
  in
  Ok code

let fmt_code ~debug =
  let rec fmt_code (conf : Conf.t) ~offset ~set_margin s =
    let {Conf.fmt_opts; _} = conf in
    let conf =
      (* Adjust margin according to [offset]. *)
      let margin = {fmt_opts.margin with v= fmt_opts.margin.v - offset} in
      {conf with fmt_opts= {fmt_opts with margin}}
    in
    let warn = fmt_opts.parse_toplevel_phrases.v in
    let input_name = !Location.input_name in
    match
      Parse_with_comments.parse_toplevel ~disable_deprecated:true conf
        ~input_name ~source:s
    with
    | Either.First {ast; comments; source; prefix= _} ->
        fmt_parse_result conf ~debug Use_file ast source comments ~set_margin
          ~fmt_code
    | Second {ast; comments; source; prefix= _} ->
        fmt_parse_result conf ~debug Repl_file ast source comments
          ~set_margin ~fmt_code
    | exception Syntaxerr.Error (Expecting (_, x)) when warn ->
        Error (`Msg (Format.asprintf "expecting: %s" x))
    | exception Syntaxerr.Error (Not_expecting (_, x)) when warn ->
        Error (`Msg (Format.asprintf "not expecting: %s" x))
    | exception Syntaxerr.Error (Other _) when warn ->
        Error (`Msg (Format.asprintf "invalid toplevel or OCaml syntax"))
    | exception e when warn -> Error (`Msg (Format.asprintf "%a" Exn.pp e))
    | exception _ -> Error (`Msg "")
  in
  fmt_code

let fmt_ast fragment ~debug source cmts conf l =
  (* [Ast.init] should be called only once per file. In particular, we don't
     want to call it when formatting comments *)
  Ast.init conf ;
  let fmt_code = fmt_code ~debug in
  fmt_file ~ctx:Top ~fmt_code ~debug fragment source cmts conf l
