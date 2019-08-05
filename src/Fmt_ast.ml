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

module Format = Format_

(** Format OCaml Ast *)

open Migrate_ast
open Asttypes
open Parsetree
open Ast
open Fmt

type c = {conf: Conf.t; source: Source.t; cmts: Cmts.t}

module Cmts = struct
  include Cmts

  let fmt c = fmt c.cmts c.conf

  let fmt_before c = fmt_before c.cmts c.conf

  let fmt_within c = fmt_within c.cmts c.conf

  let fmt_after c = fmt_after c.cmts c.conf

  let fmt_list c = fmt_list c.cmts c.conf
end

type block =
  { opn: Fmt.t
  ; pro: Fmt.t option
  ; psp: Fmt.t
  ; bdy: Fmt.t
  ; cls: Fmt.t
  ; esp: Fmt.t
  ; epi: Fmt.t option }

let smallest_loc loc stack =
  List.reduce_exn (loc :: stack) ~f:(fun a b ->
      if Location.width a < Location.width b then a else b)

let empty =
  { opn= noop
  ; pro= None
  ; psp= noop
  ; bdy= noop
  ; cls= noop
  ; esp= noop
  ; epi= None }

let compose_module {opn; pro; psp; bdy; cls; esp; epi} ~f =
  f (Option.call ~f:pro $ opn $ psp $ bdy $ cls $ esp $ Option.call ~f:epi)

(* Debug: catch and report failures at nearest enclosing Ast.t *)

let protect =
  let first = ref true in
  fun ast pp fs ->
    try pp fs
    with exc ->
      if !first && Conf.debug then (
        let bt = Caml.Printexc.get_backtrace () in
        Format.pp_print_flush fs () ;
        Caml.Format.eprintf "@\nFAIL@\n%a@\n%s@.%!" Ast.dump ast bt ;
        first := false ) ;
      raise exc

let comma_sep c : Fmt.s =
  if Poly.(c.conf.break_separators = `Before) then "@,, " else ",@;<1 2>"

let update_config ?(quiet = false) c l =
  let update_one c {attr_name= {txt; loc}; attr_payload= payload; _} =
    let result =
      match txt with
      | "ocamlformat" -> (
        match payload with
        | PStr
            [ { pstr_desc=
                  Pstr_eval
                    ( { pexp_desc= Pexp_constant (Pconst_string (str, None))
                      ; pexp_attributes= []
                      ; _ }
                    , [] )
              ; _ } ] ->
            Conf.parse_line_in_attribute c.conf str
        | _ -> Error (`Malformed "string expected") )
      | _ when String.is_prefix ~prefix:"ocamlformat." txt ->
          Error
            (`Malformed
              (Format.sprintf "unknown suffix %S"
                 (String.chop_prefix_exn ~prefix:"ocamlformat." txt)))
      | _ -> Ok c.conf
    in
    match result with
    | Ok conf -> {c with conf}
    | Error error ->
        let reason = function
          | `Malformed line -> Format.sprintf "Invalid format %S" line
          | `Misplaced (name, _) ->
              Format.sprintf "%s not allowed here" name
          | `Unknown (name, _) -> Format.sprintf "Unknown option %s" name
          | `Bad_value (name, value) ->
              Format.sprintf "Invalid value for %s: %S" name value
        in
        let w = Warnings.Attribute_payload (txt, reason error) in
        if (not c.conf.quiet) && not quiet then Compat.print_warning loc w ;
        c
  in
  List.fold ~init:c l ~f:update_one

let match_indent c ~ctx ~default =
  match (c.conf.match_indent_nested, ctx) with
  | `Always, _ | _, (Top | Sig _ | Str _) -> c.conf.match_indent
  | _ -> default

let function_indent c ~ctx ~default =
  match (c.conf.function_indent_nested, ctx) with
  | `Always, _ | _, (Top | Sig _ | Str _) -> c.conf.function_indent
  | _ -> default

let fmt_expressions c width sub_exp exprs fmt_expr
    (p : Params.elements_collection) =
  match c.conf.break_collection_expressions with
  | `Fit_or_vertical ->
      let fmt_expr ~first ~last e =
        fmt_if_k (not first) p.sep_before
        $ fmt_expr e
        $ fmt_or_k last p.sep_after_final p.sep_after_non_final
      in
      list_fl exprs fmt_expr
  | `Wrap ->
      let is_simple x = is_simple c.conf width (sub_exp x) in
      let break x1 x2 = not (is_simple x1 && is_simple x2) in
      let grps = List.group exprs ~break in
      let fmt_grp ~first:first_grp ~last:last_grp exprs =
        let fmt_expr ~first ~last e =
          fmt_if_k (not (first && first_grp)) p.sep_before
          $ fmt_expr e
          $ fmt_or_k (last && last_grp) p.sep_after_final
              p.sep_after_non_final
        in
        list_fl exprs fmt_expr
      in
      hovbox (-2) (list_fl grps fmt_grp)

(** Handle the `break-fun-decl` option *)
let wrap_fun_decl_args c k =
  match c.conf.break_fun_decl with
  | `Wrap | `Fit_or_vertical -> k
  | `Smart -> hvbox 0 k

let box_fun_decl_args c =
  match c.conf.break_fun_decl with
  | `Fit_or_vertical -> hvbox
  | `Wrap | `Smart -> hovbox

(** Handle the `break-fun-sig` option *)
let box_fun_sig_args c =
  match c.conf.break_fun_sig with
  | _ when c.conf.ocp_indent_compat -> hvbox
  | `Fit_or_vertical -> hvbox
  | `Wrap | `Smart -> hovbox

let sugar_pmod_functor c ~for_functor_kw pmod =
  let source_is_long = Source.is_long_pmod_functor c.source in
  Sugar.functor_ c.cmts ~for_functor_kw ~source_is_long pmod

let wrap_fits_breaks_exp_begin_end ~parens k =
  vbox 2
    (wrap_if_k parens
       (fits_breaks "begin " "begin" $ break_unless_newline 0 0)
       (break_unless_newline 0 (-2) $ fits_breaks " end" "end")
       (cbox 0 k))

let parens_or_begin_end c ~loc =
  match c.conf.exp_grouping with
  | `Parens -> `Parens
  | `Preserve ->
      let str = String.lstrip (Source.string_at c.source loc) in
      if String.is_prefix ~prefix:"begin" str then `Begin_end else `Parens

let wrap_fits_breaks_exp_if ?(space = true) ?(disambiguate = false) c
    ~parens ~loc k =
  match parens_or_begin_end c ~loc with
  | `Parens when disambiguate && c.conf.disambiguate_non_breaking_match ->
      wrap_if_fits_or parens "(" ")" k
  | `Parens -> wrap_fits_breaks_if ~space c.conf parens "(" ")" k
  | `Begin_end -> wrap_fits_breaks_exp_begin_end ~parens k

let wrap_exp_if ?(disambiguate = false) c ~parens ~loc k =
  match parens_or_begin_end c ~loc with
  | `Parens when disambiguate && c.conf.disambiguate_non_breaking_match ->
      wrap_if_fits_or parens "(" ")" k
  | `Parens -> wrap_if parens "(" ")" k
  | `Begin_end -> wrap_fits_breaks_exp_begin_end ~parens k

let drop_while ~f s =
  let i = ref 0 in
  while !i < String.length s && f !i s.[!i] do
    Int.incr i
  done ;
  String.sub s ~pos:!i ~len:(String.length s - !i)

let maybe_disabled_k c (loc : Location.t) (l : attributes) f k =
  if not c.conf.disable then f c
  else
    let loc = Source.extend_loc_to_include_attributes c.source loc l in
    Cmts.drop_inside c.cmts loc ;
    let s = Source.string_at c.source loc in
    let indent_of_first_line = Position.column loc.loc_start in
    let l = String.split ~on:'\n' s in
    let l =
      List.mapi l ~f:(fun i s ->
          if i = 0 then s
          else
            drop_while s ~f:(fun i c ->
                Char.is_whitespace c && i < indent_of_first_line))
    in
    k (Cmts.fmt c loc (list l "@\n" str))

let maybe_disabled c loc l f = maybe_disabled_k c loc l f Fn.id

let update_config_maybe_disabled c loc l f =
  let c = update_config c l in
  maybe_disabled c loc l f

let update_config_maybe_disabled_block c loc l f =
  let fmt bdy = {empty with opn= open_vbox 2; bdy; cls= close_box} in
  let c = update_config c l in
  maybe_disabled_k c loc l f fmt

let make_groups c items ast update_config =
  let with_config c i =
    let c = update_config c i in
    (c, (i, c))
  in
  let _, items = List.fold_map items ~init:c ~f:with_config in
  let break (i1, c1) (i2, c2) =
    Ast.break_between c.source ~cmts:c.cmts ~has_cmts_before:Cmts.has_before
      ~has_cmts_after:Cmts.has_after
      (ast i1, c1.conf)
      (ast i2, c2.conf)
  in
  List.group items ~break

let fmt_groups c ctx grps fmt_grp =
  let break_struct = c.conf.break_struct || Poly.(ctx = Top) in
  list_fl grps (fun ~first ~last grp ->
      fmt_if (break_struct && not first) "\n@\n"
      $ fmt_if ((not break_struct) && not first) "@;<1000 0>"
      $ fmt_grp ~first ~last grp
      $ fits_breaks_if ((not break_struct) && not last) "" "\n")

let fmt_recmodule c ctx items f ast =
  let update_config c i = update_config c (Ast.attributes (ast i)) in
  let grps = make_groups c items ast update_config in
  let break_struct = c.conf.break_struct || Poly.(ctx = Top) in
  let fmt_grp ~first:first_grp ~last:_ itms =
    list_fl itms (fun ~first ~last:_ (itm, c) ->
        fmt_if_k (not first) (fmt_or break_struct "@\n" "@ ")
        $ maybe_disabled c (Ast.location (ast itm)) []
          @@ fun c -> f c ctx ~rec_flag:true ~first:(first && first_grp) itm)
  in
  hvbox 0 (fmt_groups c ctx grps fmt_grp)

(* In several places, naked newlines (i.e. not "@\n") are used to avoid
   trailing space in open lines. *)
(* In several places, a break such as "@;<1000 0>" is used to force the
   enclosing box to break across multiple lines. *)

let rec fmt_longident (li : Longident.t) =
  match li with
  | Lident id -> str id
  | Ldot (li, id) ->
      hvbox 0
        ( fmt_longident li $ fmt "@,."
        $ wrap_if (is_symbol_id id) "( " " )" (str id) )
  | Lapply (li1, li2) ->
      hvbox 2 (fmt_longident li1 $ wrap "@,(" ")" (fmt_longident li2))

let fmt_longident_loc c ?(pre = noop) {txt; loc} =
  Cmts.fmt c loc (pre $ fmt_longident txt)

let fmt_str_loc c ?(pre = noop) {txt; loc} = Cmts.fmt c loc (pre $ str txt)

let char_escaped c ~loc chr =
  match (c.conf.escape_chars, chr) with
  | `Hexadecimal, _ -> Format.sprintf "\\x%02x" (Char.to_int chr)
  | `Preserve, _ -> (
    match Source.char_literal c.source loc with
    | None -> Char.escaped chr
    | Some c -> c )
  | _, '\000' .. '\128' -> Char.escaped chr
  | `Decimal, _ -> Char.escaped chr

let escape_string mode str =
  match mode with
  | `Hexadecimal ->
      let buf = Bytes.create (4 * String.length str) in
      for i = 0 to String.length str - 1 do
        let src =
          Bytes.of_string (Printf.sprintf "\\x%02x" (Char.to_int str.[i]))
        in
        Bytes.blit ~dst:buf ~dst_pos:(i * 4) ~src ~src_pos:0 ~len:4
      done ;
      Bytes.to_string buf
  | `Preserve -> str
  | `Decimal ->
      let n = ref 0 in
      for i = 0 to String.length str - 1 do
        let l =
          match str.[i] with
          | '"' | '\\' | '\n' | '\t' | '\r' | '\b' -> 2
          | ' ' .. '~' -> 1
          | _ -> 4
        in
        n := !n + l
      done ;
      if !n = String.length str then str
      else
        let buf = Bytes.create !n in
        n := 0 ;
        let set c = Bytes.set buf !n c ; Int.incr n in
        for i = 0 to String.length str - 1 do
          let chr = str.[i] in
          match chr with
          | '"' | '\\' -> set '\\' ; set chr
          | '\n' -> set '\\' ; set 'n'
          | '\t' -> set '\\' ; set 't'
          | '\r' -> set '\\' ; set 'r'
          | '\b' -> set '\\' ; set 'b'
          | ' ' .. '~' -> set chr
          | _ ->
              let code = Char.to_int chr in
              set '\\' ;
              set (Char.of_int_exn (48 + (code / 100))) ;
              set (Char.of_int_exn (48 + (code / 10 % 10))) ;
              set (Char.of_int_exn (48 + (code % 10)))
        done ;
        Bytes.to_string buf

let fmt_constant c ~loc ?epi const =
  match const with
  | Pconst_integer (lit, suf) | Pconst_float (lit, suf) ->
      str lit $ opt suf char
  | Pconst_char x -> wrap "'" "'" @@ str (char_escaped ~loc c x)
  | Pconst_string (s, Some delim) ->
      wrap_k (str ("{" ^ delim ^ "|")) (str ("|" ^ delim ^ "}")) (str s)
  | Pconst_string (s, None) -> (
      let fmt_line mode s =
        match c.conf.break_string_literals with
        | `Wrap | `Newlines_and_wrap ->
            let words = String.split (escape_string mode s) ~on:' ' in
            let fmt_word ?prev:_ curr ?next =
              match next with
              | Some "" -> str curr $ str " "
              | Some _ -> str curr $ pre_break 1 " \\" 0
              | _ -> str curr
            in
            hovbox_if (List.length words > 1) 0 (list_pn words fmt_word)
        | _ -> str (escape_string mode s)
      in
      let fmt_lines ?(break_on_newlines = false) mode lines =
        let delim = ["@,"; "@;"] in
        let fmt_line ?prev:_ curr ?next =
          let fmt_next next =
            let not_suffix suffix = not (String.is_suffix curr ~suffix) in
            let print_ln =
              List.for_all delim ~f:not_suffix || not break_on_newlines
            in
            if String.is_empty next then
              if break_on_newlines then
                fmt_if_k (String.is_empty curr) (str "\\n")
              else str "\\n"
            else if Char.equal next.[0] ' ' then
              fmt_if_k print_ln (str "\\n")
              $ pre_break 0 "\\" (-1) $ if_newline "\\"
            else fmt_if_k print_ln (str "\\n") $ pre_break 0 "\\" 0
          in
          fmt_line mode curr $ opt next fmt_next
        in
        hvbox 1
          (wrap "\"" "\"" (list_pn lines fmt_line) $ Option.call ~f:epi)
      in
      let s, mode =
        match (c.conf.break_string_literals, c.conf.escape_strings) with
        | `Never, `Preserve -> (
          match Source.string_literal c.source `Preserve loc with
          | None -> (s, `Decimal)
          | Some s -> (s, `Preserve) )
        | (`Newlines | `Wrap | `Newlines_and_wrap), `Preserve -> (
          match Source.string_literal c.source `Normalize loc with
          | None -> (s, `Decimal)
          | Some s -> (s, `Preserve) )
        | _ -> (s, c.conf.escape_strings)
      in
      let contains_pp_commands =
        let is_substring substring = String.is_substring s ~substring in
        List.exists ["@,"; "@;"] ~f:is_substring
      in
      match c.conf.break_string_literals with
      | (`Newlines | `Newlines_and_wrap) when contains_pp_commands ->
          let break_on_pp_commands in_ pattern =
            String.substr_replace_all in_ ~pattern ~with_:(pattern ^ "\n")
          in
          List.fold_left ["@,"; "@;"; "@\n"] ~init:s ~f:break_on_pp_commands
          |> String.split ~on:'\n'
          |> fmt_lines mode ~break_on_newlines:true
      | `Newlines | `Wrap | `Newlines_and_wrap ->
          fmt_lines mode (String.split ~on:'\n' s)
      | `Never -> wrap "\"" "\"" (fmt_line mode s) )

let fmt_variance = function
  | Covariant -> str "+"
  | Contravariant -> str "-"
  | Invariant -> noop

let fmt_label lbl sep =
  match lbl with
  | Nolabel -> noop
  | Labelled l -> str "~" $ str l $ fmt sep
  | Optional l -> str "?" $ str l $ fmt sep

let fmt_private_flag flag = fmt_if Poly.(flag = Private) "@ private"

let parse_docstring ~loc text =
  let location = loc.Location.loc_start in
  let location =
    { location with
      pos_cnum= location.pos_cnum + 3 (* Length of comment opening *) }
  in
  let parsed = Odoc_parser.parse_comment_raw ~location ~text in
  match parsed with
  | {value; warnings= []} -> Ok value
  | {warnings; _} -> Error (List.map warnings ~f:Odoc_model.Error.to_string)

let fmt_parsed_docstring c ~loc ?pro ~epi str_cmt parsed =
  let space_i i =
    let is_space = function
      | '\t' | '\n' | '\011' | '\012' | '\r' | ' ' -> true
      | _ -> false
    in
    0 <= i && i < String.length str_cmt && is_space str_cmt.[i]
  in
  let fmt_parsed parsed =
    fmt_if (space_i 0) " "
    $ Fmt_odoc.fmt parsed
    $ fmt_if (space_i (String.length str_cmt - 1)) " "
  in
  let fmt_raw str_cmt =
    if c.conf.wrap_comments then fill_text str_cmt else str str_cmt
  in
  let doc =
    match parsed with
    | _ when not c.conf.parse_docstrings -> fmt_raw str_cmt
    | Ok parsed -> fmt_parsed parsed
    | Error msgs ->
        if not c.conf.quiet then
          List.iter msgs
            ~f:
              (Caml.Format.eprintf
                 "Warning: Invalid documentation comment:@,%s\n%!") ;
        fmt_raw str_cmt
  in
  Cmts.fmt c loc
  @@ vbox_if (Option.is_none pro) 0
       (Option.call ~f:pro $ wrap "(**" "*)" doc $ epi)

let docstring_epi ?(standalone = false) ?next ~floating ?epi =
  let epi = if Option.is_some next then fmt "@\n" else Option.call ~f:epi in
  match next with
  | (None | Some (_, false)) when floating && not standalone ->
      fmt "\n" $ epi
  | _ -> epi

let fmt_docstring c ?standalone ?pro ?epi doc =
  list_pn (Option.value ~default:[] doc)
    (fun ?prev:_ ({txt; loc}, floating) ?next ->
      let epi = docstring_epi ?standalone ?next ~floating ?epi in
      fmt_parsed_docstring c ~loc ?pro ~epi txt (parse_docstring ~loc txt))

let fmt_docstring_around_item' ?(force_before = false) ?(fit = false) c doc1
    doc2 =
  match (doc1, doc2) with
  | Some _, Some _ ->
      ( fmt_docstring c ~epi:(fmt "@\n") doc1
      , fmt_docstring c ~pro:(fmt "@\n") doc2 )
  | None, None -> (noop, noop)
  | None, Some doc | Some doc, None -> (
      let is_tag_only =
        List.for_all ~f:(function
          | Ok es, _ -> Fmt_odoc.is_tag_only es
          | _ -> false)
      in
      let fmt_doc ?epi ?pro doc =
        list_pn doc (fun ?prev:_ (parsed, ({txt; loc}, floating)) ?next ->
            let next = Option.map next ~f:snd in
            let epi = docstring_epi ?next ~floating ?epi in
            fmt_parsed_docstring c ~loc ~epi ?pro txt parsed)
      in
      let floating_doc, doc =
        doc
        |> List.map ~f:(fun (({txt; loc}, _) as doc) ->
               (parse_docstring ~loc txt, doc))
        |> List.partition_tf ~f:(fun (_, (_, floating)) -> floating)
      in
      let floating_doc = fmt_doc ~epi:(fmt "@\n") floating_doc in
      let before () = (floating_doc $ fmt_doc ~epi:(fmt "@\n") doc, noop) in
      let after ?(pro = fmt "@\n") () = (floating_doc, fmt_doc ~pro doc) in
      match c.conf with
      | _ when force_before -> before ()
      | {doc_comments_tag_only= `Fit; _} when fit && is_tag_only doc ->
          let pro = break c.conf.doc_comments_padding 0 in
          after ~pro ()
      | {doc_comments= `Before; _} -> before ()
      | {doc_comments= `After; _} -> after () )

(** Formats docstrings and decides where to place them Handles the
    [doc-comments] and [doc-comment-tag-only] options Returns the tuple
    [doc_before, doc_after, attrs] *)
let fmt_docstring_around_item ?force_before ?fit c attrs =
  let doc1, attrs = doc_atrs attrs in
  let doc2, attrs = doc_atrs attrs in
  let doc_before, doc_after =
    fmt_docstring_around_item' ?force_before ?fit c doc1 doc2
  in
  (doc_before, doc_after, attrs)

let fmt_extension_suffix c ext =
  opt ext (fun name -> str "%" $ fmt_str_loc c name)

let field_alias ~field:(li1 : Longident.t) (li2 : Longident.t) =
  match (li1, li2) with
  | (Ldot (_, x) | Lident x), Lident y -> String.equal x y
  | _ -> false

let is_arrow_or_poly = function
  | {ptyp_desc= Ptyp_arrow _ | Ptyp_poly _; _} -> true
  | _ -> false

let fmt_assign_colon c =
  match c.conf.assignment_operator with
  | `Begin_line -> fmt "@;<1 2>:= "
  | `End_line -> fmt " :=@;<1 2>"

let fmt_assign_arrow c =
  match c.conf.assignment_operator with
  | `Begin_line -> fmt "@;<1 2><- "
  | `End_line -> fmt " <-@;<1 2>"

let fmt_docstring_padded c doc =
  fmt_docstring c ~pro:(break c.conf.doc_comments_padding 0) doc

let rec fmt_extension c ctx key (ext, pld) =
  match (pld, ctx) with
  | ( PStr [({pstr_desc= Pstr_value _ | Pstr_type _; _} as si)]
    , (Pld _ | Str _ | Top) ) ->
      fmt_structure_item c ~last:true ~ext (sub_str ~ctx si)
  | PSig [({psig_desc= Psig_type _; _} as si)], (Pld _ | Sig _ | Top) ->
      fmt_signature_item c ~ext (sub_sig ~ctx si)
  | _ -> fmt_attribute_or_extension c key Fn.id (ext, pld)

and fmt_attribute_or_extension c key maybe_box (pre, pld) =
  let cmts_last =
    match pld with
    | PStr [] -> Cmts.fmt_after c pre.loc
    | PStr [{pstr_desc= Pstr_eval ({pexp_loc; _}, []); pstr_loc; _}] ->
        Cmts.fmt_after c pexp_loc $ Cmts.fmt_after c pstr_loc
    | _ -> noop
  in
  let protect_token =
    match pld with PTyp t -> exposed_right_typ t | _ -> false
  in
  let cmts_before = Cmts.fmt_before c pre.loc in
  cmts_before
  $ maybe_box
      (wrap "[" "]"
         ( str key $ fmt_str_loc c pre
         $ fmt_payload c (Pld pld) pld
         $ fmt_if protect_token " " ))
  $ cmts_last

and fmt_attributes c ?(pre = noop) ?(suf = noop) ~key attrs =
  let fmt_attribute c pre = function
    | ( {txt= ("ocaml.doc" | "ocaml.text") as txt; loc= {loc_ghost= true; _}}
      , PStr
          [ { pstr_desc=
                Pstr_eval
                  ( { pexp_desc= Pexp_constant (Pconst_string (doc, None))
                    ; pexp_attributes= []
                    ; _ }
                  , [] )
            ; _ } ] ) ->
        fmt_or (String.equal txt "ocaml.text") "@ " " "
        $ wrap "(**" "*)" (str doc)
    | name, pld ->
        let indent =
          match pld with
          | (PStr _ | PSig _) when String.equal pre "@@@" ->
              c.conf.stritem_extension_indent
          | _ -> c.conf.extension_indent
        in
        fmt_attribute_or_extension c pre (hvbox indent) (name, pld)
  in
  let num = List.length attrs in
  let fmt_attr ~first ~last {attr_name; attr_payload; attr_loc} =
    fmt_or_k first (open_hvbox 0) (fmt "@ ")
    $ Cmts.fmt c attr_loc (fmt_attribute c key (attr_name, attr_payload))
    $ fmt_if_k last (close_box $ suf)
  in
  fmt_if_k (num > 0) (pre $ hvbox_if (num > 1) 0 (list_fl attrs fmt_attr))

and fmt_payload c ctx pld =
  protect (Pld pld)
  @@
  match pld with
  | PStr mex ->
      fmt_if (not (List.is_empty mex)) "@ " $ fmt_structure c ctx mex
  | PSig mty -> fmt ":@ " $ fmt_signature c ctx mty
  | PTyp typ -> fmt ":@ " $ fmt_core_type c (sub_typ ~ctx typ)
  | PPat (pat, exp) ->
      let fmt_when exp =
        str " when " $ fmt_expression c (sub_exp ~ctx exp)
      in
      fmt "?@ " $ fmt_pattern c (sub_pat ~ctx pat) $ opt exp fmt_when

and fmt_record_field c ?typ ?rhs ?(type_first = false) lid1 =
  let fmt_type ?(parens = false) t =
    str ": " $ fmt_core_type c t $ fmt_if parens ")"
  in
  let fmt_rhs ?(parens = false) r =
    fmt "=@;<1 2>" $ fmt_if parens "(" $ cbox 0 r
  in
  let field_space =
    match c.conf.field_space with
    | `Loose | `Tight_decl -> str " "
    | `Tight -> noop
  in
  let fmt_type_rhs =
    match (typ, rhs) with
    | Some t, Some r ->
        if type_first then field_space $ fmt_type t $ fmt "@ " $ fmt_rhs r
        else
          field_space $ fmt_rhs ~parens:true r $ fmt " "
          $ fmt_type ~parens:true t
    | Some t, None -> field_space $ fmt_type t
    | None, Some r -> field_space $ fmt_rhs r
    | None, None -> noop
  in
  Cmts.fmt_before c lid1.loc
  $ cbox 0
      (fmt_longident_loc c lid1 $ Cmts.fmt_after c lid1.loc $ fmt_type_rhs)

and fmt_core_type c ?(box = true) ?(in_type_declaration = false) ?pro
    ?(pro_space = true) ({ast= typ; _} as xtyp) =
  protect (Typ typ)
  @@
  let {ptyp_desc; ptyp_attributes; ptyp_loc; _} = typ in
  update_config_maybe_disabled c ptyp_loc ptyp_attributes
  @@ fun c ->
  ( match (ptyp_desc, pro) with
  | (Ptyp_arrow _ | Ptyp_poly _), Some pro when c.conf.ocp_indent_compat ->
      fmt_if pro_space "@;" $ str pro $ str " "
  | _, Some pro when c.conf.ocp_indent_compat ->
      fmt_if pro_space "@ " $ str pro $ str " "
  | _, Some pro -> fmt_if pro_space " " $ str pro $ fmt "@ "
  | _ -> noop )
  $
  let doc, atrs = doc_atrs ptyp_attributes in
  Cmts.fmt c ptyp_loc
  @@ ( if List.is_empty atrs then Fn.id
     else fun k -> wrap "(" ")" (k $ fmt_attributes c ~key:"@" atrs) )
  @@
  let parens = parenze_typ xtyp in
  ( hvbox_if box 0
  @@ wrap_if
       (match typ.ptyp_desc with Ptyp_tuple _ -> false | _ -> parens)
       "(" ")"
  @@
  let ctx = Typ typ in
  match ptyp_desc with
  | Ptyp_alias (typ, txt) ->
      hvbox 0 (fmt_core_type c (sub_typ ~ctx typ) $ fmt "@ as@ '" $ str txt)
  | Ptyp_any -> str "_"
  | Ptyp_arrow _ ->
      let arg_label lbl =
        match lbl with
        | Nolabel -> noop
        | Labelled l -> str l $ str ":" $ fmt "@,"
        | Optional l -> str "?" $ str l $ str ":" $ fmt "@,"
      in
      let xt1N = Sugar.arrow_typ c.cmts xtyp in
      let indent =
        if Poly.(c.conf.break_separators = `Before) then 2 else 0
      in
      ( match pro with
      | Some pro when c.conf.ocp_indent_compat ->
          fits_breaks ""
            (String.make (Int.max 1 (indent - String.length pro)) ' ')
      | _ ->
          fmt_if_k
            Poly.(c.conf.break_separators = `Before)
            (fmt_or_k c.conf.ocp_indent_compat (fits_breaks "" "")
               (fits_breaks "" "   ")) )
      $ list xt1N
          ( if Poly.(c.conf.break_separators = `Before) then
            if parens then "@;<1 1>-> " else "@ -> "
          else " ->@;<1 0>" )
          (fun (lI, xtI) ->
            hvbox_if
              Poly.(lI <> Nolabel)
              2
              (arg_label lI $ hvbox 0 (fmt_core_type c xtI)))
  | Ptyp_constr (lid, []) -> fmt_longident_loc c lid
  | Ptyp_constr (lid, [t1]) ->
      fmt_core_type c (sub_typ ~ctx t1) $ fmt "@ " $ fmt_longident_loc c lid
  | Ptyp_constr (lid, t1N) ->
      wrap_fits_breaks c.conf "(" ")"
        (list t1N (comma_sep c) (sub_typ ~ctx >> fmt_core_type c))
      $ fmt "@ " $ fmt_longident_loc c lid
  | Ptyp_extension ext ->
      hvbox c.conf.extension_indent (fmt_extension c ctx "%" ext)
  | Ptyp_package (id, cnstrs) ->
      hvbox 2
        ( hovbox 0 (fmt "module@ " $ fmt_longident_loc c id)
        $ fmt_package_type c ctx cnstrs )
  | Ptyp_poly ([], _) ->
      impossible "produced by the parser, handled elsewhere"
  | Ptyp_poly (a1N, t) ->
      hovbox_if box 0
        ( list a1N "@ " (fmt_str_loc c ~pre:(str "'"))
        $ fmt ".@ "
        $ fmt_core_type c ~box:false (sub_typ ~ctx t) )
  | Ptyp_tuple typs ->
      hvbox 0
        (wrap_fits_breaks_if ~space:false c.conf parens "(" ")"
           (list typs "@ * " (sub_typ ~ctx >> fmt_core_type c)))
  | Ptyp_var s ->
      str "'"
      (* [' a'] is a valid type variable, the space is required to not lex
         as a char. https://github.com/ocaml/ocaml/pull/2034 *)
      $ fmt_if (String.length s > 1 && Char.equal s.[1] '\'') " "
      $ str s
  | Ptyp_variant (rfs, flag, lbls) ->
      let row_fields rfs =
        match rfs with
        | [] -> Cmts.fmt_within c ~pro:noop ptyp_loc
        | _ ->
            let max acc r =
              match r.prf_desc with
              | Rtag (name, _, _) ->
                  Option.map acc ~f:(max (String.length name.txt))
              | Rinherit _ -> None
            in
            let max_len_name = List.fold_left rfs ~init:(Some 0) ~f:max in
            list rfs
              ( if in_type_declaration && Poly.(c.conf.type_decl = `Sparse)
              then "@;<1000 0>| "
              else "@ | " )
              (fmt_row_field c ~max_len_name ctx)
      in
      let protect_token =
        match List.last rfs with
        | None -> false
        | Some {prf_desc= Rinherit _; _} -> false
        | Some {prf_desc= Rtag (_, _, l); _} -> (
          match List.last l with
          | None -> false
          | Some x -> exposed_right_typ x )
      in
      let space_around = c.conf.space_around_variants in
      let closing =
        let empty = List.is_empty rfs in
        let breaks = Poly.(c.conf.type_decl = `Sparse) && space_around in
        let nspaces = if breaks then 1000 else 1 in
        let space = (protect_token || space_around) && not empty in
        fits_breaks (if space then " ]" else "]") ~hint:(nspaces, 0) "]"
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
            $ list ls "@ " (str "`" >$ str)
            $ closing
        | Open, Some _, _ -> impossible "not produced by parser" )
  | Ptyp_object ([], o_c) ->
      wrap "<@ " ">"
        ( fmt_if Poly.(o_c = Open) "..@ "
        $ Cmts.fmt_within c ~pro:noop ~epi:(str " ") ptyp_loc )
  | Ptyp_object (fields, closedness) ->
      let fmt_field {pof_desc; pof_attributes= atrs; pof_loc} =
        let doc, atrs = doc_atrs atrs in
        let fmt_field =
          match pof_desc with
          | Otag (lab_loc, typ) ->
              (* label loc * attributes * core_type -> object_field *)
              let field_loose =
                match c.conf.field_space with
                | `Loose | `Tight_decl -> true
                | `Tight -> false
              in
              fmt_str_loc c lab_loc $ fmt_if field_loose " " $ fmt ":@ "
              $ fmt_core_type c (sub_typ ~ctx typ)
          | Oinherit typ -> fmt_core_type c (sub_typ ~ctx typ)
        in
        Cmts.fmt c pof_loc
        @@ hvbox 4
             ( hvbox 2 fmt_field
             $ fmt_docstring_padded c doc
             $ fmt_attributes c ~pre:(str " ") ~key:"@" atrs )
      in
      hvbox 0
        (wrap "< " " >"
           ( list fields "@ ; " fmt_field
           $ fmt_if Poly.(closedness = Open) "@ ; .." ))
  | Ptyp_class (lid, []) -> fmt_longident_loc c ~pre:(str "#") lid
  | Ptyp_class (lid, [t1]) ->
      fmt_core_type c (sub_typ ~ctx t1)
      $ fmt "@ "
      $ fmt_longident_loc c ~pre:(str "#") lid
  | Ptyp_class (lid, t1N) ->
      wrap_fits_breaks c.conf "(" ")"
        (list t1N (comma_sep c) (sub_typ ~ctx >> fmt_core_type c))
      $ fmt "@ "
      $ fmt_longident_loc c ~pre:(str "#") lid )
  $ fmt_docstring c ~pro:(fmt "@ ") doc

and fmt_package_type c ctx cnstrs =
  let fmt_cstr ~first ~last:_ (lid, typ) =
    fmt_or first "@;<1 0>with type " "@;<1 1>and type "
    $ fmt_longident_loc c lid $ str " = "
    $ fmt_core_type c (sub_typ ~ctx typ)
  in
  list_fl cnstrs fmt_cstr

and fmt_row_field c ctx {prf_desc; prf_attributes= atrs; prf_loc}
    ~max_len_name =
  let c = update_config c atrs in
  let doc, atrs = doc_atrs atrs in
  let row =
    match prf_desc with
    | Rtag (name, const, typs) ->
        let fmt_padding =
          match max_len_name with
          | Some max_len ->
              let pad =
                String.make (max_len - String.length name.txt) ' '
              in
              fmt_if_k
                ( c.conf.align_variants_decl
                && (not (List.is_empty typs))
                && not (Cmts.has_after c.cmts name.loc) )
                (fmt_or_k
                   Poly.(c.conf.type_decl = `Sparse)
                   (str pad)
                   (fits_breaks ~level:2 "" pad))
          | None -> noop
        in
        fmt_str_loc c ~pre:(str "`") name
        $ fmt_padding
        $ fmt_if (not (const && List.is_empty typs)) " of@ "
        $ fmt_if (const && not (List.is_empty typs)) " & "
        $ list typs "@ & " (sub_typ ~ctx >> fmt_core_type c)
    | Rinherit typ -> fmt_core_type c (sub_typ ~ctx typ)
  in
  hvbox 0
    ( hvbox 0 (Cmts.fmt c prf_loc row)
    $ fmt_attributes c ~key:"@" atrs
    $ fmt_docstring_padded c doc )

and fmt_pattern c ?pro ?parens ({ctx= ctx0; ast= pat} as xpat) =
  protect (Pat pat)
  @@
  let ctx = Pat pat in
  let {ppat_desc; ppat_attributes; ppat_loc; ppat_loc_stack} = pat in
  update_config_maybe_disabled c ppat_loc ppat_attributes
  @@ fun c ->
  let parens = match parens with Some b -> b | None -> parenze_pat xpat in
  let spc = break_unless_newline 1 0 in
  ( match ppat_desc with
  | Ppat_or _ -> Fn.id
  | Ppat_construct ({txt; loc}, _) when Poly.(txt <> Longident.Lident "::")
    ->
      fun k ->
        Cmts.fmt c ~pro:spc ppat_loc
        @@ Cmts.fmt c ~pro:spc loc (Option.call ~f:pro $ k)
  | _ -> fun k -> Cmts.fmt c ppat_loc (Option.call ~f:pro $ k) )
  @@ ( if List.is_empty ppat_attributes then Fn.id
     else
       let maybe_wrap =
         match ppat_desc with Ppat_or _ -> Fn.id | _ -> wrap "(" ")"
       in
       fun k -> maybe_wrap (k $ fmt_attributes c ~key:"@" ppat_attributes)
     )
  @@
  match ppat_desc with
  | Ppat_any -> str "_"
  | Ppat_var {txt; loc} ->
      Cmts.fmt c loc @@ wrap_if (is_symbol_id txt) "( " " )" (str txt)
  | Ppat_alias (pat, {txt; loc}) ->
      let paren_pat =
        match pat.ppat_desc with
        | Ppat_or _ | Ppat_tuple _ -> Some true
        | _ -> None
      in
      hovbox 0
        (wrap_fits_breaks_if ~space:false c.conf parens "(" ")"
           ( fmt_pattern c ?parens:paren_pat (sub_pat ~ctx pat)
           $ fmt "@ as@ "
           $ Cmts.fmt c loc (wrap_if (is_symbol_id txt) "( " " )" (str txt))
           ))
  | Ppat_constant const ->
      fmt_constant c ~loc:(smallest_loc ppat_loc ppat_loc_stack) const
  | Ppat_interval (l, u) -> (
      (* we need to reconstruct locations for both side of the interval *)
      let toks =
        Source.tokens_at c.source ppat_loc ~filter:(function
          | Parser.CHAR _ | Parser.DOTDOT
           |Parser.(INT _ | STRING _ | FLOAT _) ->
              true
          | _ -> false)
      in
      match toks with
      | [ (Parser.(CHAR _ | INT _ | STRING _ | FLOAT _), loc1)
        ; (Parser.DOTDOT, _)
        ; (Parser.(CHAR _ | INT _ | STRING _ | FLOAT _), loc2) ] ->
          fmt_constant ~loc:loc1 c l
          $ str " .. "
          $ fmt_constant ~loc:loc2 c u
      | _ ->
          impossible
            "Ppat_interval is only produced by the sequence of 3 tokens: \
             CONSTANT-DOTDOT-CONSTANT " )
  | Ppat_tuple pats ->
      let parens =
        parens || Poly.(c.conf.parens_tuple_patterns = `Always)
      in
      hvbox 0
        (Params.wrap_tuple ~parens ~no_parens_if_break:false c.conf
           (list pats (comma_sep c) (sub_pat ~ctx >> fmt_pattern c)))
  | Ppat_construct ({txt= Lident (("()" | "[]") as txt); loc}, None) ->
      let opn = txt.[0] and cls = txt.[1] in
      Cmts.fmt c loc
        (hvbox 0
           (wrap_k (char opn) (char cls)
              (Cmts.fmt_within c ~pro:(str " ") ~epi:(str " ") ppat_loc)))
  | Ppat_construct (lid, None) -> fmt_longident_loc c lid
  | Ppat_construct
      ( {txt= Lident "::"; loc}
      , Some {ppat_desc= Ppat_tuple [x; y]; ppat_attributes= []; ppat_loc; _}
      ) -> (
    match Sugar.list_pat c.cmts pat with
    | Some (loc_xpats, nil_loc) ->
        let p = Params.get_list_pat c.conf ~ctx:ctx0 in
        let fmt_pat ~first ~last (locs, xpat) =
          fmt_if_k (not first) p.sep_before
          $ Cmts.fmt_list c ~eol:(fmt "@;<1 2>") locs (fmt_pattern c xpat)
          $ fmt_or_k last p.sep_after_final p.sep_after_non_final
        in
        hvbox 0
          (Cmts.fmt c ppat_loc
             (p.box
                ( list_fl loc_xpats fmt_pat
                $ Cmts.fmt_before c ~pro:(fmt "@;<1 2>") ~epi:noop nil_loc
                $ Cmts.fmt_after c ~pro:(fmt "@ ") ~epi:noop nil_loc )))
    | None ->
        hvbox 0
          (wrap_if parens "(" ")"
             (Cmts.fmt c ppat_loc
                ( fmt_pattern c (sub_pat ~ctx x)
                $ Cmts.fmt c ~pro:(fmt "@ ") ~epi:noop loc
                    (break_unless_newline 1 0 $ str ":: ")
                $ fmt_pattern c (sub_pat ~ctx y) ))) )
  | Ppat_construct (lid, Some pat) ->
      cbox 2
        (wrap_if parens "(" ")"
           ( fmt_longident_loc c lid $ fmt "@ "
           $ fmt_pattern c (sub_pat ~ctx pat) ))
  | Ppat_variant (lbl, None) -> str "`" $ str lbl
  | Ppat_variant (lbl, Some pat) ->
      cbox 2
        (wrap_if parens "(" ")"
           (str "`" $ str lbl $ fmt "@ " $ fmt_pattern c (sub_pat ~ctx pat)))
  | Ppat_record (flds, closed_flag) ->
      let fmt_field (lid1, pat) =
        let {ppat_desc; ppat_loc; ppat_attributes; _} = pat in
        let fmt_rhs ~ctx p = fmt_pattern c (sub_pat ~ctx p) in
        hvbox 0
          ( Cmts.fmt c ppat_loc
          @@
          match ppat_desc with
          | Ppat_var {txt= txt'; _}
            when field_alias ~field:lid1.txt (Longident.parse txt')
                 && List.is_empty ppat_attributes ->
              fmt_record_field c lid1
          | Ppat_constraint ({ppat_desc= Ppat_var {txt; _}; ppat_loc; _}, t)
            when field_alias ~field:lid1.txt (Longident.parse txt)
                 && List.is_empty ppat_attributes ->
              let typ = sub_typ ~ctx:(Pat pat) t in
              Cmts.fmt c ppat_loc @@ fmt_record_field c ~typ lid1
          | Ppat_constraint ({ppat_desc= Ppat_unpack _; ppat_loc; _}, _) ->
              Cmts.fmt c ppat_loc
              @@ fmt_record_field c ~rhs:(fmt_rhs ~ctx pat) lid1
          | Ppat_constraint (p, t) when List.is_empty ppat_attributes ->
              let typ = sub_typ ~ctx:(Pat pat) t
              and rhs = fmt_rhs ~ctx:(Pat pat) p in
              let type_first =
                Poly.(`Type_first = Source.typed_pattern t p)
              in
              Cmts.fmt c p.ppat_loc
              @@ fmt_record_field c ~typ ~rhs ~type_first lid1
          | _ -> fmt_record_field c ~rhs:(fmt_rhs ~ctx pat) lid1 )
      in
      let p1, p2 = Params.get_record_pat c.conf ~ctx:ctx0 in
      let fmt_field ~first ~last x =
        fmt_if_k (not first) p1.sep_before
        $ fmt_field x
        $ fmt_or_k
            (last && Poly.(closed_flag = Closed))
            p1.sep_after_final p1.sep_after_non_final
      in
      hvbox 0
        (wrap_if parens "(" ")"
           (p1.box
              ( list_fl flds fmt_field
              $ fmt_if_k Poly.(closed_flag = Open) p2.wildcard )))
  | Ppat_array [] ->
      hvbox 0
        (wrap_fits_breaks c.conf "[|" "|]" (Cmts.fmt_within c ppat_loc))
  | Ppat_array pats ->
      let p = Params.get_array_pat c.conf ~ctx:ctx0 in
      let fmt_pat ~first ~last pat =
        fmt_if_k (not first) p.sep_before
        $ fmt_pattern c (sub_pat ~ctx pat)
        $ fmt_or_k last p.sep_after_final p.sep_after_non_final
      in
      hvbox 0 (p.box (list_fl pats fmt_pat))
  | Ppat_or _ ->
      let has_doc = not (List.is_empty xpat.ast.ppat_attributes) in
      let nested =
        match ctx0 with
        | Pat {ppat_desc= Ppat_or _; _} -> not has_doc
        | Exp {pexp_desc= Pexp_match _ | Pexp_try _ | Pexp_function _; _} ->
            not has_doc
        | _ -> false
      in
      let xpats = Sugar.or_pat c.cmts xpat in
      let space p =
        match p.ppat_desc with
        | Ppat_constant (Pconst_integer (i, _) | Pconst_float (i, _)) -> (
          match i.[0] with '-' | '+' -> true | _ -> false )
        | _ -> false
      in
      let pro0 =
        Option.call ~f:pro
        $ fits_breaks
            (if parens then "(" else "")
            (if nested then "" else "( ")
      in
      let proI ?(space = false) =
        match ctx0 with
        | Exp {pexp_desc= Pexp_function _ | Pexp_match _ | Pexp_try _; _}
          when Poly.(c.conf.break_cases <> `Nested) -> (
            fmt_if Poly.(c.conf.break_cases = `All) "@;<1000 0>"
            $
            match c.conf.indicate_nested_or_patterns with
            | `Space when space -> or_newline "| " " | "
            | `Space -> or_newline "| " " |"
            | `Unsafe_no -> or_newline "| " "| " )
        | _ -> break_unless_newline 1 0 $ str "| "
      in
      let is_simple {ppat_desc; _} =
        match ppat_desc with
        | Ppat_any | Ppat_constant _ | Ppat_var _
         |Ppat_variant (_, (None | Some {ppat_desc= Ppat_any; _}))
         |Ppat_construct (_, (None | Some {ppat_desc= Ppat_any; _})) ->
            true
        | _ -> false
      in
      let break {ast= p1; _} {ast= p2; _} =
        Poly.(c.conf.break_cases = `Nested)
        || (not (is_simple p1))
        || not (is_simple p2)
      in
      hvbox 0
        ( list_fl (List.group xpats ~break)
            (fun ~first:first_grp ~last:_ xpat_grp ->
              list_fl xpat_grp (fun ~first ~last xpat ->
                  let open_box =
                    if Poly.(c.conf.break_cases = `Fit_or_vertical) then
                      open_hvbox
                    else open_hovbox
                  in
                  let pro =
                    if first_grp && first then pro0 $ open_box (-2)
                    else if first then proI $ open_box (-2)
                    else proI ~space:(space xpat.ast)
                  in
                  (* side effects of Cmts.fmt_before before [fmt_pattern] is
                     important *)
                  let leading_cmt =
                    let loc = xpat.ast.ppat_loc in
                    if Cmts.has_before c.cmts loc then
                      let loc_before = Cmts.fmt_before c loc in
                      fmt "@;<1000 0>" $ loc_before
                    else noop
                  in
                  leading_cmt $ fmt_pattern c ~pro xpat
                  $ fmt_if_k last close_box))
        $ fmt_or_k nested
            (fits_breaks (if parens then ")" else "") "")
            (fits_breaks (if parens then ")" else "") ~hint:(1, 2) ")") )
  | Ppat_constraint
      ( {ppat_desc= Ppat_unpack name; ppat_attributes= []; ppat_loc; _}
      , ( { ptyp_desc= Ptyp_package (id, cnstrs)
          ; ptyp_attributes= []
          ; ptyp_loc= (* TODO: use ptyp_loc *) _
          ; _ } as typ ) ) ->
      let ctx = Typ typ in
      hovbox 0
        (wrap_if parens "(" ")"
           (hvbox 1
              (Cmts.fmt c typ.ptyp_loc
                 ( hovbox 0
                     ( Cmts.fmt c ppat_loc
                         (str "module " $ fmt_str_loc c name)
                     $ fmt "@ : " $ fmt_longident_loc c id )
                 $ fmt_package_type c ctx cnstrs ))))
  | Ppat_constraint (pat, typ) ->
      hvbox 2
        (wrap_if parens "(" ")"
           ( fmt_pattern c (sub_pat ~ctx pat)
           $ ( match ctx0 with
             | Exp {pexp_desc= Pexp_let _; _} -> fmt "@ : "
             | _ -> fmt " :@ " )
           $ fmt_core_type c (sub_typ ~ctx typ) ))
  | Ppat_type lid -> fmt_longident_loc c ~pre:(str "#") lid
  | Ppat_lazy pat ->
      cbox 2
        (wrap_if parens "(" ")"
           (fmt "lazy@ " $ fmt_pattern c (sub_pat ~ctx pat)))
  | Ppat_unpack name ->
      wrap_fits_breaks_if ~space:false c.conf parens "(" ")"
        (fmt "module@ " $ fmt_str_loc c name)
  | Ppat_exception pat ->
      cbox 2
        (wrap_if parens "(" ")"
           (fmt "exception@ " $ fmt_pattern c (sub_pat ~ctx pat)))
  | Ppat_extension ext ->
      hvbox c.conf.extension_indent (fmt_extension c ctx "%" ext)
  | Ppat_open (lid, pat) ->
      let can_skip_parens =
        match pat.ppat_desc with
        | Ppat_array _ | Ppat_record _ -> true
        | Ppat_tuple _ -> Poly.(c.conf.parens_tuple_patterns = `Always)
        | _ -> Option.is_some (Sugar.list_pat c.cmts pat)
      in
      let opn, cls = if can_skip_parens then (".", "") else (".(", ")") in
      cbox 0
        ( fmt_longident_loc c lid
        $ wrap_k (str opn) (str cls)
            (fmt "@;<0 2>" $ fmt_pattern c (sub_pat ~ctx pat)) )

and fmt_fun_args c ?(pro = noop) args =
  let fmt_fun_arg (a : Sugar.arg_kind) =
    match a with
    | Val
        ( ((Labelled l | Optional l) as lbl)
        , ( { ast=
                { ppat_desc=
                    ( Ppat_var {txt; loc= _}
                    | Ppat_constraint
                        ( { ppat_desc= Ppat_var {txt; loc= _}
                          ; ppat_attributes= []
                          ; ppat_loc= _
                          ; _ }
                        , _ ) )
                ; ppat_attributes= []
                ; ppat_loc= _
                ; _ }
            ; _ } as xpat )
        , None )
      when String.equal l txt ->
        let symbol = match lbl with Labelled _ -> "~" | _ -> "?" in
        cbox 0 (str symbol $ fmt_pattern c xpat)
    | Val (lbl, xpat, None) ->
        cbox 0 (fmt_label lbl ":" $ fmt_pattern c xpat)
    | Val
        ( Optional l
        , ( { ast=
                { ppat_desc= Ppat_var {txt; loc= _}
                ; ppat_attributes= []
                ; ppat_loc= _
                ; _ }
            ; _ } as xpat )
        , Some xexp )
      when String.equal l txt ->
        cbox 0
          (wrap "?(" ")"
             ( fmt_pattern c xpat $ fmt " =@;<1 2>"
             $ hovbox 2 (fmt_expression c xexp) ))
    | Val
        ( Optional l
        , ( { ast=
                { ppat_desc=
                    Ppat_constraint
                      ( { ppat_desc= Ppat_var {txt; loc= _}
                        ; ppat_loc= _
                        ; ppat_attributes= _
                        ; _ }
                      , _ )
                ; ppat_attributes= []
                ; ppat_loc= _
                ; _ }
            ; _ } as xpat )
        , Some xexp )
      when String.equal l txt ->
        cbox 0
          (wrap "?(" ")"
             ( fmt_pattern c ~parens:false xpat
             $ fmt " =@;<1 2>" $ fmt_expression c xexp ))
    | Val (Optional l, xpat, Some xexp) ->
        cbox 0
          ( str "?" $ str l
          $ wrap ":(" ")"
              ( fmt_pattern c ~parens:false xpat
              $ fmt " =@;<1 2>" $ fmt_expression c xexp ) )
    | Val ((Labelled _ | Nolabel), _, Some _) ->
        impossible "not accepted by parser"
    | Newtypes [] -> impossible "not accepted by parser"
    | Newtypes names ->
        cbox 0
          (wrap "(" ")" (str "type " $ list names "@ " (fmt_str_loc c)))
  in
  fmt_if_k (not (List.is_empty args)) (pro $ list args "@;" fmt_fun_arg)

(** The second returned value of [fmt_body] belongs to a box of level N-1 if
    the first returned value belongs to a box of level N. *)
and fmt_body c ?ext ({ast= body; _} as xbody) =
  let ctx = Exp body in
  let parens = parenze_exp xbody in
  match body with
  | {pexp_desc= Pexp_function cs; pexp_attributes; pexp_loc; _} ->
      ( ( update_config_maybe_disabled c pexp_loc pexp_attributes
        @@ fun c ->
        fmt "@ "
        $ Cmts.fmt_before c pexp_loc
        $ fmt_if parens "(" $ str "function"
        $ fmt_extension_suffix c ext
        $ fmt_attributes c ~key:"@" pexp_attributes )
      , update_config_maybe_disabled c pexp_loc pexp_attributes
        @@ fun c ->
        fmt_cases c ctx cs $ fmt_if parens ")" $ Cmts.fmt_after c pexp_loc
      )
  | _ -> (noop, fmt_expression c ~eol:(fmt "@;<1000 0>") xbody)

and fmt_index_op c ctx ~parens ?set {txt= s, opn, cls; loc} l is =
  wrap_if parens "(" ")"
    (hovbox 0
       ( fmt_expression c (sub_exp ~ctx l)
       $ Cmts.fmt_before c loc
       $ str (Printf.sprintf "%s%c" s opn)
       $ Cmts.fmt_after c loc
       $ list is (comma_sep c) (fun i -> fmt_expression c (sub_exp ~ctx i))
       $ str (Printf.sprintf "%c" cls)
       $ opt set (fun e ->
             fmt_assign_arrow c $ fmt_expression c (sub_exp ~ctx e)) ))

and fmt_label_arg ?(box = true) ?epi ?parens ?eol c
    (lbl, ({ast= arg; _} as xarg)) =
  match (lbl, arg.pexp_desc) with
  | (Labelled l | Optional l), Pexp_ident {txt= Lident i; loc}
    when String.equal l i && List.is_empty arg.pexp_attributes ->
      Cmts.fmt c loc @@ Cmts.fmt c ?eol arg.pexp_loc @@ fmt_label lbl ""
  | _ ->
      hvbox_if box 2
        (fmt_label lbl ":@," $ fmt_expression c ~box ?epi ?parens xarg)

and fmt_args ~first:first_grp ~last:last_grp c ctx args =
  let fmt_arg ?prev (lbl, arg) ?next =
    let ({ast; _} as xarg) = sub_exp ~ctx arg in
    let openbox = open_hovbox (if first_grp then 2 else 0) in
    let consecutive_prefix_ops =
      match next with
      | Some (_, e) -> is_prefix ast && exposed_left_exp e
      | _ -> false
    in
    let spc =
      consecutive_prefix_ops || Option.is_some next || not last_grp
    in
    let box =
      match ast.pexp_desc with
      | Pexp_fun _ | Pexp_function _ -> Some false
      | _ -> None
    in
    let epi =
      match (lbl, next) with
      | _, None -> None
      | Nolabel, _ -> Some (fits_breaks "" ~hint:(1000, -1) "")
      | _ -> Some (fits_breaks "" ~hint:(1000, -3) "")
    in
    fmt_if_k (Option.is_none prev) openbox
    $ hovbox 2 (fmt_label_arg c ?box ?epi (lbl, xarg))
    $ fmt_if_k (Option.is_none next) close_box
    $ fmt_if_k spc (break_unless_newline 1 0)
  in
  list_pn args fmt_arg

and fmt_sequence c ?ext parens width xexp pexp_loc fmt_atrs =
  let fmt_sep ?(force_break = false) xe1 ext xe2 =
    let blank_line =
      match c.conf.sequence_blank_line with
      | `Preserve_one ->
          let l1 = xe1.ast.pexp_loc.loc_end.pos_lnum in
          let l2 = xe2.ast.pexp_loc.loc_start.pos_lnum in
          l2 - l1 > 1
      | `Compact -> false
    in
    let break =
      if blank_line then fmt "\n@;<1000 0>"
      else if c.conf.break_sequences || force_break then fmt "@;<1000 0>"
      else if parens && Poly.(c.conf.sequence_style = `Before) then
        fmt "@;<1 -2>"
      else fmt "@;<1 0>"
    in
    match c.conf.sequence_style with
    | `Before ->
        break $ str ";"
        $ fmt_extension_suffix c ext
        $ fmt_or_k (Option.is_some ext)
            (fmt_or parens "@ " "@;<1 2>")
            (str " ")
    | `Separator -> str " ;" $ fmt_extension_suffix c ext $ break
    | `Terminator -> str ";" $ fmt_extension_suffix c ext $ break
  in
  let is_simple x = is_simple c.conf width x in
  let break (_, xexp1) (_, xexp2) =
    not (is_simple xexp1 && is_simple xexp2)
  in
  let elts = Sugar.sequence c.conf c.cmts xexp in
  ( match elts with
  | (None, _) :: (first_ext, _) :: _ ->
      let compare {txt= x; _} {txt= y; _} = String.compare x y in
      assert (Option.compare compare first_ext ext = 0)
  | _ -> impossible "at least two elements" ) ;
  let grps = List.group elts ~break in
  let fmt_seq ?prev (ext, curr) ?next:_ =
    let f (_, prev) = fmt_sep prev ext curr in
    Option.value_map prev ~default:noop ~f $ fmt_expression c curr
  in
  let fmt_seq_list ?prev x ?next:_ =
    let f prev =
      let prev = snd (List.last_exn prev) in
      let ext, curr = List.hd_exn x in
      fmt_sep ~force_break:true prev ext curr
    in
    Option.value_map prev ~default:noop ~f $ list_pn x fmt_seq
  in
  hvbox 0
    ( wrap_fits_breaks_exp_if ~space:false c ~loc:pexp_loc ~parens
        (hvbox_if parens 0 @@ list_pn grps fmt_seq_list)
    $ fmt_atrs )

and fmt_expression c ?(box = true) ?pro ?epi ?eol ?parens ?(indent_wrap = 0)
    ?ext ({ast= exp; _} as xexp) =
  protect (Exp exp)
  @@
  let {pexp_desc; pexp_loc; pexp_attributes; pexp_loc_stack} = exp in
  update_config_maybe_disabled c pexp_loc pexp_attributes
  @@ fun c ->
  let fmt_cmts = Cmts.fmt c ?eol pexp_loc in
  let fmt_atrs = fmt_attributes c ~pre:(str " ") ~key:"@" pexp_attributes in
  let parens = Option.value parens ~default:(parenze_exp xexp) in
  let width xe = String.length (Cmts.preserve (fmt_expression c) xe) in
  let fmt_op_args op_args =
    let fmt_arg ~last_op ~first:_ ~last lbl_xarg =
      let _, ({ast= arg; _} as xarg) = lbl_xarg in
      let parens =
        ((not last_op) && exposed_right_exp Ast.Non_apply arg)
        || parenze_exp xarg
      in
      let box =
        match (snd lbl_xarg).ast.pexp_desc with
        | Pexp_fun _ | Pexp_function _ -> Some (not last)
        | _ -> None
      in
      fmt_label_arg c ?box ~parens lbl_xarg $ fmt_if (not last) "@ "
    in
    let fmt_args ~last_op xargs = list_fl xargs (fmt_arg ~last_op) in
    let fmt_op_args ~first ~last (fmt_op, xargs) =
      let is_not_indented exp =
        match exp.pexp_desc with
        | Pexp_ifthenelse _ | Pexp_let _ | Pexp_letexception _
         |Pexp_letmodule _ | Pexp_match _ | Pexp_newtype _
         |Pexp_sequence _ | Pexp_try _ ->
            true
        | Pexp_open _ -> (
          match c.conf.let_open with
          | `Auto | `Long -> true
          | `Short -> false
          | `Preserve -> Source.is_long_pexp_open c.source exp )
        | _ -> false
      in
      let final_break =
        match xargs with
        | (_, {ast= a0; _}) :: _ -> last && is_not_indented a0
        | _ -> false
      in
      hvbox 0
        ( fmt_op
        $ (if final_break then fmt "@ " else fmt_if (not first) " ")
        $ hovbox_if (not last) 2 (fmt_args ~last_op:last xargs) )
      $ fmt_if_k (not last) (break 0 0)
    in
    let is_nested_diff_prec_infix_ops =
      let infix_prec ast =
        match ast with
        | Exp {pexp_desc= Pexp_apply (e, _); _} when is_infix e ->
            prec_ast ast
        | Exp
            ( { pexp_desc=
                  Pexp_construct
                    ( {txt= Lident "::"; loc= _}
                    , Some
                        { pexp_desc= Pexp_tuple [_; _]
                        ; pexp_loc= _
                        ; pexp_attributes= _
                        ; _ } )
              ; pexp_loc= _
              ; pexp_attributes= _
              ; _ } as exp )
          when not (is_sugared_list exp) ->
            prec_ast ast
        | _ -> None
      in
      (* Make the precedence explicit for infix operators *)
      match (infix_prec xexp.ctx, infix_prec (Exp xexp.ast)) with
      | Some (InfixOp0 | ColonEqual), _ | _, Some (InfixOp0 | ColonEqual) ->
          (* special case for refs update and all InfixOp0 to reduce parens
             noise *)
          false
      | None, _ | _, None -> false
      | Some p1, Some p2 -> Poly.(p1 <> p2)
    in
    let parens_or_nested = parens || is_nested_diff_prec_infix_ops in
    let parens_or_forced =
      parens || Poly.(c.conf.infix_precedence = `Parens)
    in
    let fmt_op_arg_group ~first:first_grp ~last:last_grp args =
      list_fl args
        (fun ~first ~last (_, fmt_before_cmts, fmt_after_cmts, op_args) ->
          let very_first = first_grp && first in
          let very_last = last_grp && last in
          let hint =
            match c.conf.indicate_multiline_delimiters with
            | `Space -> (1, 0)
            | `No -> (0, 0)
            | `Closing_on_separate_line -> (1000, 0)
          in
          fmt_if_k very_first
            (fits_breaks_if parens_or_nested "("
               ( if parens_or_forced then
                 match c.conf.indicate_multiline_delimiters with
                 | `Space -> "( "
                 | `No | `Closing_on_separate_line -> "("
               else "" ))
          $ fmt_before_cmts
          $ fmt_if_k first
              (open_hovbox (if first_grp && parens then -2 else 0))
          $ fmt_after_cmts
          $ fmt_op_args ~first:very_first op_args ~last:very_last
          $ fmt_if_k last close_box
          $ fmt_or_k very_last
              (fmt_or_k parens_or_forced
                 (fits_breaks_if parens_or_nested ")" ~hint ")")
                 (fits_breaks_if parens_or_nested ")" ""))
              (break_unless_newline 1 0))
    in
    let op_args_grouped =
      match c.conf.break_infix with
      | `Wrap ->
          let not_simple (_, arg) = not (is_simple c.conf width arg) in
          let exists_not_simple args = List.exists args ~f:not_simple in
          let break (has_cmts, _, _, (_, args1)) (_, _, _, (_, args2)) =
            has_cmts || exists_not_simple args1 || exists_not_simple args2
          in
          List.group op_args ~break
      | `Fit_or_vertical -> List.map ~f:(fun x -> [x]) op_args
    in
    hvbox indent_wrap (list_fl op_args_grouped fmt_op_arg_group $ fmt_atrs)
  in
  let ctx = Exp exp in
  let fmt_args_grouped e0 a1N =
    let all = (Nolabel, e0) :: a1N in
    let is_simple x = is_simple c.conf width (sub_exp ~ctx x) in
    let break (_, a1) (_, a2) = not (is_simple a1 && is_simple a2) in
    let groups =
      if c.conf.wrap_fun_args then List.group all ~break
      else List.map all ~f:(fun x -> [x])
    in
    list_fl groups (fmt_args c ctx)
  in
  hvbox_if box 0 @@ fmt_cmts
  @@ (fun fmt -> Option.call ~f:pro $ fmt)
  @@
  match pexp_desc with
  | Pexp_apply (_, []) -> impossible "not produced by parser"
  | Pexp_apply
      ( { pexp_desc= Pexp_ident {txt= Lident "|>"; loc}
        ; pexp_attributes= []
        ; pexp_loc= _
        ; _ }
      , [ (Nolabel, e0)
        ; ( Nolabel
          , { pexp_desc=
                Pexp_extension
                  ( name
                  , PStr
                      [ ( { pstr_desc=
                              Pstr_eval
                                ( ( { pexp_desc= Pexp_fun _
                                    ; pexp_loc= _
                                    ; pexp_attributes= _
                                    ; _ } as retn )
                                , [] )
                          ; pstr_loc= _ } as pld ) ] )
            ; pexp_attributes= _
            ; pexp_loc= _
            ; _ } ) ] ) ->
      let xargs, xbody = Sugar.fun_ c.cmts (sub_exp ~ctx:(Str pld) retn) in
      hvbox 0
        (wrap_fits_breaks_exp_if ~space:false c ~loc:pexp_loc ~parens
           ( fmt_expression c (sub_exp ~ctx e0)
           $ fmt "@\n"
           $ Cmts.fmt c loc (fmt "|>@\n")
           $ hvbox c.conf.extension_indent
               (wrap "[" "]"
                  ( str "%"
                  $ hovbox 2
                      ( fmt_str_loc c name $ str " fun "
                      $ fmt_attributes c ~suf:(str " ") retn.pexp_attributes
                          ~key:"@"
                      $ fmt_fun_args c xargs $ fmt "@ ->" )
                  $ fmt "@ " $ fmt_expression c xbody )) ))
  | Pexp_apply
      ( {pexp_desc= Pexp_ident ident; pexp_attributes= []; pexp_loc; _}
      , (Nolabel, s) :: idx )
    when Option.is_some (index_op_get_sugar ident idx) ->
      let op, idx = Option.value_exn (index_op_get_sugar ident idx) in
      Cmts.relocate c.cmts ~src:pexp_loc ~before:ident.loc ~after:ident.loc ;
      fmt_index_op c ctx ~parens op s idx
  | Pexp_apply
      ( {pexp_desc= Pexp_ident ident; pexp_attributes= []; pexp_loc; _}
      , (Nolabel, s) :: idx_and_e )
    when Option.is_some (index_op_set_sugar ident idx_and_e) ->
      let op, idx, e =
        Option.value_exn (index_op_set_sugar ident idx_and_e)
      in
      Cmts.relocate c.cmts ~src:pexp_loc ~before:ident.loc ~after:ident.loc ;
      fmt_index_op c ctx ~parens op s idx ~set:e
  | Pexp_apply
      ( { pexp_desc= Pexp_ident {txt= Lident ":="; loc}
        ; pexp_attributes= []
        ; pexp_loc
        ; _ }
      , [(Nolabel, r); (Nolabel, v)] )
    when is_simple c.conf width (sub_exp ~ctx r) ->
      Cmts.relocate c.cmts ~src:pexp_loc ~before:loc ~after:loc ;
      wrap_if parens "(" ")"
        (hovbox 0
           ( fmt_expression c (sub_exp ~ctx r)
           $ Cmts.fmt c loc (fmt_assign_colon c)
           $ hvbox 2 (fmt_expression c (sub_exp ~ctx v)) ))
  | Pexp_apply
      ( { pexp_desc=
            Pexp_ident
              {txt= Lident (("~-" | "~-." | "~+" | "~+.") as op); loc}
        ; pexp_loc
        ; pexp_attributes= []
        ; _ }
      , [(Nolabel, e1)] ) ->
      let op =
        if Location.width loc = String.length op - 1 then
          String.sub op ~pos:1 ~len:(String.length op - 1)
        else op
      in
      let spc = fmt_if (exposed_left_exp e1) "@ " in
      wrap_if parens "(" ")"
        ( Cmts.fmt c pexp_loc
          @@ hvbox 2 (str op $ spc $ fmt_expression c (sub_exp ~ctx e1))
        $ fmt_atrs )
  | Pexp_apply
      ( ( { pexp_desc= Pexp_ident {txt= Lident maybe_hash; loc}
          ; pexp_attributes= []
          ; pexp_loc
          ; _ } as op )
      , [ (Nolabel, l)
        ; ( Nolabel
          , ( {pexp_desc= Pexp_ident _; pexp_loc= _; pexp_attributes= _; _}
            as r ) ) ] )
    when String.is_prefix ~prefix:"#" maybe_hash ->
      Cmts.relocate c.cmts ~src:pexp_loc ~before:loc ~after:loc ;
      wrap_if parens "(" ")"
        ( fmt_expression c (sub_exp ~ctx l)
        $ fmt_expression c (sub_exp ~ctx op)
        $ fmt_expression c (sub_exp ~ctx r) )
  | Pexp_apply
      ( ( { pexp_desc= Pexp_ident {txt= Lident id; loc= _}
          ; pexp_attributes= []
          ; pexp_loc= _
          ; _ } as op )
      , [ (Nolabel, l)
        ; ( Nolabel
          , ({pexp_desc= Pexp_fun _; pexp_loc= _; pexp_attributes; _} as r)
          ) ] )
    when is_infix_id id && not c.conf.break_infix_before_func ->
      let xargs, xbody = Sugar.fun_ c.cmts (sub_exp ~ctx r) in
      let indent_wrap = if parens then -2 else 0 in
      let pre_body, body = fmt_body c ?ext xbody in
      let followed_by_infix_op =
        match xbody.ast.pexp_desc with
        | Pexp_apply
            ( { pexp_desc= Pexp_ident {txt= Lident id; loc= _}
              ; pexp_attributes= []
              ; _ }
            , [ (Nolabel, _)
              ; (Nolabel, {pexp_desc= Pexp_fun _ | Pexp_function _; _}) ] )
          when is_infix_id id ->
            true
        | _ -> false
      in
      wrap_fits_breaks_if c.conf parens "(" ")"
        (hovbox 0
           ( hvbox 2
               ( hvbox indent_wrap
                   ( fmt_expression ~indent_wrap c (sub_exp ~ctx l)
                   $ fmt "@;"
                   $ hovbox 2
                       ( fmt_expression c (sub_exp ~ctx op)
                       $ fmt "@ fun "
                       $ fmt_attributes c ~key:"@" pexp_attributes
                           ~suf:(str " ")
                       $ hvbox_if
                           (not c.conf.wrap_fun_args)
                           4 (fmt_fun_args c xargs)
                       $ fmt "@ ->" ) )
               $ pre_body )
           $ fmt_or followed_by_infix_op "@;<1000 0>" "@ "
           $ body ))
  | Pexp_apply
      ( ( { pexp_desc= Pexp_ident {txt= Lident id; loc}
          ; pexp_attributes= []
          ; pexp_loc= _
          ; _ } as op )
      , [ (Nolabel, l)
        ; ( Nolabel
          , ( {pexp_desc= Pexp_function cs; pexp_loc= _; pexp_attributes; _}
            as r ) ) ] )
    when is_infix_id id && not c.conf.break_infix_before_func ->
      Cmts.relocate c.cmts ~src:pexp_loc ~before:loc ~after:loc ;
      let xr = sub_exp ~ctx r in
      let parens_r = parenze_exp xr in
      let indent = function_indent c ~ctx ~default:0 in
      wrap_if parens "(" ")"
        (hvbox indent
           ( hvbox 0
               ( fmt_expression c (sub_exp ~ctx l)
               $ fmt "@;"
               $ hovbox 2
                   ( fmt_expression c (sub_exp ~ctx op)
                   $ fmt "@ " $ fmt_if parens_r "( " $ str "function"
                   $ fmt_extension_suffix c ext
                   $ fmt_attributes c ~key:"@" pexp_attributes ) )
           $ fmt "@ " $ fmt_cases c (Exp r) cs $ fmt_if parens_r " )" ))
  | Pexp_apply
      ( { pexp_desc= Pexp_ident {txt= Lident id; loc= _}
        ; pexp_attributes= []
        ; pexp_loc= _
        ; _ }
      , [(Nolabel, _); (Nolabel, _)] )
    when is_infix_id id && not (is_monadic_binding_id id) ->
      let op_args = Sugar.infix c.cmts (prec_ast (Exp exp)) xexp in
      fmt_op_args
        (List.map op_args ~f:(fun (op, args) ->
             match op with
             | Some ({ast= {pexp_loc; _}; _} as op) ->
                 (* side effects of Cmts.fmt_before before fmt_expression is
                    important *)
                 let has_cmts = Cmts.has_before c.cmts pexp_loc in
                 let fmt_before_cmts = Cmts.fmt_before c pexp_loc in
                 let fmt_after_cmts = Cmts.fmt_after c pexp_loc in
                 let fmt_op = fmt_expression c op in
                 (has_cmts, fmt_before_cmts, fmt_after_cmts, (fmt_op, args))
             | None -> (false, noop, noop, (noop, args))))
  | Pexp_apply
      ( { pexp_desc= Pexp_ident {txt= Lident id; loc}
        ; pexp_loc
        ; pexp_attributes= _
        ; _ }
      , (Nolabel, s) :: (Nolabel, i) :: _ )
    when Option.is_some (index_op_get id) ->
      let index_op = Option.value_exn (index_op_get id) in
      Cmts.relocate c.cmts ~src:pexp_loc ~before:loc ~after:loc ;
      fmt_index_op c ctx ~parens {txt= index_op; loc} s [i]
  | Pexp_apply
      ( { pexp_desc= Pexp_ident {txt= Lident id; loc}
        ; pexp_loc
        ; pexp_attributes= _
        ; _ }
      , (Nolabel, s) :: (Nolabel, i) :: (Nolabel, e) :: _ )
    when Option.is_some (index_op_set id) ->
      let index_op = Option.value_exn (index_op_set id) in
      Cmts.relocate c.cmts ~src:pexp_loc ~before:loc ~after:loc ;
      fmt_index_op c ctx ~parens {txt= index_op; loc} s [i] ~set:e
  | Pexp_apply (e0, [(Nolabel, e1)]) when is_prefix e0 ->
      hvbox 2
        (wrap_fits_breaks_exp_if ~space:false c ~loc:pexp_loc ~parens
           ( fmt_expression c ~box (sub_exp ~ctx e0)
           $ fmt_expression c ~box (sub_exp ~ctx e1)
           $ fmt_atrs ))
  | Pexp_apply (e0, e1N1) -> (
      let wrap = if c.conf.wrap_fun_args then Fn.id else hvbox 2 in
      match List.rev e1N1 with
      | ( lbl
        , ({pexp_desc= Pexp_fun _; pexp_loc; pexp_attributes= _; _} as eN1)
        )
        :: rev_e1N
        when List.for_all rev_e1N ~f:(fun (_, eI) ->
                 is_simple c.conf (fun _ -> 0) (sub_exp ~ctx eI)) ->
          let e1N = List.rev rev_e1N in
          (* side effects of Cmts.fmt c.cmts before Sugar.fun_ is important *)
          let cmts_before = Cmts.fmt_before c pexp_loc in
          let xargs, xbody = Sugar.fun_ c.cmts (sub_exp ~ctx eN1) in
          let box =
            match xbody.ast.pexp_desc with
            | Pexp_fun _ | Pexp_function _ -> Some false
            | _ -> None
          in
          hvbox 0
            (wrap_if parens "(" ")"
               ( hovbox 2
                   ( wrap
                       ( fmt_args_grouped e0 e1N $ fmt "@ "
                       $ fmt_label lbl ":" $ cmts_before
                       $ hvbox 0
                           ( str "(fun "
                           $ fmt_attributes c ~key:"@" eN1.pexp_attributes
                               ~suf:(str " ")
                           $ hvbox 0 (fmt_fun_args c xargs $ fmt "@ ->") )
                       )
                   $ fmt
                       ( match xbody.ast.pexp_desc with
                       | Pexp_function _ -> "@ "
                       | _ -> "@;<1 2>" )
                   $ cbox 0 (fmt_expression c ?box xbody)
                   $ str ")" $ Cmts.fmt_after c pexp_loc )
               $ fmt_atrs ))
      | ( lbl
        , ( { pexp_desc= Pexp_function [{pc_lhs; pc_guard= None; pc_rhs}]
            ; pexp_loc
            ; pexp_attributes= _
            ; _ } as eN ) )
        :: rev_e1N
        when List.for_all rev_e1N ~f:(fun (_, eI) ->
                 is_simple c.conf (fun _ -> 0) (sub_exp ~ctx eI)) ->
          let e1N = List.rev rev_e1N in
          let ctx = Exp eN in
          (* side effects of Cmts.fmt_before before [fmt_pattern] is
             important *)
          let leading_cmt = Cmts.fmt_before c pc_lhs.ppat_loc in
          hvbox 2
            (wrap_if parens "(" ")"
               ( hovbox 4
                   ( wrap
                       ( fmt_args_grouped e0 e1N $ fmt "@ "
                       $ Cmts.fmt_before c pexp_loc
                       $ fmt_label lbl ":" $ str "(function"
                       $ fmt_attributes c ~pre:(str " ") ~key:"@"
                           eN.pexp_attributes )
                   $ fmt "@ " $ leading_cmt
                   $ hvbox 0
                       ( fmt_pattern c ~pro:(if_newline "| ")
                           (sub_pat ~ctx pc_lhs)
                       $ fmt "@ ->" )
                   $ fmt "@ "
                   $ cbox 0 (fmt_expression c (sub_exp ~ctx pc_rhs))
                   $ str ")" $ Cmts.fmt_after c pexp_loc )
               $ fmt_atrs ))
      | ( lbl
        , ( {pexp_desc= Pexp_function cs; pexp_loc; pexp_attributes= _; _}
          as eN ) )
        :: rev_e1N
        when List.for_all rev_e1N ~f:(fun (_, eI) ->
                 is_simple c.conf (fun _ -> 0) (sub_exp ~ctx eI)) ->
          let e1N = List.rev rev_e1N in
          let ctx'' = Exp eN in
          let default_indent = if c.conf.wrap_fun_args then 2 else 4 in
          let indent = function_indent c ~ctx ~default:default_indent in
          hvbox indent
            (wrap_if parens "(" ")"
               ( hovbox 2
                   (wrap
                      ( fmt_args_grouped e0 e1N $ fmt "@ "
                      $ Cmts.fmt_before c pexp_loc
                      $ fmt_label lbl ":" $ str "(function"
                      $ fmt_attributes c ~pre:(str " ") ~key:"@"
                          eN.pexp_attributes ))
               $ fmt "@ " $ fmt_cases c ctx'' cs $ str ")"
               $ Cmts.fmt_after c pexp_loc $ fmt_atrs ))
      | _ ->
          wrap_if parens "(" ")"
            (hvbox 2 (fmt_args_grouped e0 e1N1) $ fmt_atrs) )
  | Pexp_array [] ->
      hvbox 0
        ( wrap_fits_breaks c.conf "[|" "|]" (Cmts.fmt_within c pexp_loc)
        $ fmt_atrs )
  | Pexp_array e1N ->
      let p = Params.get_array_expr c.conf in
      hvbox 0
        ( p.box
            (fmt_expressions c width (sub_exp ~ctx) e1N
               (sub_exp ~ctx >> fmt_expression c)
               p)
        $ fmt_atrs )
  | Pexp_assert e0 ->
      let paren_body = parenze_exp (sub_exp ~ctx e0) in
      let hint =
        match c.conf.indicate_multiline_delimiters with
        | `Space -> (1, 0)
        | `No -> (0, 0)
        | `Closing_on_separate_line -> (1000, 0)
      in
      hovbox 0
        (wrap_if parens "(" ")"
           (hvbox 0
              ( hvbox 2
                  ( fmt_or paren_body "assert (@," "assert@ "
                  $ fmt_expression c ~parens:false (sub_exp ~ctx e0) )
              $ fits_breaks_if paren_body ")" ~hint ")"
              $ fmt_atrs )))
  | Pexp_constant const ->
      wrap_if
        (parens || not (List.is_empty pexp_attributes))
        "(" ")"
        ( fmt_constant c
            ~loc:(smallest_loc pexp_loc pexp_loc_stack)
            ?epi const
        $ fmt_atrs )
  | Pexp_constraint
      ( {pexp_desc= Pexp_pack me; pexp_attributes= []; pexp_loc= _; _}
      , { ptyp_desc= Ptyp_package (id, cnstrs)
        ; ptyp_attributes= []
        ; ptyp_loc= _
        ; _ } ) ->
      let opn_paren, cls_paren =
        match c.conf.indicate_multiline_delimiters with
        | `No -> (str "(", str ")")
        | `Space -> (fits_breaks "(" "( ", fits_breaks ")" ~hint:(1, 0) ")")
        | `Closing_on_separate_line ->
            (str "(", fits_breaks ")" ~hint:(1000, -2) ")")
      in
      hovbox 0
        (compose_module
           (fmt_module_expr c (sub_mod ~ctx me))
           ~f:(fun m ->
             hvbox 2
               ( hovbox 0
                   ( opn_paren $ str "module " $ m $ fmt "@ : "
                   $ fmt_longident_loc c id )
               $ fmt_package_type c ctx cnstrs
               $ fmt_atrs $ cls_paren )))
  | Pexp_constraint (e, t) ->
      hvbox 2
        (wrap_fits_breaks ~space:false c.conf "(" ")"
           ( fmt_expression c (sub_exp ~ctx e)
           $ fmt "@ : "
           $ fmt_core_type c (sub_typ ~ctx t)
           $ fmt_atrs ))
  | Pexp_construct ({txt= Lident (("()" | "[]") as txt); loc}, None) ->
      let opn = char txt.[0] and cls = char txt.[1] in
      let pro = str " " and epi = str " " in
      Cmts.fmt c loc
      @@ hvbox 0
           (wrap_if parens "(" ")"
              ( wrap_k opn cls (Cmts.fmt_within c ~pro ~epi pexp_loc)
              $ fmt_atrs ))
  | Pexp_construct (({txt= Lident "::"; loc= _} as lid), None) ->
      wrap_if parens "(" ")"
        (wrap "(" ")" (fmt_longident_loc c lid $ fmt_atrs))
  | Pexp_construct (lid, None) ->
      wrap_if parens "(" ")" (fmt_longident_loc c lid $ fmt_atrs)
  | Pexp_construct
      ( {txt= Lident "::"; loc= _}
      , Some
          {pexp_desc= Pexp_tuple [_; _]; pexp_attributes= []; pexp_loc= _; _}
      ) -> (
    match Sugar.list_exp c.cmts exp with
    | Some (loc_xes, nil_loc) ->
        let p = Params.get_list_expr c.conf in
        hvbox 0
          (wrap_if
             (not (List.is_empty pexp_attributes))
             "(" ")"
             ( p.box
                 ( fmt_expressions c width snd loc_xes
                     (fun (locs, xexp) ->
                       Cmts.fmt_list c ~eol:(fmt "@;<1 2>") locs
                       @@ fmt_expression c xexp)
                     p
                 $ Cmts.fmt_before c ~pro:(fmt "@;<1 2>") ~epi:noop nil_loc
                 $ Cmts.fmt_after c ~pro:(fmt "@ ") ~epi:noop nil_loc )
             $ fmt_atrs ))
    | None ->
        let loc_args = Sugar.infix_cons xexp in
        fmt_op_args
          (List.mapi loc_args ~f:(fun i (locs, arg) ->
               let f l = Cmts.has_before c.cmts l in
               let has_cmts = List.exists ~f locs in
               let fmt_before_cmts = list locs "" (Cmts.fmt_before c) in
               let fmt_op = fmt_if (i > 0) "::" in
               let fmt_after_cmts = list locs "" (Cmts.fmt_after c) in
               ( has_cmts
               , fmt_before_cmts
               , fmt_after_cmts
               , (fmt_op, [(Nolabel, arg)]) ))) )
  | Pexp_construct (({txt= Lident "::"; loc= _} as lid), Some arg) ->
      let opn, cls =
        match c.conf.indicate_multiline_delimiters with
        | `No -> (str "(", str ")")
        | `Space | `Closing_on_separate_line -> (str "( ", str " )")
      in
      wrap_if parens "(" ")"
        ( hvbox 2
            ( wrap_k opn cls (fmt_longident_loc c lid)
            $ fmt "@ "
            $ fmt_expression c (sub_exp ~ctx arg) )
        $ fmt_atrs )
  | Pexp_construct (lid, Some arg) ->
      wrap_if parens "(" ")"
        ( hvbox 2
            ( fmt_longident_loc c lid $ fmt "@ "
            $ fmt_expression c (sub_exp ~ctx arg) )
        $ fmt_atrs )
  | Pexp_variant (s, arg) ->
      hvbox 2
        (wrap_if parens "(" ")"
           ( str "`" $ str s
           $ opt arg (fmt "@ " >$ (sub_exp ~ctx >> fmt_expression c))
           $ fmt_atrs ))
  | Pexp_field (exp, lid) ->
      hvbox 2
        (wrap_if parens "(" ")"
           ( fmt_expression c (sub_exp ~ctx exp)
           $ fmt "@,." $ fmt_longident_loc c lid $ fmt_atrs ))
  | Pexp_newtype _ | Pexp_fun _ ->
      let xargs, xbody = Sugar.fun_ c.cmts xexp in
      let pre_body, body = fmt_body c ?ext xbody in
      let default_indent = if Option.is_none eol then 2 else 1 in
      let indent = function_indent c ~ctx ~default:default_indent in
      hvbox_if box indent
        (wrap_exp_if c ~loc:pexp_loc ~parens ~disambiguate:true
           ( hovbox 2
               ( hovbox 4
                   ( str "fun "
                   $ fmt_attributes c ~key:"@" pexp_attributes
                       ~suf:(str " ")
                   $ hvbox_if
                       (not c.conf.wrap_fun_args)
                       0 (fmt_fun_args c xargs)
                   $ fmt "@ " )
               $ str "->" $ pre_body )
           $ fmt "@ " $ body ))
  | Pexp_function cs ->
      let indent = function_indent c ~ctx ~default:0 in
      wrap_exp_if c ~loc:pexp_loc ~parens ~disambiguate:true
        ( hvbox 2
            ( str "function"
            $ fmt_extension_suffix c ext
            $ fmt_attributes c ~key:"@" pexp_attributes )
        $ break 1 indent
        $ hvbox 0 (fmt_cases c ctx cs) )
  | Pexp_ident {txt; loc} ->
      let wrap, wrap_ident =
        if is_symbol exp && not (List.is_empty pexp_attributes) then
          (wrap "( " " )", true)
        else if is_monadic_binding exp then (wrap "( " " )", false)
        else if is_symbol exp then (wrap_if parens "( " " )", false)
        else (wrap_if parens "(" ")", false)
      in
      Cmts.fmt c loc
      @@ wrap
           ( wrap_if wrap_ident "(" ")"
               (fmt_longident txt $ Cmts.fmt_within c loc)
           $ fmt_atrs )
  | Pexp_ifthenelse _ ->
      let cnd_exps = Sugar.ite c.cmts xexp in
      let parens_prev_bch = ref false in
      hvbox 0
        (wrap_fits_breaks_exp_if ~space:false c ~loc:pexp_loc ~parens
           (list_fl cnd_exps
              (fun ~first ~last (xcond, xbch, pexp_attributes) ->
                let parens_bch = parenze_exp xbch in
                let p =
                  Params.get_if_then_else c.conf ~first ~last ~parens
                    ~parens_bch ~parens_prev_bch:!parens_prev_bch ~xcond
                    ~expr_loc:pexp_loc
                    ~fmt_extension_suffix:(fmt_extension_suffix c ext)
                    ~fmt_attributes:
                      (fmt_attributes c ~pre:(str " ") ~key:"@"
                         pexp_attributes)
                    ~fmt_cond:(fmt_expression c)
                    ~exp_grouping:(parens_or_begin_end c ~loc:pexp_loc)
                in
                parens_prev_bch := parens_bch ;
                p.box_branch
                  ( p.cond
                  $ p.box_keyword_and_expr
                      ( p.branch_pro
                      $ p.wrap_parens
                          ( fmt_expression c ~box:false ~parens:false
                              ?pro:p.expr_pro ?eol:p.expr_eol xbch
                          $ p.break_end_branch ) ) )
                $ fmt_if_k (not last) p.space_between_branches)))
  | Pexp_let (rec_flag, bindings, body) ->
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
                          ; pexp_loc= _
                          ; _ }
                        , _ )
                  ; pstr_loc= _ } ] ) ->
            0
        | _ -> c.conf.indent_after_in
      in
      let fmt_expr = fmt_expression c (sub_exp ~ctx body) in
      let parens = parens || not (List.is_empty pexp_attributes) in
      fmt_let c ctx ~ext ~rec_flag ~bindings ~parens ~loc:pexp_loc
        ~attributes:pexp_attributes ~fmt_atrs ~fmt_expr ~indent_after_in
  | Pexp_letexception (ext_cstr, exp) ->
      let pre = fmt "let exception@ " in
      hvbox 0
        ( wrap_if
            (parens || not (List.is_empty pexp_attributes))
            "(" ")"
            ( hvbox 0
                ( hvbox 2
                    (hvbox 2
                       ( pre
                       $ fmt_extension_constructor c (str ": ") ctx ext_cstr
                       ))
                $ fmt "@ in" )
            $ fmt "@;<1000 0>"
            $ fmt_expression c (sub_exp ~ctx exp) )
        $ fmt_atrs )
  | Pexp_letmodule (name, pmod, exp) ->
      let keyword = str "let module" $ fmt_extension_suffix c ext in
      let xargs, xbody =
        sugar_pmod_functor c ~for_functor_kw:false (sub_mod ~ctx pmod)
      in
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
        match xbody.ast.pmod_desc with Pmod_apply _ -> true | _ -> false
      in
      hvbox 0
        ( wrap_if
            (parens || not (List.is_empty pexp_attributes))
            "(" ")"
            ( hvbox 2
                (fmt_module c keyword ~eqty:":" name xargs (Some xbody) xmty
                   [] ~epi:(str "in") ~can_sparse)
            $ fmt "@;<1000 0>"
            $ fmt_expression c (sub_exp ~ctx exp) )
        $ fmt_atrs )
  | Pexp_open
      ( { popen_override= flag
        ; popen_expr
        ; popen_attributes= attributes
        ; popen_loc }
      , e0 ) ->
      let override = Poly.(flag = Override) in
      let let_open =
        match c.conf.let_open with
        | `Preserve ->
            if Source.is_long_pexp_open c.source exp then `Long else `Short
        | x -> x
      in
      let force_break_if =
        match (let_open, e0.pexp_desc) with
        | `Long, _
         |( _
          , ( Pexp_let _ | Pexp_extension _ | Pexp_letexception _
            | Pexp_letmodule _ | Pexp_open _ ) ) ->
            true
        | _ -> (
          match popen_expr.pmod_desc with
          | Pmod_ident _ -> override
          | _ -> true )
      in
      let force_fit_if =
        match (let_open, xexp.ctx, popen_expr.pmod_desc) with
        | `Short, _, Pmod_ident _ when not override -> true
        | `Short, _, _ -> false
        | _, Exp {pexp_desc= Pexp_apply _ | Pexp_construct _; _}, _ ->
            not force_break_if
        | _ -> false
      in
      let fits_breaks = fits_breaks ~force_fit_if ~force_break_if
      and fits_breaks_if = fits_breaks_if ~force_fit_if ~force_break_if in
      let can_skip_parens =
        match e0.pexp_desc with
        | Pexp_array _ | Pexp_record _ -> true
        | Pexp_tuple _ -> Poly.(c.conf.parens_tuple = `Always)
        | _ -> Option.is_some (Sugar.list_exp c.cmts e0)
      in
      let opn, cls = if can_skip_parens then (".", "") else (".(", ")") in
      hvbox 0
        ( fits_breaks_if parens "" "("
        $ fits_breaks "" "let "
        $ Cmts.fmt c popen_loc
            ( fits_breaks "" (if override then "open! " else "open ")
            $ fmt_module_statement c ~attributes noop
                (sub_mod ~ctx popen_expr) )
        $ fits_breaks opn " in"
        $ fmt_or_k force_fit_if (fmt "@;<0 2>")
            (fits_breaks "" ~hint:(1000, 0) "")
        $ fmt_expression c (sub_exp ~ctx e0)
        $ fits_breaks cls ""
        $ fits_breaks_if parens "" ")"
        $ fmt_atrs )
  | Pexp_match (e0, cs) | Pexp_try (e0, cs) -> (
      let keyword =
        match exp.pexp_desc with
        | Pexp_match _ -> "match"
        | Pexp_try _ -> "try"
        | _ -> impossible "previous match"
      in
      let compact =
        match exp.pexp_desc with
        | Pexp_try
            ( _
            , [ { pc_lhs=
                    { ppat_desc=
                        Ppat_or _ | Ppat_alias ({ppat_desc= Ppat_or _; _}, _)
                    ; _ }
                ; _ } ] )
          when Poly.(c.conf.break_cases = `All) ->
            None
        | Pexp_try (_, [x]) when Poly.(c.conf.single_case = `Compact) ->
            Some x
        | _ -> None
      in
      match compact with
      | None ->
          (* TODO: remove leading_cmt once we figure out how to not
             introduce regression with 4.07 Without the line below {[ let ()
             = ( (* before *) match (* after *) x with _ -> x) ]} Gets
             reformatted into {[ let () = match (* before *) (* after *) x
             with _ -> x ]} *)
          let leading_cmt = Cmts.fmt_before c e0.pexp_loc in
          let indent = match_indent c ~ctx:xexp.ctx ~default:0 in
          hvbox indent
            (wrap_fits_breaks_exp_if ~space:false c ~loc:pexp_loc ~parens
               ~disambiguate:true
               ( leading_cmt
               $ hvbox 0
                   ( str keyword
                   $ fmt_extension_suffix c ext
                   $ fmt_attributes c ~key:"@" pexp_attributes
                   $ fmt "@;<1 2>"
                   $ fmt_expression c (sub_exp ~ctx e0)
                   $ fmt "@ with" )
               $ fmt "@ " $ fmt_cases c ctx cs ))
      | Some {pc_lhs; pc_guard; pc_rhs} ->
          (* side effects of Cmts.fmt_before before [fmt_pattern] is
             important *)
          let xpc_rhs = sub_exp ~ctx pc_rhs in
          let leading_cmt = Cmts.fmt_before c pc_lhs.ppat_loc in
          let parens_here, parens_for_exp =
            if c.conf.leading_nested_match_parens then (false, None)
            else (parenze_exp xpc_rhs, Some false)
          in
          wrap_fits_breaks_exp_if ~space:false c ~loc:pexp_loc ~parens
            ~disambiguate:true
            (hovbox 2
               ( hvbox 0
                   ( str keyword
                   $ fmt_extension_suffix c ext
                   $ fmt_attributes c ~key:"@" pexp_attributes
                   $ fmt "@;<1 2>"
                   $ fmt_expression c (sub_exp ~ctx e0)
                   $ fmt "@," )
               $ break_unless_newline 1 (-2)
               $ hvbox 0
                   ( hvbox 0
                       ( fmt "with@ " $ leading_cmt
                       $ hvbox 0
                           ( fmt_pattern c ~pro:(if_newline "| ")
                               (sub_pat ~ctx pc_lhs)
                           $ opt pc_guard (fun g ->
                                 fmt "@ when "
                                 $ fmt_expression c (sub_exp ~ctx g))
                           $ fmt "@ ->" $ fmt_if parens_here " (" ) )
                   $ fmt "@;<1 2>"
                   $ cbox 0
                       (fmt_expression c ?parens:parens_for_exp xpc_rhs) )
               $ fmt_if parens_here
                   ( match c.conf.indicate_multiline_delimiters with
                   | `No -> ")"
                   | `Space -> " )"
                   | `Closing_on_separate_line -> "@;<1000 -2>)" ) )) )
  | Pexp_pack me ->
      let fmt_mod m =
        wrap_fits_breaks_exp_if ~space:false c ~parens:true ~loc:pexp_loc
          (str "module " $ m $ fmt_atrs)
      in
      hovbox 0
        (compose_module (fmt_module_expr c (sub_mod ~ctx me)) ~f:fmt_mod)
  | Pexp_record (flds, default) ->
      let fmt_field (lid1, f) =
        let fmt_rhs e = fmt_expression c (sub_exp ~ctx e) in
        hvbox 0
          ( match f.pexp_desc with
          | Pexp_ident {txt; loc= _}
            when field_alias ~field:lid1.txt txt
                 && List.is_empty f.pexp_attributes ->
              Cmts.fmt c f.pexp_loc @@ fmt_record_field c lid1
          | Pexp_constraint
              ( { pexp_desc= Pexp_ident {txt; loc= _}
                ; pexp_loc
                ; pexp_attributes= _
                ; _ }
              , t )
            when field_alias ~field:lid1.txt txt
                 && List.is_empty f.pexp_attributes ->
              Cmts.fmt c f.pexp_loc @@ Cmts.fmt c pexp_loc
              @@ fmt_record_field c lid1 ~typ:(sub_typ ~ctx t)
          | Pexp_constraint
              ( {pexp_desc= Pexp_pack _; pexp_loc= _; pexp_attributes= _; _}
              , _ ) ->
              Cmts.fmt c f.pexp_loc
              @@ fmt_record_field c ~rhs:(fmt_rhs f) lid1
          | Pexp_constraint (e, t) when List.is_empty f.pexp_attributes ->
              let type_first =
                Poly.(`Type_first = Source.typed_expression t e)
              in
              Cmts.fmt c f.pexp_loc
              @@ fmt_record_field c ~typ:(sub_typ ~ctx t) ~rhs:(fmt_rhs e)
                   ~type_first lid1
          | _ ->
              Cmts.fmt c f.pexp_loc
              @@ fmt_record_field c ~rhs:(fmt_rhs f) lid1 )
      in
      let p1, p2 = Params.get_record_expr c.conf in
      let fmt_field ~first ~last x =
        fmt_if_k (not first) p1.sep_before
        $ fmt_field x
        $ fmt_or_k last p1.sep_after_final p1.sep_after_non_final
      in
      hvbox 0
        ( p1.box
            ( opt default (fun d ->
                  hvbox 2
                    (fmt_expression c (sub_exp ~ctx d) $ fmt "@;<1 -2>")
                  $ fmt "with" $ p2.break_after_with)
            $ list_fl flds fmt_field )
        $ fmt_atrs )
  | Pexp_extension
      ( ext
      , PStr
          [ { pstr_desc=
                Pstr_eval
                  ( ( {pexp_desc= Pexp_sequence _; pexp_attributes= []; _}
                    as e1 )
                  , _ )
            ; pstr_loc= _ } ] )
    when List.is_empty pexp_attributes
         && ( Poly.(c.conf.extension_sugar = `Always)
            || Source.extension_using_sugar ~name:ext ~payload:e1 ) ->
      fmt_sequence c parens width xexp pexp_loc fmt_atrs ~ext
  | Pexp_sequence _ ->
      fmt_sequence c parens width xexp pexp_loc fmt_atrs ?ext
  | Pexp_setfield (e1, lid, e2) ->
      hvbox 0
        (wrap_fits_breaks_exp_if ~space:false c ~loc:pexp_loc ~parens
           ( fmt_expression c (sub_exp ~ctx e1)
           $ str "." $ fmt_longident_loc c lid $ fmt_assign_arrow c
           $ fmt_expression c (sub_exp ~ctx e2)
           $ fmt_atrs ))
  | Pexp_tuple es ->
      let parens =
        match xexp.ctx with
        | Str {pstr_desc= Pstr_eval _; pstr_loc= _} -> false
        | _ -> parens || Poly.(c.conf.parens_tuple = `Always)
      in
      let no_parens_if_break =
        match xexp.ctx with
        | Exp {pexp_desc= Pexp_extension _; _} -> true
        | Pld _ -> true
        | Str {pstr_desc= Pstr_eval _; _} -> true
        | _ -> false
      in
      hvbox 0
        ( Params.wrap_tuple ~parens ~no_parens_if_break c.conf
            (list es (comma_sep c) (sub_exp ~ctx >> fmt_expression c))
        $ fmt_atrs )
  | Pexp_lazy e ->
      hvbox 2
        (wrap_fits_breaks_exp_if ~space:false c ~loc:pexp_loc ~parens
           (fmt "lazy@ " $ fmt_expression c (sub_exp ~ctx e) $ fmt_atrs))
  | Pexp_extension
      ( ext
      , PStr
          [ ( { pstr_desc=
                  Pstr_eval
                    ( ( { pexp_desc=
                            ( Pexp_while _ | Pexp_for _ | Pexp_match _
                            | Pexp_try _ | Pexp_let _ | Pexp_ifthenelse _
                            | Pexp_new _ | Pexp_letmodule _ | Pexp_object _
                            | Pexp_function _ )
                        ; pexp_attributes= []
                        ; pexp_loc= _
                        ; _ } as e1 )
                    , _ )
              ; pstr_loc= _ } as str ) ] )
    when List.is_empty pexp_attributes
         && ( Poly.(c.conf.extension_sugar = `Always)
            || Source.extension_using_sugar ~name:ext ~payload:e1 ) ->
      hvbox 0
        ( fmt_expression c ~box ?eol ~parens ~ext (sub_exp ~ctx:(Str str) e1)
        $ fmt_atrs )
  | Pexp_extension ext ->
      hvbox 0
        (wrap_fits_breaks_exp_if ~space:false c ~loc:pexp_loc ~parens
           ( hvbox c.conf.extension_indent (fmt_extension c ctx "%" ext)
           $ fmt_atrs ))
  | Pexp_for (p1, e1, e2, dir, e3) ->
      hvbox 0
        (wrap_fits_breaks_exp_if ~space:false c ~loc:pexp_loc ~parens
           ( hovbox 0
               ( hvbox 2
                   ( hvbox 0
                       ( str "for"
                       $ fmt_extension_suffix c ext
                       $ fmt "@;<1 2>"
                       $ hovbox 0
                           ( fmt_pattern c (sub_pat ~ctx p1)
                           $ fmt "@ =@;<1 2>"
                           $ fmt_expression c (sub_exp ~ctx e1)
                           $ fmt_or Poly.(dir = Upto) "@ to " "@ downto "
                           $ fmt_expression c (sub_exp ~ctx e2) )
                       $ fmt "@;do" )
                   $ fmt "@;<1000 0>"
                   $ fmt_expression c (sub_exp ~ctx e3) )
               $ fmt "@;<1000 0>done" )
           $ fmt_atrs ))
  | Pexp_coerce (e1, t1, t2) ->
      hvbox 2
        (wrap_fits_breaks ~space:false c.conf "(" ")"
           ( fmt_expression c (sub_exp ~ctx e1)
           $ opt t1 (fmt "@ : " >$ (sub_typ ~ctx >> fmt_core_type c))
           $ fmt "@ :> "
           $ fmt_core_type c (sub_typ ~ctx t2)
           $ fmt_atrs ))
  | Pexp_while (e1, e2) ->
      hvbox 0
        (wrap_fits_breaks_exp_if ~space:false c ~loc:pexp_loc ~parens
           ( hovbox 0
               ( hvbox 2
                   ( hvbox 0
                       ( str "while"
                       $ fmt_extension_suffix c ext
                       $ fmt "@;<1 2>"
                       $ fmt_expression c (sub_exp ~ctx e1)
                       $ fmt "@;do" )
                   $ fmt "@;<1000 0>"
                   $ fmt_expression c (sub_exp ~ctx e2) )
               $ fmt "@;<1000 0>done" )
           $ fmt_atrs ))
  | Pexp_unreachable -> str "."
  | Pexp_send (exp, meth) ->
      hvbox 2
        (wrap_if parens "(" ")"
           ( fmt_expression c (sub_exp ~ctx exp)
           $ fmt "@,#" $ fmt_str_loc c meth $ fmt_atrs ))
  | Pexp_new {txt; loc} ->
      Cmts.fmt c loc
      @@ hvbox 2
           (wrap_if parens "(" ")"
              ( str "new"
              $ fmt_extension_suffix c ext
              $ fmt "@ " $ fmt_longident txt $ fmt_atrs ))
  | Pexp_object {pcstr_self; pcstr_fields} ->
      hvbox 0
        (wrap_if parens "(" ")"
           ( fmt_class_structure c ~ctx ?ext pcstr_self pcstr_fields
           $ fmt_atrs ))
  | Pexp_override l -> (
      let fmt_field ({txt; loc}, f) =
        let eol = fmt "@;<1 3>" in
        let txt = Longident.parse txt in
        match f.pexp_desc with
        | Pexp_ident {txt= txt'; loc} when field_alias ~field:txt txt' ->
            Cmts.fmt c ~eol loc @@ fmt_longident txt'
        | Pexp_constraint
            ( ( { pexp_desc= Pexp_ident {txt= txt'; loc}
                ; pexp_attributes= _
                ; pexp_loc= _
                ; _ } as e )
            , t )
          when field_alias ~field:txt txt' ->
            Cmts.fmt c ~eol loc @@ fmt_expression c (sub_exp ~ctx:(Exp f) e)
            $ str " : "
            $ fmt_core_type c (sub_typ ~ctx:(Exp f) t)
        | _ ->
            Cmts.fmt c ~eol loc @@ fmt_longident txt
            $ str " = "
            $ fmt_expression c (sub_exp ~ctx f)
      in
      match l with
      | [] -> wrap "{<" ">}" (Cmts.fmt_within c pexp_loc)
      | _ ->
          hvbox 0
            (wrap_if parens "(" ")"
               (wrap_fits_breaks ~space:false c.conf "{<" ">}"
                  (list l "@;<0 1>; " fmt_field))) )
  | Pexp_setinstvar (name, expr) ->
      hvbox 0
        (wrap_fits_breaks_exp_if ~space:false c ~loc:pexp_loc ~parens
           ( fmt_str_loc c name $ fmt_assign_arrow c
           $ hvbox 2 (fmt_expression c (sub_exp ~ctx expr)) ))
  | Pexp_poly _ ->
      impossible "only used for methods, handled during method formatting"
  | Pexp_letop {let_; ands; body} ->
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
                  ; _ } ] ) ->
            0
        | _ -> c.conf.indent_after_in
      in
      let fmt_expr = fmt_expression c (sub_exp ~ctx body) in
      let parens = parens || not (List.is_empty pexp_attributes) in
      fmt_let_op c ctx ~ext ~parens ~fmt_atrs ~fmt_expr (let_ :: ands)
        ~indent_after_in

and fmt_class_structure c ~ctx ?ext self_ fields =
  let _, fields =
    List.fold_map fields ~init:c ~f:(fun c i ->
        let c =
          match i.pcf_desc with
          | Pcf_attribute atr -> update_config c [atr]
          | _ -> c
        in
        (c, (i, c)))
  in
  let cmts_after_self = Cmts.fmt_after c self_.ppat_loc in
  let self_ =
    match self_ with
    | {ppat_desc= Ppat_any; ppat_attributes= []; _} -> None
    | s -> Some s
  in
  let fmt_field (cf, c) =
    maybe_disabled c cf.pcf_loc [] @@ fun c -> fmt_class_field c ctx cf
  in
  hvbox 2
    ( hvbox 0
        ( str "object"
        $ fmt_extension_suffix c ext
        $ opt self_ (fun self_ ->
              fmt "@;" $ wrap "(" ")" (fmt_pattern c (sub_pat ~ctx self_)))
        )
    $ cmts_after_self
    $ ( match fields with
      | ({pcf_desc= Pcf_attribute a; _}, _) :: _
        when Option.is_some (fst (doc_atrs [a])) ->
          str "\n"
      | _ -> noop )
    $ fmt_if (not (List.is_empty fields)) "@;<1000 0>"
    $ hvbox 0 (list fields "\n@\n" fmt_field) )
  $ fmt_or (List.is_empty fields) "@ " "@\n"
  $ str "end"

and fmt_class_signature c ~ctx ~parens ?ext self_ fields =
  let _, fields =
    List.fold_map fields ~init:c ~f:(fun c i ->
        let c =
          match i.pctf_desc with
          | Pctf_attribute atr -> update_config c [atr]
          | _ -> c
        in
        (c, (i, c)))
  in
  let cmts_after_self = Cmts.fmt_after c self_.ptyp_loc in
  let self_ =
    match self_ with
    | {ptyp_desc= Ptyp_any; ptyp_attributes= []; _} -> None
    | s -> Some s
  in
  let fmt_field (cf, c) =
    maybe_disabled c cf.pctf_loc []
    @@ fun c -> fmt_class_type_field c ctx cf
  in
  hvbox 0
    (wrap_if parens "(" ")"
       ( hvbox 2
           ( hvbox 0
               ( str "object"
               $ fmt_extension_suffix c ext
               $ opt self_ (fun self_ ->
                     fmt "@;"
                     $ wrap "(" ")" (fmt_core_type c (sub_typ ~ctx self_)))
               )
           $ cmts_after_self
           $ ( match fields with
             | ({pctf_desc= Pctf_attribute a; _}, _) :: _
               when Option.is_some (fst (doc_atrs [a])) ->
                 str "\n"
             | _ -> noop )
           $ fmt_if (not (List.is_empty fields)) "@;<1000 0>"
           $ hvbox 0 (list fields "\n@\n" fmt_field) )
       $ fmt_or (List.is_empty fields) "@ " "@\n"
       $ str "end" ))

and fmt_class_type c ?(box = true) ({ast= typ; _} as xtyp) =
  protect (Cty typ)
  @@
  let {pcty_desc; pcty_loc; pcty_attributes} = typ in
  update_config_maybe_disabled c pcty_loc pcty_attributes
  @@ fun c ->
  let doc, atrs = doc_atrs pcty_attributes in
  Cmts.fmt c pcty_loc
  @@
  let parens = parenze_cty xtyp in
  ( hvbox_if box 0 @@ wrap_if parens "(" ")"
  @@
  let ctx = Cty typ in
  match pcty_desc with
  | Pcty_constr (name, params) ->
      let params = List.map params ~f:(fun x -> (x, Invariant)) in
      fmt_class_params c ctx params
      $ fmt_longident_loc c name
      $ fmt_attributes c ~key:"@" atrs
  | Pcty_signature {pcsig_self; pcsig_fields} ->
      fmt_class_signature c ~ctx ~parens pcsig_self pcsig_fields
      $ fmt_attributes c ~key:"@" atrs
  | Pcty_arrow (_, _, _) ->
      let arg_label lbl =
        match lbl with
        | Nolabel -> noop
        | Labelled l -> str l $ str ":"
        | Optional l -> str "?" $ str l $ str ":"
      in
      let xt1N = Sugar.class_arrow_typ c.cmts (sub_cty ~ctx typ) in
      let fmt_arg (lI, xtI) =
        hvbox 0
          ( match xtI with
          | `core_type ct -> arg_label lI $ fmt_core_type c ct
          | `class_type ct -> arg_label lI $ fmt_class_type c ct )
      in
      hvbox_if box 0 (list xt1N "@;-> " fmt_arg)
      $ fmt_attributes c ~key:"@" atrs
  | Pcty_extension ext ->
      fmt_extension c ctx "%" ext $ fmt_attributes c ~key:"@" atrs
  | Pcty_open (popen, cl) ->
      hvbox 0
        ( fmt_open_description c ~keyword:"let open" ~kw_attributes:atrs
            popen
        $ fmt " in@;<1000 0>"
        $ fmt_class_type c (sub_cty ~ctx cl) ) )
  $ fmt_docstring c ~pro:(fmt "@ ") doc

and fmt_class_expr c ?eol ?(box = true) ({ast= exp; _} as xexp) =
  protect (Cl exp)
  @@
  let {pcl_desc; pcl_loc; pcl_attributes} = exp in
  update_config_maybe_disabled c pcl_loc pcl_attributes
  @@ fun c ->
  let parens = parenze_cl xexp in
  let ctx = Cl exp in
  let fmt_args_grouped e0 a1N =
    let width xe = String.length (Cmts.preserve (fmt_expression c) xe) in
    let is_simple x = is_simple c.conf width (sub_exp ~ctx x) in
    let break (_, a1) (_, a2) = not (is_simple a1 && is_simple a2) in
    (* TODO: consider [e0] when grouping *)
    fmt_class_expr c (sub_cl ~ctx e0)
    $ fmt "@ "
    $ list_fl (List.group a1N ~break) (fmt_args c ctx)
  in
  let fmt_cmts = Cmts.fmt c ?eol pcl_loc in
  let fmt_atrs = fmt_attributes c ~pre:(str " ") ~key:"@" pcl_attributes in
  hvbox_if box 0 @@ fmt_cmts
  @@
  match pcl_desc with
  | Pcl_constr (name, params) ->
      let params = List.map params ~f:(fun x -> (x, Invariant)) in
      fmt_class_params c ctx params $ fmt_longident_loc c name $ fmt_atrs
  | Pcl_structure {pcstr_fields; pcstr_self} ->
      hvbox 0
        (wrap_if parens "(" ")"
           ( fmt_class_structure c ~ctx ?ext:None pcstr_self pcstr_fields
           $ fmt_atrs ))
  | Pcl_fun _ ->
      let xargs, xbody = Sugar.cl_fun c.cmts xexp in
      hvbox_if box
        (if Option.is_none eol then 2 else 1)
        (wrap_if parens "(" ")"
           ( hovbox 2
               ( box_fun_decl_args c 0
                   ( str "fun "
                   $ fmt_attributes c ~key:"@" pcl_attributes ~suf:(str " ")
                   $ wrap_fun_decl_args c (fmt_fun_args c xargs)
                   $ fmt "@ " )
               $ str "->" )
           $ fmt "@ "
           $ fmt_class_expr c ~eol:(fmt "@;<1000 0>") xbody ))
  | Pcl_apply (e0, e1N1) ->
      wrap_if parens "(" ")" (hvbox 2 (fmt_args_grouped e0 e1N1) $ fmt_atrs)
  | Pcl_let (rec_flag, bindings, body) ->
      let indent_after_in =
        match body.pcl_desc with
        | Pcl_let _ -> 0
        | _ -> c.conf.indent_after_in
      in
      let fmt_expr = fmt_class_expr c (sub_cl ~ctx body) in
      let parens = parens || not (List.is_empty pcl_attributes) in
      fmt_let c ctx ~ext:None ~rec_flag ~bindings ~parens ~loc:pcl_loc
        ~attributes:pcl_attributes ~fmt_atrs ~fmt_expr ~indent_after_in
  | Pcl_constraint (e, t) ->
      hvbox 2
        (wrap_fits_breaks ~space:false c.conf "(" ")"
           ( fmt_class_expr c (sub_cl ~ctx e)
           $ fmt "@ : "
           $ fmt_class_type c (sub_cty ~ctx t) ))
      $ fmt_atrs
  | Pcl_extension ext -> fmt_extension c ctx "%" ext $ fmt_atrs
  | Pcl_open (popen, cl) ->
      hvbox 0
        ( fmt_open_description c ~keyword:"let open"
            ~kw_attributes:pcl_attributes popen
        $ fmt " in@;<1000 0>"
        $ fmt_class_expr c (sub_cl ~ctx cl) )

and fmt_class_field c ctx (cf : class_field) =
  let {pcf_desc; pcf_loc; pcf_attributes} = cf in
  update_config_maybe_disabled c pcf_loc pcf_attributes
  @@ fun c ->
  let fmt_cmts = Cmts.fmt c ?eol:None pcf_loc in
  let doc_before, doc_after, atrs =
    fmt_docstring_around_item ~fit:true c pcf_attributes
  in
  let fmt_atrs = fmt_attributes c ~pre:(str " ") ~key:"@@" atrs in
  let fmt_kind = function
    | Cfk_virtual typ ->
        (fmt "@ : " $ fmt_core_type c (sub_typ ~ctx typ), noop, noop, noop)
    | Cfk_concrete
        ( _
        , { pexp_desc=
              Pexp_poly
                ( e
                , Some
                    ( { ptyp_desc= Ptyp_poly (poly_args, _)
                      ; ptyp_loc= _
                      ; ptyp_attributes= _
                      ; _ } as poly ) )
          ; pexp_loc
          ; pexp_attributes= _
          ; _ } ) -> (
        let rec cleanup names e args' =
          match (e, args') with
          | {pexp_desc= Pexp_constraint (e, t); _}, [] ->
              Some (List.rev names, t, e)
          | ( {pexp_desc= Pexp_newtype (({txt; _} as newtyp), body); _}
            , {txt= txt'; _} :: args )
            when String.equal txt txt' ->
              cleanup (newtyp :: names) body args
          | _ -> None
        in
        match cleanup [] e poly_args with
        | Some (args, t, e) ->
            let before =
              match args with x :: _ -> x.loc | [] -> e.pexp_loc
            in
            Cmts.relocate c.cmts ~src:pexp_loc ~before ~after:e.pexp_loc ;
            ( fmt "@ : type "
              $ list args "@ " (fun name -> fmt_str_loc c name)
              $ fmt_core_type ~pro:"." ~pro_space:false c (sub_typ ~ctx t)
            , noop
            , fmt "@;<1 2>="
            , fmt "@ " $ fmt_expression c (sub_exp ~ctx e) )
        | None ->
            ( fmt "@ : " $ fmt_core_type c (sub_typ ~ctx poly)
            , noop
            , fmt "@;<1 2>="
            , fmt "@ " $ fmt_expression c (sub_exp ~ctx e) ) )
    | Cfk_concrete
        ( _
        , {pexp_desc= Pexp_poly (e, poly); pexp_loc; pexp_attributes= _; _}
        ) ->
        let xargs, xbody =
          match poly with
          | None ->
              Sugar.fun_ c.cmts ~will_keep_first_ast_node:false
                (sub_exp ~ctx e)
          | Some _ -> ([], sub_exp ~ctx e)
        in
        let ty, e =
          match (xbody.ast, poly) with
          | {pexp_desc= Pexp_constraint (e, t); pexp_loc; _}, None ->
              Cmts.relocate c.cmts ~src:pexp_loc ~before:t.ptyp_loc
                ~after:e.pexp_loc ;
              (Some t, sub_exp ~ctx e)
          | {pexp_desc= Pexp_constraint _; _}, Some _ -> (poly, xbody)
          | _, poly -> (poly, xbody)
        in
        Cmts.relocate c.cmts ~src:pexp_loc ~before:e.ast.pexp_loc
          ~after:e.ast.pexp_loc ;
        ( noop
        , fmt_if (not (List.is_empty xargs)) "@ "
          $ wrap_fun_decl_args c (fmt_fun_args c xargs)
          $ opt ty (fun t -> fmt "@ : " $ fmt_core_type c (sub_typ ~ctx t))
        , fmt "@;<1 2>="
        , fmt "@ " $ fmt_expression c e )
    | Cfk_concrete (_, e) ->
        let ty, e =
          match e with
          | {pexp_desc= Pexp_constraint (e, t); _} -> (Some t, e)
          | _ -> (None, e)
        in
        ( opt ty (fun t -> fmt "@ : " $ fmt_core_type c (sub_typ ~ctx t))
        , noop
        , fmt "@;<1 2>="
        , fmt "@ " $ fmt_expression c (sub_exp ~ctx e) )
  in
  let virtual_or_override = function
    | Cfk_virtual _ -> fmt "@ virtual"
    | Cfk_concrete (Override, _) -> str "!"
    | Cfk_concrete (Fresh, _) -> noop
  in
  let pcf =
    match pcf_desc with
    | Pcf_inherit (override, cl, parent) ->
        hovbox 2
          ( str "inherit"
          $ fmt_if Poly.(override = Override) "!"
          $ fmt "@ "
          $ ( fmt_class_expr c (sub_cl ~ctx cl)
            $ opt parent (fun p -> str " as " $ fmt_str_loc c p) ) )
    | Pcf_method (name, priv, kind) ->
        let typ, args, eq, expr = fmt_kind kind in
        hvbox 2
          ( hovbox 2
              ( hovbox 4
                  (box_fun_decl_args c 4
                     ( box_fun_sig_args c 4
                         ( str "method" $ virtual_or_override kind
                         $ fmt_if Poly.(priv = Private) " private"
                         $ fmt " " $ fmt_str_loc c name $ typ )
                     $ args ))
              $ eq )
          $ expr )
    | Pcf_val (name, mut, kind) ->
        let typ, args, eq, expr = fmt_kind kind in
        hvbox 2
          ( hovbox 2
              ( hovbox 4
                  (box_fun_decl_args c 4
                     ( box_fun_sig_args c 4
                         ( str "val" $ virtual_or_override kind
                         $ fmt_if Poly.(mut = Mutable) " mutable"
                         $ fmt " " $ fmt_str_loc c name $ typ )
                     $ args ))
              $ eq )
          $ expr )
    | Pcf_constraint (t1, t2) ->
        fmt "constraint@ "
        $ fmt_core_type c (sub_typ ~ctx t1)
        $ str " = "
        $ fmt_core_type c (sub_typ ~ctx t2)
    | Pcf_initializer e ->
        fmt "initializer@ " $ fmt_expression c (sub_exp ~ctx e)
    | Pcf_attribute atr ->
        let doc, atrs = doc_atrs [atr] in
        fmt_docstring c ~standalone:true ~epi:noop doc
        $ fmt_attributes c ~key:"@@@" atrs
    | Pcf_extension ext -> fmt_extension c ctx "%%" ext
  in
  fmt_cmts (hvbox 0 (doc_before $ pcf $ fmt_atrs $ doc_after))

and fmt_class_type_field c ctx (cf : class_type_field) =
  let {pctf_desc; pctf_loc; pctf_attributes} = cf in
  update_config_maybe_disabled c pctf_loc pctf_attributes
  @@ fun c ->
  let fmt_cmts = Cmts.fmt c pctf_loc in
  let doc, atrs = doc_atrs pctf_attributes in
  let fmt_atrs = fmt_attributes c ~pre:(str " ") ~key:"@@" atrs in
  fmt_cmts
    ( fmt_docstring c ~epi:(fmt "@\n") doc
    $ hvbox 0
        ( match pctf_desc with
        | Pctf_inherit ct ->
            hovbox 2 (fmt "inherit@ " $ fmt_class_type c (sub_cty ~ctx ct))
        | Pctf_method (name, priv, virt, ty) ->
            box_fun_sig_args c 2
              ( hovbox 4
                  ( str "method"
                  $ fmt_if Poly.(virt = Virtual) "@ virtual"
                  $ fmt_if Poly.(priv = Private) "@ private"
                  $ fmt "@ " $ fmt_str_loc c name )
              $ fmt " :@ "
              $ fmt_core_type c (sub_typ ~ctx ty) )
        | Pctf_val (name, mut, virt, ty) ->
            box_fun_sig_args c 2
              ( hovbox 4
                  ( str "val"
                  $ fmt_if Poly.(virt = Virtual) "@ virtual"
                  $ fmt_if Poly.(mut = Mutable) "@ mutable"
                  $ fmt "@ " $ fmt_str_loc c name )
              $ fmt " :@ "
              $ fmt_core_type c (sub_typ ~ctx ty) )
        | Pctf_constraint (t1, t2) ->
            fmt "constraint@ "
            $ fmt_core_type c (sub_typ ~ctx t1)
            $ str " = "
            $ fmt_core_type c (sub_typ ~ctx t2)
        | Pctf_attribute atr ->
            let doc, atrs = doc_atrs [atr] in
            fmt_docstring c ~standalone:true ~epi:noop doc
            $ fmt_attributes c ~key:"@@@" atrs
        | Pctf_extension ext -> fmt_extension c ctx "%%" ext )
    $ fmt_atrs )

and fmt_cases c ctx cs =
  let rec pattern_len ?(parens = 0) pat =
    match pat.ppat_desc with
    | Ppat_any -> Some 1
    | Ppat_var {txt= s; _}
     |Ppat_constant (Pconst_integer (s, _))
     |Ppat_constant (Pconst_float (s, _))
     |Ppat_construct ({txt= Lident s; _}, None) ->
        Some (String.length s)
    | Ppat_construct ({txt= Lident s; _}, Some arg) -> (
      match pattern_len ~parens:2 arg with
      | Some arg -> Some (parens + String.length s + 1 + arg)
      | None -> None )
    | Ppat_constant (Pconst_char chr) ->
        Some (String.length (char_escaped c ~loc:pat.ppat_loc chr) + 2)
    | Ppat_variant (s, None) -> Some (String.length s + 1)
    | Ppat_variant (s, Some arg) -> (
      match pattern_len ~parens:2 arg with
      | Some arg -> Some (parens + 1 + String.length s + 1 + arg)
      | None -> None )
    | Ppat_tuple ps ->
        (* commas and parenthesis *)
        let init = ((List.length ps - 1) * 2) + parens in
        fold_pattern_len ~parens:2 ~init ~f:( + ) ps
    | Ppat_alias _ | Ppat_interval _ | Ppat_construct _
     |Ppat_constant (Pconst_string _)
     |Ppat_record _ | Ppat_array _ | Ppat_constraint _ | Ppat_type _
     |Ppat_or _ | Ppat_unpack _ | Ppat_lazy _ | Ppat_exception _
     |Ppat_extension _ | Ppat_open _ ->
        None
  and fold_pattern_len ?parens ?(init = 0) ~f ps =
    List.fold_until ~init ps
      ~f:(fun acc pat ->
        match pattern_len ?parens pat with
        | Some l -> Continue (f acc l)
        | None -> Stop None)
      ~finish:(fun acc -> Some acc)
  in
  let max_len_name =
    fold_pattern_len ~f:max (List.map ~f:(fun case -> case.pc_lhs) cs)
  in
  list_fl cs (fun ~first ~last {pc_lhs; pc_guard; pc_rhs} ->
      let xrhs = sub_exp ~ctx pc_rhs in
      let indent =
        match
          (c.conf.cases_matching_exp_indent, (ctx, pc_rhs.pexp_desc))
        with
        | ( `Compact
          , ( Exp {pexp_desc= Pexp_function _ | Pexp_match _ | Pexp_try _; _}
            , (Pexp_match _ | Pexp_try _) ) ) ->
            2
        | _, _ -> c.conf.cases_exp_indent
      in
      let align_nested_match =
        match (pc_rhs.pexp_desc, c.conf.nested_match) with
        | (Pexp_match _ | Pexp_try _), `Align -> last
        | _ -> false
      in
      let parens_here, parens_for_exp =
        if align_nested_match then (false, Some false)
        else if c.conf.leading_nested_match_parens then (false, None)
        else (parenze_exp xrhs, Some false)
      in
      (* side effects of Cmts.fmt_before before [fmt_lhs] is important *)
      let leading_cmt = Cmts.fmt_before c pc_lhs.ppat_loc in
      let xlhs = sub_pat ~ctx pc_lhs in
      let paren_lhs =
        match pc_lhs.ppat_desc with
        | Ppat_or _ when Option.is_some pc_guard -> true
        | _ -> parenze_pat xlhs
      in
      let eol =
        Option.some_if
          (Cmts.has_before c.cmts pc_rhs.pexp_loc)
          (fmt "@;<1000 0>")
      in
      let indent = if align_nested_match then 0 else indent in
      let fmt_padding =
        let level = match c.conf.break_cases with `Nested -> 2 | _ -> 3 in
        fmt_if_k
          ( c.conf.align_cases
          && not (Cmts.has_after c.cmts xlhs.ast.ppat_loc) )
          ( match (max_len_name, pattern_len xlhs.ast) with
          | Some max_len, Some len ->
              let pad = String.make (max_len - len) ' ' in
              fmt_or_k
                Poly.(c.conf.break_cases = `All)
                (str pad)
                (fits_breaks ~level "" pad)
          | _ -> noop )
      in
      Params.get_cases c.conf ~first ~indent ~parens_here
      |> fun (p : Params.cases) ->
      p.leading_space $ leading_cmt
      $ p.box_all
          ( p.box_pattern_arrow
              ( hvbox 0
                  ( fmt_pattern c ~pro:p.bar ~parens:paren_lhs xlhs
                  $ fmt_padding
                  $ opt pc_guard (fun g ->
                        fmt "@;<1 2>when "
                        $ fmt_expression c (sub_exp ~ctx g)) )
              $ p.break_before_arrow $ str "->" $ p.break_after_arrow
              $ fmt_if parens_here " (" )
          $ p.break_after_opening_paren
          $ hovbox 0
              ( fmt_expression ?eol c ?parens:parens_for_exp xrhs
              $ fmt_if parens_here
                  ( match c.conf.indicate_multiline_delimiters with
                  | `Space -> "@ )"
                  | `No -> "@,)"
                  | `Closing_on_separate_line -> "@;<1000 -2>)" ) ) ))

and fmt_value_description c ctx vd =
  let { pval_name= {txt; loc}
      ; pval_type
      ; pval_prim
      ; pval_attributes
      ; pval_loc } =
    vd
  in
  update_config_maybe_disabled c pval_loc pval_attributes
  @@ fun c ->
  let pre = if List.is_empty pval_prim then "val" else "external" in
  let doc_before, doc_after, atrs =
    fmt_docstring_around_item c pval_attributes
  in
  hvbox 0
    ( doc_before
    $ box_fun_sig_args c 2
        ( str pre $ str " "
        $ Cmts.fmt c loc (wrap_if (is_symbol_id txt) "( " " )" (str txt))
        $ fmt_core_type c ~pro:":"
            ~box:
              (not (c.conf.ocp_indent_compat && is_arrow_or_poly pval_type))
            ~pro_space:true (sub_typ ~ctx pval_type)
        $ list_fl pval_prim (fun ~first ~last:_ s ->
              fmt_if first "@ =" $ fmt " \"" $ str s $ fmt "\"") )
    $ fmt_attributes c ~pre:(fmt "@;<1 2>") ~key:"@@" atrs
    $ doc_after )

and fmt_tydcl_params c ctx params =
  fmt_if_k
    (not (List.is_empty params))
    ( wrap_fits_breaks_if ~space:false c.conf
        (List.length params > 1)
        "(" ")"
        (list params (comma_sep c) (fun (ty, vc) ->
             fmt_variance vc $ fmt_core_type c (sub_typ ~ctx ty)))
    $ fmt "@ " )

and fmt_class_params c ctx params =
  let fmt_param ~first ~last (ty, vc) =
    fmt_if (first && exposed_left_typ ty) " "
    $ fmt_if_k (not first) (fmt (comma_sep c))
    $ fmt_variance vc
    $ fmt_core_type c (sub_typ ~ctx ty)
    $ fmt_if (last && exposed_right_typ ty) " "
  in
  fmt_if_k
    (not (List.is_empty params))
    (hvbox 0
       ( wrap_fits_breaks c.conf "[" "]" (list_fl params fmt_param)
       $ fmt "@ " ))

and fmt_type_declaration c ?ext ?(pre = "") ?(brk = noop) ctx ?fmt_name
    ?(eq = "=") decl =
  let { ptype_name= {txt; loc}
      ; ptype_params
      ; ptype_cstrs
      ; ptype_kind
      ; ptype_private
      ; ptype_manifest
      ; ptype_attributes
      ; ptype_loc } =
    decl
  in
  update_config_maybe_disabled c ptype_loc ptype_attributes
  @@ fun c ->
  let fmt_manifest ~priv manifest decl =
    let break_before_manifest_kind =
      match ptype_kind with
      | Ptype_abstract -> fmt "@ "
      | Ptype_variant _ | Ptype_record _ | Ptype_open -> fmt "@;<1 4>"
    in
    let fmt_manifest typ =
      fmt_private_flag priv $ break_before_manifest_kind
      $ fmt_core_type c ~in_type_declaration:true (sub_typ ~ctx typ)
    in
    let eq = str " " $ str eq in
    match (manifest, decl) with
    | Some m, Some d -> eq $ fmt_manifest m $ str " =" $ d
    | Some m, None -> eq $ fmt_manifest m
    | None, Some d -> eq $ d
    | None, None -> noop
  in
  let box_manifest k =
    hvbox c.conf.type_decl_indent
      ( str pre
      $ fmt_extension_suffix c ext
      $ str " "
      $ hvbox_if
          (not (List.is_empty ptype_params))
          0
          ( fmt_tydcl_params c ctx ptype_params
          $ Option.value fmt_name ~default:(str txt) )
      $ k )
  in
  let fmt_manifest_kind mfst priv kind =
    match kind with
    | Ptype_abstract -> box_manifest (fmt_manifest ~priv mfst None)
    | Ptype_variant [] ->
        box_manifest
          (fmt_manifest ~priv:Public mfst (Some (fmt_private_flag priv)))
        $ fmt "@ |"
    | Ptype_variant ctor_decls ->
        let max acc d =
          let len_around = if is_symbol_id d.pcd_name.txt then 4 else 0 in
          max acc (String.length d.pcd_name.txt + len_around)
        in
        let max_len_name = List.fold_left ctor_decls ~init:0 ~f:max in
        box_manifest
          (fmt_manifest ~priv:Public mfst (Some (fmt_private_flag priv)))
        $ fmt "@ "
        $ list_fl ctor_decls
            (fmt_constructor_declaration c ~max_len_name ctx)
    | Ptype_record lbl_decls ->
        let p = Params.get_record_type c.conf in
        let fmt_decl ~first ~last x =
          fmt_if_k (not first) p.sep_before
          $ fmt_label_declaration c ctx x ~last
          $ fmt_if (last && exposed_right_typ x.pld_type) " "
          $ fmt_if_k (not last) p.sep_after
        in
        box_manifest
          (fmt_manifest ~priv:Public mfst
             (Some (fmt_private_flag priv $ p.docked_before)))
        $ p.break_before
        $ p.box_record (list_fl lbl_decls fmt_decl)
        $ p.break_after $ p.docked_after
    | Ptype_open ->
        box_manifest
          (fmt_manifest ~priv:Public mfst
             (Some (fmt_private_flag priv $ str " ..")))
  in
  let fmt_cstr (t1, t2, loc) =
    Cmts.fmt c loc
      (hvbox 2
         ( fmt "constraint@ "
         $ fmt_core_type c (sub_typ ~ctx t1)
         $ fmt " =@ "
         $ fmt_core_type c (sub_typ ~ctx t2) ))
  in
  let fmt_cstrs cstrs =
    fmt_if_k
      (not (List.is_empty cstrs))
      (fmt "@ " $ hvbox 0 (list cstrs "@ " fmt_cstr))
  in
  (* Docstring cannot be placed after variant declarations *)
  let force_before =
    match ptype_kind with Ptype_variant _ -> true | _ -> false
  in
  let doc_before, doc_after, atrs =
    let fit = Ast.type_decl_is_simple decl in
    fmt_docstring_around_item ~force_before ~fit c ptype_attributes
  in
  Cmts.fmt c loc @@ Cmts.fmt c ptype_loc
  @@ hvbox 0
       ( doc_before
       $ hvbox 0
           ( hvbox c.conf.type_decl_indent
               ( fmt_manifest_kind ptype_manifest ptype_private ptype_kind
               $ fmt_cstrs ptype_cstrs )
           $ fmt_attributes c ~pre:(fmt "@ ") ~key:"@@" atrs )
       $ doc_after )
  $ brk

and fmt_label_declaration c ctx decl ?(last = false) =
  let {pld_mutable; pld_name; pld_type; pld_loc; pld_attributes} = decl in
  update_config_maybe_disabled c pld_loc pld_attributes
  @@ fun c ->
  let doc, atrs = doc_atrs pld_attributes in
  let cmt_after_type = Cmts.fmt_after c pld_type.ptyp_loc in
  let field_loose =
    match c.conf.field_space with
    | `Loose -> true
    | `Tight_decl | `Tight -> false
  in
  let fmt_semicolon =
    match c.conf.break_separators with
    | `Before -> noop
    | `After -> fmt_if (not last) ";"
    | `After_and_docked ->
        fmt_or_k last (fits_breaks ~level:5 "" ";") (str ";")
  in
  hovbox 0
    ( Cmts.fmt_before c pld_loc
    $ hvbox 4
        ( hvbox 3
            ( hvbox 4
                ( hvbox 2
                    ( fmt_if Poly.(pld_mutable = Mutable) "mutable "
                    $ fmt_str_loc c pld_name $ fmt_if field_loose " "
                    $ fmt ":@ "
                    $ fmt_core_type c (sub_typ ~ctx pld_type)
                    $ fmt_semicolon )
                $ cmt_after_type )
            $ fmt_attributes c ~pre:(fmt "@;<1 1>") ~key:"@" atrs )
        $ Cmts.fmt_after c pld_loc
        $ fmt_docstring_padded c doc ) )

and fmt_constructor_declaration c ctx ~max_len_name ~first ~last:_ cstr_decl
    =
  let {pcd_name= {txt; loc}; pcd_args; pcd_res; pcd_attributes; pcd_loc} =
    cstr_decl
  in
  update_config_maybe_disabled c pcd_loc pcd_attributes
  @@ fun c ->
  let doc, atrs = doc_atrs pcd_attributes in
  let fmt_padding =
    let is_empty =
      match pcd_args with
      | Pcstr_tuple x -> List.is_empty x
      | Pcstr_record x -> List.is_empty x
    in
    let len_around = if is_symbol_id txt then 4 else 0 in
    let pad =
      String.make (max_len_name - String.length txt - len_around) ' '
    in
    fmt_if_k
      ( c.conf.align_constructors_decl && (not is_empty)
      && not (Cmts.has_after c.cmts loc) )
      (fmt_or_k
         Poly.(c.conf.type_decl = `Sparse)
         (str pad)
         (fits_breaks ~level:3 "" pad))
  in
  fmt_if (not first)
    ( match c.conf.type_decl with
    | `Sparse -> "@;<1000 0>"
    | `Compact -> "@ " )
  $ Cmts.fmt_before c pcd_loc $ Cmts.fmt_before c loc
  $ fmt_or_k first (if_newline "| ") (str "| ")
  $ hvbox 0
      ( hovbox 2
          ( hvbox 2
              ( Cmts.fmt c loc
                  (wrap_if (is_symbol_id txt) "( " " )" (str txt))
              $ fmt_padding
              $ fmt_constructor_arguments_result c ctx pcd_args pcd_res )
          $ fmt_attributes c ~pre:(fmt "@;") ~key:"@" atrs
          $ fmt_docstring_padded c doc )
      $ Cmts.fmt_after c ~pro:(fmt_or c.conf.wrap_comments "@ " " ") pcd_loc
      )

and fmt_constructor_arguments c ctx ~pre = function
  | Pcstr_tuple [] -> noop
  | Pcstr_tuple typs ->
      pre $ fmt "@ "
      $ hvbox 0 (list typs "@ * " (sub_typ ~ctx >> fmt_core_type c))
  | Pcstr_record lds ->
      let p = Params.get_record_type c.conf in
      let fmt_ld ~first ~last x =
        fmt_if_k (not first) p.sep_before
        $ fmt_label_declaration c ctx x ~last
        $ fmt_if (last && exposed_right_typ x.pld_type) " "
        $ fmt_if_k (not last) p.sep_after
      in
      pre $ p.docked_before $ p.break_before
      $ p.box_record (list_fl lds fmt_ld)
      $ p.break_after $ p.docked_after

and fmt_constructor_arguments_result c ctx args res =
  let pre = fmt_or (Option.is_none res) " of" " :" in
  let before_type = match args with Pcstr_tuple [] -> ": " | _ -> "-> " in
  let fmt_type typ =
    fmt "@ " $ str before_type $ fmt_core_type c (sub_typ ~ctx typ)
  in
  fmt_constructor_arguments c ctx ~pre args $ opt res fmt_type

and fmt_type_extension c ctx
    { ptyext_attributes
    ; ptyext_params
    ; ptyext_path
    ; ptyext_constructors
    ; ptyext_private
    ; ptyext_loc } =
  let c = update_config c ptyext_attributes in
  let doc, atrs = doc_atrs ptyext_attributes in
  let fmt_ctor ctor =
    let sep =
      match ctor.pext_kind with
      | Pext_decl (_, Some _) -> fmt " :@ "
      | Pext_decl (_, None) | Pext_rebind _ -> fmt " of@ "
    in
    hvbox 0 (fmt_extension_constructor c sep ctx ctor)
  in
  Cmts.fmt c ptyext_loc
  @@ hvbox 2
       ( fmt_docstring c ~epi:(fmt "@,") doc
       $ hvbox c.conf.type_decl_indent
           ( str "type "
           $ hvbox_if
               (not (List.is_empty ptyext_params))
               0
               (fmt_tydcl_params c ctx ptyext_params)
           $ fmt_longident_loc c ptyext_path
           $ str " +="
           $ fmt_private_flag ptyext_private
           $ fmt "@ "
           $ hvbox 0
               (if_newline "| " $ list ptyext_constructors "@ | " fmt_ctor)
           )
       $ fmt_attributes c ~pre:(fmt "@ ") ~key:"@@" atrs )

and fmt_type_exception ~pre c sep ctx
    {ptyexn_attributes; ptyexn_constructor; ptyexn_loc} =
  let doc1, atrs = doc_atrs ptyexn_attributes in
  let doc1 = Option.value ~default:[] doc1 in
  let {pext_attributes; _} = ptyexn_constructor in
  (* On 4.08 the doc is attached to the constructor *)
  let doc1, pext_attributes = doc_atrs ~acc:doc1 pext_attributes in
  let doc2, pext_attributes = doc_atrs pext_attributes in
  let doc_before, doc_after = fmt_docstring_around_item' c doc1 doc2 in
  let ptyexn_constructor = {ptyexn_constructor with pext_attributes} in
  Cmts.fmt c ptyexn_loc
    (hvbox 0
       ( doc_before
       $ hvbox 2
           (pre $ fmt_extension_constructor c sep ctx ptyexn_constructor)
       $ fmt_attributes c ~pre:(fmt "@ ") ~key:"@@" atrs
       $ doc_after ))

and fmt_extension_constructor c sep ctx ec =
  let {pext_name; pext_kind; pext_attributes; pext_loc} = ec in
  update_config_maybe_disabled c pext_loc pext_attributes
  @@ fun c ->
  let doc, atrs = doc_atrs pext_attributes in
  let suf =
    match pext_kind with
    | Pext_decl (_, None) | Pext_rebind _ -> noop
    | Pext_decl (_, Some _) -> str " "
  in
  Cmts.fmt c pext_loc
  @@ hvbox 4
       ( hvbox 2
           ( fmt_str_loc c pext_name
           $
           match pext_kind with
           | Pext_decl ((Pcstr_tuple [] | Pcstr_record []), None) -> noop
           | Pext_decl ((Pcstr_tuple [] | Pcstr_record []), Some res) ->
               sep $ fmt_core_type c (sub_typ ~ctx res)
           | Pext_decl (args, res) ->
               fmt_constructor_arguments_result c ctx args res
           | Pext_rebind lid -> str " = " $ fmt_longident_loc c lid )
       $ fmt_attributes c ~pre:(fmt "@ ") ~key:"@" atrs ~suf
       $ fmt_docstring_padded c doc )

and fmt_functor_arg c (name, mt) =
  wrap "(" ")"
    ( match mt with
    | None -> fmt_str_loc c name
    | Some mt ->
        hovbox 0
          ( hovbox 0 (fmt_str_loc c name $ fmt "@ : ")
          $ compose_module (fmt_module_type c mt) ~f:Fn.id ) )

and fmt_module_type c ({ast= mty; _} as xmty) =
  let ctx = Mty mty in
  let {pmty_desc; pmty_loc; pmty_attributes} = mty in
  update_config_maybe_disabled_block c pmty_loc pmty_attributes
  @@ fun c ->
  let parens = parenze_mty xmty in
  match pmty_desc with
  | Pmty_ident lid ->
      { empty with
        bdy= fmt_longident_loc c lid
      ; epi=
          Some (fmt_attributes c ~key:"@" pmty_attributes ~pre:(fmt "@ "))
      }
  | Pmty_signature s ->
      let empty =
        List.is_empty s && not (Cmts.has_within c.cmts pmty_loc)
      in
      let doc, atrs = doc_atrs pmty_attributes in
      let before = Cmts.fmt_before c pmty_loc in
      let within = Cmts.fmt_within c ~pro:noop pmty_loc in
      let after = Cmts.fmt_after c pmty_loc in
      { opn= noop
      ; pro=
          Some
            ( before
            $ fmt_docstring c ~epi:(fmt "@,") doc
            $ str "sig" $ fmt_if empty " " )
      ; psp= fmt_if (not empty) "@;<1000 2>"
      ; bdy= within $ fmt_signature c ctx s
      ; cls= noop
      ; esp= fmt_if (not empty) "@;<1000 0>"
      ; epi=
          Some
            ( str "end" $ after
            $ fmt_attributes c ~key:"@" atrs ~pre:(fmt "@ ") ) }
  | Pmty_functor _ ->
      let for_functor_kw = true in
      let xargs, mt2 = Sugar.functor_type c.cmts ~for_functor_kw xmty in
      let blk = fmt_module_type c mt2 in
      { blk with
        pro=
          Some
            ( str "functor"
            $ fmt_attributes c ~pre:(str " ") ~key:"@" pmty_attributes
            $ fmt "@;<1 2>"
            $ list xargs "@;<1 2>" (fmt_functor_arg c)
            $ fmt "@;<1 2>->"
            $ opt blk.pro (fun pro -> str " " $ pro) )
      ; epi= Some (Option.call ~f:blk.epi $ Cmts.fmt_after c pmty_loc)
      ; psp=
          fmt_or_k (Option.is_none blk.pro)
            (fits_breaks " " ~hint:(1, 2) "")
            blk.psp }
  | Pmty_with _ ->
      let wcs, mt = Sugar.mod_with (sub_mty ~ctx mty) in
      let fmt_cstr ~first ~last:_ wc =
        fmt_or first "@ with" "@;<1 1>and" $ fmt_with_constraint c ctx wc
      in
      let fmt_cstrs ~first:_ ~last:_ (wcs_and, loc, attr) =
        Cmts.fmt c loc
          ( list_fl wcs_and fmt_cstr
          $ fmt_attributes c ~pre:(str " ") ~key:"@" attr )
      in
      let {pro; psp; bdy; esp; epi; opn= _; cls= _} =
        fmt_module_type c mt
      in
      { empty with
        pro=
          Option.map pro ~f:(fun pro ->
              open_hvbox 0 $ fmt_if parens "(" $ pro)
      ; psp
      ; bdy=
          fmt_if_k (Option.is_none pro) (open_hvbox 2 $ fmt_if parens "(")
          $ hvbox 0 bdy
          $ fmt_if_k (Option.is_some epi) esp
          $ Option.call ~f:epi $ list_fl wcs fmt_cstrs $ fmt_if parens ")"
          $ close_box
      ; esp= fmt_if_k (Option.is_none epi) esp
      ; epi= Some (Cmts.fmt_after c pmty_loc) }
  | Pmty_typeof me -> (
      let blk = fmt_module_expr c (sub_mod ~ctx me) in
      let epi =
        Option.call ~f:blk.epi $ Cmts.fmt_after c pmty_loc
        $ fmt_if parens ")"
        $ fmt_attributes c ~key:"@" pmty_attributes ~pre:(fmt "@ ")
      in
      match blk.pro with
      | Some pro ->
          { blk with
            pro=
              Some
                ( Cmts.fmt_before c pmty_loc
                $ fmt_if parens "(" $ str "module type of " $ pro )
          ; epi= Some epi }
      | _ ->
          { blk with
            bdy=
              Cmts.fmt c pmty_loc
              @@ hvbox 2
                   (fmt_if parens "(" $ fmt "module type of@ " $ blk.bdy)
          ; epi= Some epi } )
  | Pmty_extension ext ->
      { empty with
        bdy= fmt_extension c ctx "%" ext
      ; epi=
          Some (fmt_attributes c ~key:"@" pmty_attributes ~pre:(fmt "@ "))
      }
  | Pmty_alias lid ->
      { empty with
        bdy= fmt_longident_loc c lid
      ; epi=
          Some (fmt_attributes c ~key:"@" pmty_attributes ~pre:(fmt "@ "))
      }

and fmt_signature c ctx itms =
  let update_config c i =
    match i.psig_desc with
    | Psig_attribute atr -> update_config c [atr]
    | _ -> c
  in
  let grps = make_groups c itms (fun x -> Sig x) update_config in
  let fmt_grp (i, c) =
    maybe_disabled c i.psig_loc []
    @@ fun c -> fmt_signature_item c (sub_sig ~ctx i)
  in
  let fmt_grp itms = list itms "@\n" fmt_grp in
  hvbox 0 (list grps "\n@;<1000 0>" fmt_grp)

and fmt_signature_item c ?ext {ast= si; _} =
  protect (Sig si)
  @@
  let epi = fmt "\n@\n" and eol = fmt "\n@\n" and adj = fmt "@\n" in
  let fmt_cmts_before = Cmts.fmt_before c ~epi ~eol ~adj si.psig_loc in
  let maybe_box =
    Location.is_single_line si.psig_loc c.conf.margin
    && Source.has_cmt_same_line_after c.source si.psig_loc
  in
  let pro = fmt_or maybe_box "@ " "\n@\n" in
  let fmt_cmts_after = Cmts.fmt_after ~pro c si.psig_loc in
  (fun k -> fmt_cmts_before $ hvbox_if maybe_box 0 (k $ fmt_cmts_after))
  @@
  let ctx = Sig si in
  match si.psig_desc with
  | Psig_attribute atr ->
      let doc, atrs = doc_atrs [atr] in
      fmt_docstring c ~standalone:true ~epi:noop doc
      $ fmt_attributes c ~key:"@@@" atrs
  | Psig_exception exc ->
      hvbox 2
        (fmt_type_exception ~pre:(fmt "exception@ ") c (fmt " of@ ") ctx exc)
  | Psig_extension (ext, atrs) ->
      hvbox c.conf.stritem_extension_indent
        ( fmt_extension c ctx "%%" ext
        $ fmt_attributes c ~pre:(fmt "@ ") ~key:"@@" atrs )
  | Psig_include {pincl_mod; pincl_attributes; pincl_loc} ->
      update_config_maybe_disabled c pincl_loc pincl_attributes
      @@ fun c ->
      let doc_before, doc_after, atrs =
        let force_before = not (Ast.module_type_is_simple pincl_mod) in
        fmt_docstring_around_item c ~force_before ~fit:true pincl_attributes
      in
      let keyword, {opn; pro; psp; bdy; cls; esp; epi} =
        match pincl_mod with
        | {pmty_desc= Pmty_typeof me; pmty_loc; pmty_attributes= _} ->
            ( str "include"
              $ Cmts.fmt c ~pro:(str " ") ~epi:noop pmty_loc
                  (fmt "@ module type of")
            , fmt_module_expr c (sub_mod ~ctx me) )
        | _ -> (str "include", fmt_module_type c (sub_mty ~ctx pincl_mod))
      in
      let box = wrap_k opn cls in
      hvbox 0
        ( doc_before
        $ ( box
              ( hvbox 2 (keyword $ opt pro (fun pro -> str " " $ pro))
              $ fmt_or_k (Option.is_some pro) psp (fmt "@;<1 2>")
              $ bdy )
          $ esp $ Option.call ~f:epi
          $ fmt_attributes c ~pre:(fmt "@ ") ~key:"@@" atrs )
        $ doc_after )
  | Psig_modtype mtd -> fmt_module_type_declaration c ctx mtd
  | Psig_module md ->
      hvbox 0 (fmt_module_declaration c ctx ~rec_flag:false ~first:true md)
  | Psig_modsubst ms -> hvbox 0 (fmt_module_substitution c ctx ms)
  | Psig_open od -> fmt_open_description c ~kw_attributes:[] od
  | Psig_recmodule mds ->
      fmt_recmodule c ctx mds fmt_module_declaration (fun x ->
          Mty x.pmd_type)
  | Psig_type (rec_flag, decls) -> fmt_type c ?ext rec_flag decls ctx
  | Psig_typext te -> fmt_type_extension c ctx te
  | Psig_value vd -> fmt_value_description c ctx vd
  | Psig_class cl -> fmt_class_types c ctx ~pre:"class" ~sep:":" cl
  | Psig_class_type cl ->
      fmt_class_types c ctx ~pre:"class type" ~sep:"=" cl
  | Psig_typesubst decls -> fmt_type c ?ext ~eq:":=" Recursive decls ctx

and fmt_class_types c ctx ~pre ~sep (cls : class_type class_infos list) =
  list_fl cls (fun ~first ~last:_ cl ->
      update_config_maybe_disabled c cl.pci_loc cl.pci_attributes
      @@ fun c ->
      let doc_before, doc_after, atrs =
        let force_before = not (Ast.class_type_is_simple cl.pci_expr) in
        fmt_docstring_around_item ~force_before c cl.pci_attributes
      in
      let class_types =
        hovbox 2
          ( hvbox 2
              ( str (if first then pre else "and")
              $ fmt_if Poly.(cl.pci_virt = Virtual) "@ virtual"
              $ fmt "@ "
              $ fmt_class_params c ctx cl.pci_params
              $ fmt_str_loc c cl.pci_name $ fmt "@ " $ str sep )
          $ fmt "@;"
          $ fmt_class_type c (sub_cty ~ctx cl.pci_expr)
          $ fmt_attributes c ~pre:(fmt "@;") ~key:"@@" atrs )
      in
      fmt_if (not first) "\n@\n"
      $ hovbox 0
        @@ Cmts.fmt c cl.pci_loc (doc_before $ class_types $ doc_after))

and fmt_class_exprs c ctx (cls : class_expr class_infos list) =
  list_fl cls (fun ~first ~last:_ cl ->
      update_config_maybe_disabled c cl.pci_loc cl.pci_attributes
      @@ fun c ->
      let xargs, xbody =
        match cl.pci_expr.pcl_attributes with
        | [] ->
            Sugar.cl_fun c.cmts ~will_keep_first_ast_node:false
              (sub_cl ~ctx cl.pci_expr)
        | _ -> ([], sub_cl ~ctx cl.pci_expr)
      in
      let ty, e =
        match xbody.ast with
        | {pcl_desc= Pcl_constraint (e, t); _} -> (Some t, sub_cl ~ctx e)
        | _ -> (None, xbody)
      in
      let doc_before, doc_after, atrs =
        let force_before = not (Ast.class_decl_is_simple cl.pci_expr) in
        fmt_docstring_around_item ~force_before c cl.pci_attributes
      in
      let class_exprs =
        hovbox 2
          ( hovbox 2
              ( box_fun_decl_args c 2
                  ( hovbox 2
                      ( str (if first then "class" else "and")
                      $ fmt_if Poly.(cl.pci_virt = Virtual) "@ virtual"
                      $ fmt "@ "
                      $ fmt_class_params c ctx cl.pci_params
                      $ fmt_str_loc c cl.pci_name )
                  $ fmt_if (not (List.is_empty xargs)) "@ "
                  $ wrap_fun_decl_args c (fmt_fun_args c xargs) )
              $ opt ty (fun t ->
                    fmt " :@ " $ fmt_class_type c (sub_cty ~ctx t))
              $ fmt "@ =" )
          $ fmt "@;" $ fmt_class_expr c e )
        $ fmt_attributes c ~pre:(fmt "@;") ~key:"@@" atrs
      in
      fmt_if (not first) "\n@\n"
      $ hovbox 0
        @@ Cmts.fmt c cl.pci_loc (doc_before $ class_exprs $ doc_after))

and fmt_module c ?epi ?(can_sparse = false) keyword ?(eqty = "=") name xargs
    xbody xmty attributes =
  let f (name, xarg) = (name, Option.map ~f:(fmt_module_type c) xarg) in
  let arg_blks = List.map xargs ~f in
  let blk_t =
    Option.value_map xmty ~default:empty ~f:(fun xmty ->
        let blk = fmt_module_type c xmty in
        { blk with
          pro=
            Some
              (str " " $ str eqty $ opt blk.pro (fun pro -> str " " $ pro))
        ; psp= fmt_if (Option.is_none blk.pro) "@;<1 2>" $ blk.psp })
  in
  let blk_b =
    Option.value_map xbody ~default:empty ~f:(fmt_module_expr c)
  in
  let box_t = wrap_k blk_t.opn blk_t.cls in
  let box_b = wrap_k blk_b.opn blk_b.cls in
  let fmt_arg ?prev:_ (name, arg_mtyp) ?next =
    let maybe_box k =
      match arg_mtyp with Some {pro= None; _} -> hvbox 0 k | _ -> k
    in
    fmt "@ "
    $ maybe_box
        (wrap "(" ")"
           ( fmt_str_loc c name
           $ opt arg_mtyp (fun {pro; psp; bdy; cls; esp; epi; opn= _} ->
                 (* TODO: handle opn *)
                 str " : "
                 $ opt pro (fun pro -> pro $ close_box)
                 $ psp $ bdy
                 $ fmt_if_k (Option.is_some pro) cls
                 $ esp
                 $ ( match next with
                   | Some (_, Some {opn; pro= Some _; _}) ->
                       opn $ open_hvbox 0
                   | _ -> noop )
                 $ Option.call ~f:epi) ))
  in
  let single_line =
    Option.for_all xbody ~f:(fun x -> module_expr_is_simple x.ast)
    && Option.for_all xmty ~f:(fun x -> module_type_is_simple x.ast)
  in
  let compact = Poly.(c.conf.let_module = `Compact) || not can_sparse in
  let fmt_pro = opt blk_b.pro (fun pro -> fmt "@ " $ pro) in
  let doc_before, doc_after, atrs =
    fmt_docstring_around_item c ~force_before:(not single_line) ~fit:true
      attributes
  in
  hvbox
    (if compact then 0 else 2)
    ( doc_before
    $ box_b
        ( (if Option.is_some blk_t.epi then hovbox else hvbox)
            0
            ( box_t
                ( hvbox_if
                    (Option.is_some blk_t.pro)
                    0
                    ( ( match arg_blks with
                      | (_, Some {opn; pro= Some _; _}) :: _ ->
                          opn $ open_hvbox 0
                      | _ -> noop )
                    $ hvbox 4
                        ( keyword $ str " " $ fmt_str_loc c name
                        $ list_pn arg_blks fmt_arg )
                    $ Option.call ~f:blk_t.pro )
                $ blk_t.psp $ blk_t.bdy )
            $ blk_t.esp $ Option.call ~f:blk_t.epi
            $ fmt_if (Option.is_some xbody) " ="
            $ fmt_if_k compact fmt_pro )
        $ fmt_if_k (not compact) fmt_pro
        $ blk_b.psp
        $ fmt_if (Option.is_none blk_b.pro && Option.is_some xbody) "@ "
        $ blk_b.bdy )
    $ blk_b.esp $ Option.call ~f:blk_b.epi
    $ fmt_attributes c ~pre:(fmt "@ ") ~key:"@@" atrs
    $ doc_after
    $ opt epi (fun epi ->
          fmt_or_k compact
            (fmt_or
               (Option.is_some blk_b.epi && not c.conf.ocp_indent_compat)
               " " "@ ")
            (fmt "@;<1 -2>")
          $ epi) )

and fmt_module_declaration c ctx ~rec_flag ~first pmd =
  let {pmd_name; pmd_type; pmd_attributes; pmd_loc} = pmd in
  update_config_maybe_disabled c pmd_loc pmd_attributes
  @@ fun c ->
  let keyword =
    if first then if rec_flag then str "module rec" else str "module"
    else str "and"
  in
  let xargs, xmty =
    if rec_flag then ([], sub_mty ~ctx pmd_type)
    else
      Sugar.functor_type c.cmts ~for_functor_kw:false
        (sub_mty ~ctx pmd_type)
  in
  let eqty =
    match xmty.ast.pmty_desc with Pmty_alias _ -> None | _ -> Some ":"
  in
  Cmts.fmt c pmd_loc
    (fmt_module c keyword pmd_name xargs None ?eqty (Some xmty)
       pmd_attributes)

and fmt_module_substitution c ctx pms =
  let {pms_name; pms_manifest; pms_attributes; pms_loc} = pms in
  update_config_maybe_disabled c pms_loc pms_attributes
  @@ fun c ->
  let xmty =
    (* TODO: improve *)
    sub_mty ~ctx
      { pmty_desc= Pmty_ident pms_manifest
      ; pmty_loc= pms_loc
      ; pmty_attributes= [] }
  in
  Cmts.fmt c pms_loc
    (fmt_module c (str "module") ~eqty:":=" pms_name [] None (Some xmty)
       pms_attributes)

and fmt_module_type_declaration c ctx pmtd =
  let {pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc} = pmtd in
  update_config_maybe_disabled c pmtd_loc pmtd_attributes
  @@ fun c ->
  fmt_module c (str "module type") pmtd_name [] None
    (Option.map pmtd_type ~f:(sub_mty ~ctx))
    pmtd_attributes

and fmt_open_description c ?(keyword = "open") ~kw_attributes
    {popen_expr= popen_lid; popen_override; popen_attributes; popen_loc} =
  update_config_maybe_disabled c popen_loc popen_attributes
  @@ fun c ->
  let doc_before, doc_after, atrs =
    fmt_docstring_around_item ~fit:true c popen_attributes
  in
  hovbox 0
    ( doc_before $ str keyword
    $ fmt_if Poly.(popen_override = Override) "!"
    $ Cmts.fmt_before c popen_loc
    $ fmt_attributes c ~key:"@" kw_attributes
    $ str " "
    $ fmt_longident_loc c popen_lid
    $ fmt_attributes c ~pre:(str " ") ~key:"@@" atrs
    $ Cmts.fmt_after c popen_loc
    $ doc_after )

(** TODO: merge with `fmt_module_declaration` *)
and fmt_module_statement c ~attributes keyword mod_expr =
  let blk = fmt_module_expr c mod_expr in
  let box = wrap_k blk.opn blk.cls in
  let force_before = not (module_expr_is_simple mod_expr.ast) in
  let doc_before, doc_after, atrs =
    fmt_docstring_around_item ~force_before ~fit:true c attributes
  in
  doc_before
  $ box (hvbox 2 (keyword $ Option.call ~f:blk.pro) $ blk.psp $ blk.bdy)
  $ blk.esp $ Option.call ~f:blk.epi
  $ fmt_attributes c ~pre:(str " ") ~key:"@@" atrs
  $ doc_after

and fmt_with_constraint c ctx = function
  | Pwith_type (ident, td) ->
      fmt_type_declaration ~pre:" type" c ctx
        ~fmt_name:(fmt_longident_loc c ident)
        td
  | Pwith_module (m1, m2) ->
      str " module " $ fmt_longident_loc c m1 $ str " = "
      $ fmt_longident_loc c m2
  | Pwith_typesubst (lid, td) ->
      fmt_type_declaration ~pre:" type" c ~eq:":=" ctx
        ~fmt_name:(fmt_longident_loc c lid) td
  | Pwith_modsubst (m1, m2) ->
      str " module " $ fmt_longident_loc c m1 $ str " := "
      $ fmt_longident_loc c m2

and maybe_generative c ~ctx = function
  | {pmod_desc= Pmod_structure []; pmod_attributes= []; pmod_loc}
    when not (Cmts.has_within c.cmts pmod_loc) ->
      empty
  | m -> fmt_module_expr c (sub_mod ~ctx m)

and fmt_module_expr ?(can_break_before_struct = false) c
    ({ast= m; _} as xmod) =
  let ctx = Mod m in
  let {pmod_desc; pmod_loc; pmod_attributes} = m in
  update_config_maybe_disabled_block c pmod_loc pmod_attributes
  @@ fun c ->
  let parens = parenze_mod xmod in
  match pmod_desc with
  | Pmod_apply (({pmod_desc= Pmod_ident _; _} as me_f), me_a) ->
      let doc, atrs = doc_atrs pmod_attributes in
      let blk_f = fmt_module_expr c (sub_mod ~ctx me_f) in
      let blk_a = maybe_generative c ~ctx me_a in
      let box_f = wrap_k blk_f.opn blk_f.cls in
      let fmt_rator =
        fmt_docstring c ~epi:(fmt "@,") doc
        $ box_f (blk_f.psp $ Option.call ~f:blk_f.pro $ blk_f.bdy)
        $ blk_f.esp $ Option.call ~f:blk_f.epi
        $ fmt_or_k
            ( c.conf.break_struct && can_break_before_struct
            && not (module_expr_is_simple me_a) )
            (break_unless_newline 1000 0 $ str "(")
            (fmt "@ (")
      in
      let epi =
        Option.call ~f:blk_a.epi $ str ")"
        $ fmt_attributes c ~pre:(str " ") ~key:"@" atrs
        $ Cmts.fmt_after c pmod_loc
      in
      if Option.is_some blk_a.pro then
        { blk_a with
          pro=
            Some
              ( Cmts.fmt_before c pmod_loc
              $ hvbox 2 fmt_rator $ Option.call ~f:blk_a.pro )
        ; epi= Some epi }
      else
        { blk_a with
          opn= open_hvbox 2 $ blk_a.opn
        ; bdy=
            Cmts.fmt_before c pmod_loc
            $ open_hvbox 2 $ fmt_rator $ blk_a.bdy
        ; cls= close_box $ blk_a.cls $ close_box
        ; epi= Some epi }
  | Pmod_apply (me_f, me_a) ->
      let can_break_before_struct =
        match me_f.pmod_desc with Pmod_apply _ -> true | _ -> false
      in
      let doc, atrs = doc_atrs pmod_attributes in
      let blk_f =
        fmt_module_expr ~can_break_before_struct c (sub_mod ~ctx me_f)
      in
      let blk_a = maybe_generative c ~ctx me_a in
      let has_epi =
        Cmts.has_after c.cmts pmod_loc || not (List.is_empty atrs)
      in
      { empty with
        opn= blk_a.opn $ blk_f.opn $ open_hvbox 2
      ; bdy=
          hvbox 2
            ( Cmts.fmt_before c pmod_loc
            $ fmt_docstring c ~epi:(fmt "@,") doc
            $ wrap_if parens "(" ")"
                ( Option.call ~f:blk_f.pro $ blk_f.psp $ blk_f.bdy
                $ blk_f.esp )
            $ Option.call ~f:blk_f.epi
            $ wrap "@ (" ")"
                ( Option.call ~f:blk_a.pro $ blk_a.psp $ blk_a.bdy
                $ blk_a.esp $ Option.call ~f:blk_a.epi ) )
      ; cls= close_box $ blk_f.cls $ blk_a.cls
      ; epi=
          Option.some_if has_epi
            ( Cmts.fmt_after c pmod_loc
            $ fmt_attributes c ~pre:(str " ") ~key:"@" atrs ) }
  | Pmod_constraint (me, mt) ->
      let doc, atrs = doc_atrs pmod_attributes in
      let blk_e = fmt_module_expr c (sub_mod ~ctx me) in
      let blk_t = fmt_module_type c (sub_mty ~ctx mt) in
      let has_epi =
        Cmts.has_after c.cmts pmod_loc || not (List.is_empty atrs)
      in
      { opn= blk_t.opn $ blk_e.opn $ open_hovbox 2
      ; pro=
          Some
            ( Cmts.fmt_before c pmod_loc
            $ fmt_docstring c ~epi:(fmt "@,") doc
            $ str "(" )
      ; psp= fmt "@,"
      ; bdy=
          ( hvbox 0
              ( Option.call ~f:blk_e.pro $ blk_e.psp $ blk_e.bdy $ blk_e.esp
              $ Option.call ~f:blk_e.epi $ fmt " :@;<1 2>"
              $ hvbox 0
                  ( Option.call ~f:blk_t.pro $ blk_t.psp $ blk_t.bdy
                  $ blk_t.esp $ Option.call ~f:blk_t.epi ) )
          $
          match c.conf.indicate_multiline_delimiters with
          | `Space -> fits_breaks ")" " )"
          | `No -> str ")"
          | `Closing_on_separate_line ->
              fits_breaks ")" ~hint:(1000, -2) ")" )
      ; cls= close_box $ blk_e.cls $ blk_t.cls
      ; esp= noop
      ; epi=
          Option.some_if has_epi
            ( Cmts.fmt_after c pmod_loc
            $ fmt_attributes c ~pre:(str " ") ~key:"@" atrs ) }
  | Pmod_functor _ ->
      let xargs, me = sugar_pmod_functor c ~for_functor_kw:true xmod in
      let doc, atrs = doc_atrs pmod_attributes in
      let {opn; pro; psp; bdy; cls; esp; epi} = fmt_module_expr c me in
      { empty with
        opn
      ; bdy=
          Cmts.fmt c pmod_loc
            ( fmt_docstring c ~epi:(fmt "@,") doc
            $ hvbox 0
                (wrap_if parens "(" ")"
                   ( str "functor"
                   $ fmt_attributes c ~pre:(str " ") ~key:"@" atrs
                   $ fmt "@;<1 2>"
                   $ list xargs "@;<1 2>" (fmt_functor_arg c)
                   $ fmt "@;<1 2>->@;<1 2>"
                   $ hvbox 0
                       ( Option.call ~f:pro $ psp $ bdy $ esp
                       $ Option.call ~f:epi ) )) )
      ; cls }
  | Pmod_ident lid ->
      let doc, atrs = doc_atrs pmod_attributes in
      let has_pro = Cmts.has_before c.cmts pmod_loc || Option.is_some doc in
      let has_epi =
        Cmts.has_after c.cmts pmod_loc || not (List.is_empty atrs)
      in
      { empty with
        opn= open_hvbox 2
      ; pro=
          Option.some_if has_pro
            ( Cmts.fmt_before c pmod_loc
            $ fmt_docstring c ~epi:(fmt "@,") doc )
      ; bdy= fmt_longident_loc c lid
      ; cls= close_box
      ; epi=
          Option.some_if has_epi
            ( Cmts.fmt_after c pmod_loc
            $ fmt_attributes c ~pre:(str " ") ~key:"@" atrs ) }
  | Pmod_structure sis ->
      let empty =
        List.is_empty sis && not (Cmts.has_within c.cmts pmod_loc)
      in
      let doc, atrs = doc_atrs pmod_attributes in
      let before = Cmts.fmt_before c pmod_loc in
      let within = Cmts.fmt_within c ~pro:noop pmod_loc in
      let after = Cmts.fmt_after c pmod_loc in
      { opn= noop
      ; pro=
          Some
            ( before
            $ fmt_docstring c ~epi:(fmt "@,") doc
            $ str "struct" $ fmt_if empty " " )
      ; psp=
          fmt_if_k (not empty)
            (fmt_or c.conf.break_struct "@;<1000 2>" "@;<1 2>")
      ; bdy= within $ fmt_structure c ctx sis
      ; cls= noop
      ; esp=
          fmt_if_k (not empty)
            (fmt_or c.conf.break_struct "@;<1000 0>" "@;<1 0>")
      ; epi=
          Some
            ( str "end" $ after
            $ fmt_attributes c ~pre:(fmt "@ ") ~key:"@" atrs ) }
  | Pmod_unpack
      { pexp_desc=
          Pexp_constraint
            ( e1
            , { ptyp_desc= Ptyp_package (id, cnstrs)
              ; ptyp_attributes= []
              ; ptyp_loc= _
              ; _ } )
      ; pexp_attributes= []
      ; pexp_loc= _
      ; _ } ->
      (* TODO: handle ptyp_loc and pexp_loc *)
      let doc, atrs = doc_atrs pmod_attributes in
      let has_epi =
        Cmts.has_after c.cmts pmod_loc || not (List.is_empty atrs)
      in
      { empty with
        pro=
          Some
            ( Cmts.fmt_before c pmod_loc
            $ fmt_docstring c ~epi:(fmt "@,") doc )
      ; bdy=
          Cmts.fmt c pmod_loc
          @@ hovbox 0
               (wrap_fits_breaks ~space:false c.conf "(" ")"
                  (hvbox 2
                     ( hovbox 0
                         ( str "val "
                         $ fmt_expression c (sub_exp ~ctx e1)
                         $ fmt "@;<1 2>: " $ fmt_longident_loc c id )
                     $ fmt_package_type c ctx cnstrs )))
      ; epi=
          Option.some_if has_epi
            ( Cmts.fmt_after c pmod_loc
            $ fmt_attributes c ~pre:(str " ") ~key:"@" atrs ) }
  | Pmod_unpack e1 ->
      let doc, atrs = doc_atrs pmod_attributes in
      let has_epi =
        Cmts.has_after c.cmts pmod_loc || not (List.is_empty atrs)
      in
      let has_pro = Cmts.has_before c.cmts pmod_loc || Option.is_some doc in
      { empty with
        pro=
          Option.some_if has_pro
            ( Cmts.fmt_before c pmod_loc
            $ fmt_docstring c ~epi:(fmt "@,") doc )
      ; bdy=
          Cmts.fmt c pmod_loc
          @@ hvbox 2
               (wrap_fits_breaks ~space:false c.conf "(" ")"
                  (str "val " $ fmt_expression c (sub_exp ~ctx e1)))
      ; epi=
          Option.some_if has_epi
            ( Cmts.fmt_after c pmod_loc
            $ fmt_attributes c ~pre:(str " ") ~key:"@" atrs ) }
  | Pmod_extension x1 ->
      let doc, atrs = doc_atrs pmod_attributes in
      let has_pro = Cmts.has_before c.cmts pmod_loc || Option.is_some doc in
      let has_epi =
        Cmts.has_after c.cmts pmod_loc || not (List.is_empty atrs)
      in
      { empty with
        pro=
          Option.some_if has_pro
            ( Cmts.fmt_before c pmod_loc
            $ fmt_docstring c ~epi:(fmt "@,") doc )
      ; bdy= Cmts.fmt c pmod_loc @@ fmt_extension c ctx "%" x1
      ; epi=
          Option.some_if has_epi
            ( Cmts.fmt_after c pmod_loc
            $ fmt_attributes c ~pre:(str " ") ~key:"@" atrs ) }

and fmt_structure c ctx itms =
  let update_config c i =
    match i.pstr_desc with
    | Pstr_attribute atr -> update_config c [atr]
    | _ -> c
  in
  let grps = make_groups c itms (fun x -> Str x) update_config in
  let break_struct = c.conf.break_struct || Poly.(ctx = Top) in
  let fmt_grp ~first:_ ~last:last_grp itms =
    list_fl itms (fun ~first ~last (itm, c) ->
        let last = last && last_grp in
        fmt_if_k (not first) (fmt_or break_struct "@\n" "@ ")
        $ maybe_disabled c itm.pstr_loc []
          @@ fun c -> fmt_structure_item c ~last (sub_str ~ctx itm))
  in
  hvbox 0 (fmt_groups c ctx grps fmt_grp)

and fmt_type c ?ext ?eq rec_flag decls ctx =
  let fmt_decl ~first ~last decl =
    let pre =
      if first then
        if Poly.(rec_flag = Recursive) then "type" else "type nonrec"
      else "and"
    and brk = fmt_if (not last) "\n" in
    let ext = if first then ext else None in
    fmt_type_declaration c ~pre ?eq ?ext ~brk ctx decl
    $ fmt_if (not last) "@ "
  in
  vbox 0 (list_fl decls fmt_decl)

and fmt_structure_item c ~last:last_item ?ext {ctx; ast= si} =
  protect (Str si)
  @@
  let skip_double_semi =
    match ctx with Pld (PStr [_]) -> true | _ -> false
  in
  let ctx = Str si in
  let epi = fmt "\n@\n" and eol = fmt "\n@\n" and adj = fmt "@\n" in
  let fmt_cmts_before = Cmts.fmt_before c ~epi ~eol ~adj si.pstr_loc in
  let maybe_box =
    Location.is_single_line si.pstr_loc c.conf.margin
    && Source.has_cmt_same_line_after c.source si.pstr_loc
  in
  let pro = fmt_or maybe_box "@ " "\n@\n" in
  let fmt_cmts_after = Cmts.fmt_after ~pro c si.pstr_loc in
  (fun k -> fmt_cmts_before $ hvbox_if maybe_box 0 (k $ fmt_cmts_after))
  @@
  match si.pstr_desc with
  | Pstr_attribute atr ->
      let doc, atrs = doc_atrs [atr] in
      fmt_docstring c ~standalone:true ~epi:noop doc
      $ fmt_attributes c ~key:"@@@" atrs
  | Pstr_eval (exp, atrs) ->
      let doc, atrs = doc_atrs atrs in
      fmt_if (not skip_double_semi) ";;@\n"
      $ fmt_docstring c doc
      $ cbox 0 (fmt_expression c (sub_exp ~ctx exp))
      $ fmt_attributes c ~pre:(str " ") ~key:"@@" atrs
  | Pstr_exception extn_constr ->
      hvbox 2
        (fmt_type_exception ~pre:(fmt "exception@ ") c (str ": ") ctx
           extn_constr)
  | Pstr_include {pincl_mod; pincl_attributes= attributes; pincl_loc} ->
      update_config_maybe_disabled c pincl_loc attributes
      @@ fun c ->
      fmt_module_statement c ~attributes (str "include ")
        (sub_mod ~ctx pincl_mod)
  | Pstr_module binding ->
      fmt_module_binding c ctx ~rec_flag:false ~first:true binding
  | Pstr_open
      {popen_expr; popen_override; popen_attributes= attributes; popen_loc}
    ->
      update_config_maybe_disabled c popen_loc attributes
      @@ fun c ->
      let keyword =
        str "open"
        $ fmt_if (Poly.equal popen_override Override) "!"
        $ str " "
      in
      fmt_module_statement c ~attributes keyword (sub_mod ~ctx popen_expr)
  | Pstr_primitive vd -> fmt_value_description c ctx vd
  | Pstr_recmodule bindings ->
      fmt_recmodule c ctx bindings fmt_module_binding (fun x ->
          Mod x.pmb_expr)
  | Pstr_type (rec_flag, decls) -> fmt_type c ?ext rec_flag decls ctx
  | Pstr_typext te -> fmt_type_extension c ctx te
  | Pstr_value (rec_flag, bindings) ->
      let with_conf c b =
        let c = update_config ~quiet:true c b.pvb_attributes in
        (c, (b, c))
      in
      let _, bindings = List.fold_map bindings ~init:c ~f:with_conf in
      let break (itmI, cI) (itmJ, cJ) =
        (not (List.is_empty itmI.pvb_attributes))
        || (not (List.is_empty itmJ.pvb_attributes))
        || Ast.break_between c.source ~cmts:c.cmts
             ~has_cmts_before:Cmts.has_before ~has_cmts_after:Cmts.has_after
             (Exp itmI.pvb_expr, cI.conf)
             (Exp itmJ.pvb_expr, cJ.conf)
      in
      let grps = List.group bindings ~break in
      let fmt_grp ~first:first_grp ~last:last_grp bindings =
        list_fl bindings (fun ~first ~last (binding, c) ->
            let epi =
              match c.conf.let_binding_spacing with
              | `Compact -> None
              | `Sparse ->
                  let force_fit_if = last && last_grp && last_item in
                  Some (fits_breaks ~force_fit_if "" "\n")
              | `Double_semicolon ->
                  Option.some_if (last && last_grp)
                    (fits_breaks "" ~hint:(1000, -2) ";;")
            in
            let {pvb_pat; pvb_expr; pvb_attributes= attributes; pvb_loc= loc}
                =
              binding
            in
            let op = if first && first_grp then "let" else "and" in
            let rec_flag =
              first && first_grp && Poly.(rec_flag = Recursive)
            in
            fmt_if (not first) "@\n"
            $ fmt_value_binding c op ~rec_flag
                ?ext:(if first && first_grp then ext else None)
                ctx ?epi ~attributes ~loc pvb_pat pvb_expr)
      in
      hvbox 0
        (list_fl grps (fun ~first ~last grp ->
             fmt_grp ~first ~last grp $ fmt_if (not last) "\n@\n"))
  | Pstr_modtype mtd -> fmt_module_type_declaration c ctx mtd
  | Pstr_extension (ext, atrs) ->
      let doc, atrs = doc_atrs atrs in
      fmt_docstring c doc
      $ fmt_extension c ctx "%%" ext
      $ fmt_attributes c ~pre:(str " ") ~key:"@@" atrs
  | Pstr_class_type cl ->
      fmt_class_types c ctx ~pre:"class type" ~sep:"=" cl
  | Pstr_class cls -> fmt_class_exprs c ctx cls

and fmt_let c ctx ~ext ~rec_flag ~bindings ~parens ~fmt_atrs ~fmt_expr ~loc
    ~attributes ~indent_after_in =
  let fmt_in indent =
    match c.conf.break_before_in with
    | `Fit_or_vertical -> break 1 (-indent) $ fmt "in"
    | `Auto -> fits_breaks " in" ~hint:(1, -indent) "in"
  in
  let fmt_binding ~first ~last binding =
    let ext = if first then ext else None in
    let in_ indent = fmt_if_k last (fmt_in indent) in
    let {pvb_pat; pvb_expr; pvb_attributes= attributes; pvb_loc= loc} =
      binding
    in
    let op = if first then "let" else "and" in
    let rec_flag = first && Poly.(rec_flag = Recursive) in
    fmt_value_binding c op ~rec_flag ?ext ctx ~in_ ~attributes ~loc pvb_pat
      pvb_expr
    $ fmt_if (not last)
        ( match c.conf.let_and with
        | `Sparse -> "@;<1000 0>"
        | `Compact -> "@ " )
  in
  wrap_exp_if c ~loc
    ~parens:(parens || not (List.is_empty attributes))
    (vbox 0
       ( hvbox 0 (list_fl bindings fmt_binding)
       $ break 1000 indent_after_in
       $ hvbox 0 fmt_expr ))
  $ fmt_atrs

and fmt_let_op c ctx ~ext ~parens ~fmt_atrs ~fmt_expr bindings
    ~indent_after_in =
  let fmt_binding ~first ~last binding =
    let ext = if first then ext else None in
    let in_ indent = fmt_if_k last (break 1 (-indent) $ str "in") in
    let {pbop_op= {txt= op; _}; pbop_pat; pbop_exp; pbop_loc= loc} =
      binding
    in
    fmt_value_binding c op ~rec_flag:false ?ext ~in_ ctx ~attributes:[] ~loc
      pbop_pat pbop_exp
    $ fmt_if (not last)
        ( match c.conf.let_and with
        | `Sparse -> "@;<1000 0>"
        | `Compact -> "@ " )
  in
  wrap_if parens "(" ")"
    (vbox 0
       ( hvbox 0 (list_fl bindings fmt_binding)
       $ break 1000 indent_after_in
       $ hvbox 0 fmt_expr ))
  $ fmt_atrs

and fmt_value_binding c let_op ~rec_flag ?ext ?in_ ?epi ctx ~attributes ~loc
    pvb_pat pvb_expr =
  update_config_maybe_disabled c loc attributes
  @@ fun c ->
  let doc1, atrs = doc_atrs attributes in
  let doc2, atrs = doc_atrs atrs in
  let xpat, xargs, fmt_cstr, xbody =
    let ({ast= pat; _} as xpat) =
      match (pvb_pat.ppat_desc, pvb_expr.pexp_desc) with
      (* recognize and undo the pattern of code introduced by
         ocaml/ocaml@fd0dc6a0fbf73323c37a73ea7e8ffc150059d6ff to fix
         https://caml.inria.fr/mantis/view.php?id=7344 *)
      | ( Ppat_constraint
            ( ({ppat_desc= Ppat_var _; _} as pat)
            , {ptyp_desc= Ptyp_poly ([], typ1); _} )
        , Pexp_constraint (_, typ2) )
        when Poly.(typ1 = typ2) ->
          Cmts.relocate c.cmts ~src:pvb_pat.ppat_loc ~before:pat.ppat_loc
            ~after:pat.ppat_loc ;
          sub_pat ~ctx:(Pat pvb_pat) pat
      | _ -> sub_pat ~ctx pvb_pat
    in
    let pat_is_extension {ppat_desc; _} =
      match ppat_desc with Ppat_extension _ -> true | _ -> false
    in
    let ({ast= body; _} as xbody) = sub_exp ~ctx pvb_expr in
    if
      (not (List.is_empty xbody.ast.pexp_attributes))
      || pat_is_extension pat
    then (xpat, [], None, xbody)
    else
      match Sugar.polynewtype c.cmts pat body with
      | Some (xpat, pvars, xtyp, xbody) ->
          let fmt_cstr =
            fmt_or c.conf.ocp_indent_compat "@ : " " :@ "
            $ hvbox 0
                ( fmt "type "
                $ list pvars " " (fmt_str_loc c)
                $ fmt ".@ " $ fmt_core_type c xtyp )
          in
          (xpat, [], Some fmt_cstr, xbody)
      | None ->
          let xpat =
            match xpat.ast.ppat_desc with
            | Ppat_constraint (p, {ptyp_desc= Ptyp_poly ([], _); _}) ->
                sub_pat ~ctx:xpat.ctx p
            | _ -> xpat
          in
          let xargs, ({ast= body; _} as xbody) =
            match pat with
            | {ppat_desc= Ppat_var _; ppat_attributes= []; _} ->
                Sugar.fun_ c.cmts ~will_keep_first_ast_node:false xbody
            | _ -> ([], xbody)
          in
          let fmt_cstr, xbody =
            let ctx = Exp body in
            let fmt_cstr_and_xbody typ exp =
              ( Some
                  ( fmt_or_k c.conf.ocp_indent_compat
                      (fits_breaks " " ~hint:(1000, 0) "")
                      (fmt "@;<0 -1>")
                  $ cbox_if c.conf.ocp_indent_compat 0
                      (fmt_core_type c ~pro:":"
                         ~pro_space:(not c.conf.ocp_indent_compat)
                         ~box:(not c.conf.ocp_indent_compat)
                         (sub_typ ~ctx typ)) )
              , sub_exp ~ctx exp )
            in
            match (body.pexp_desc, pat.ppat_desc) with
            | ( Pexp_constraint
                  ( ({pexp_desc= Pexp_pack _; pexp_attributes= []; _} as exp)
                  , ( {ptyp_desc= Ptyp_package _; ptyp_attributes= []; _} as
                    typ ) )
              , _ )
              when Poly.(Source.typed_expression typ exp = `Type_first) ->
                Cmts.relocate c.cmts ~src:body.pexp_loc ~before:exp.pexp_loc
                  ~after:exp.pexp_loc ;
                fmt_cstr_and_xbody typ exp
            | ( Pexp_constraint
                  ( {pexp_desc= Pexp_pack _; _}
                  , {ptyp_desc= Ptyp_package _; _} )
              , _ )
             |Pexp_constraint _, Ppat_constraint _ ->
                (None, xbody)
            | Pexp_constraint (exp, typ), _
              when Poly.(Source.typed_expression typ exp = `Type_first) ->
                Cmts.relocate c.cmts ~src:body.pexp_loc ~before:exp.pexp_loc
                  ~after:exp.pexp_loc ;
                fmt_cstr_and_xbody typ exp
            | _ -> (None, xbody)
          in
          (xpat, xargs, fmt_cstr, xbody)
  in
  let indent =
    match xbody.ast.pexp_desc with
    | Pexp_function _ ->
        function_indent c ~ctx ~default:c.conf.let_binding_indent
    | Pexp_fun _ -> c.conf.let_binding_indent - 1
    | _ -> c.conf.let_binding_indent
  in
  let f {attr_name= {loc; _}; _} =
    Location.compare_start loc pvb_expr.pexp_loc < 1
  in
  let at_attrs, at_at_attrs = List.partition_tf atrs ~f in
  let pre_body, body = fmt_body c xbody in
  let pat_has_cmt = Cmts.has_before c.cmts xpat.ast.ppat_loc in
  fmt_docstring c ~epi:(fmt "@\n") doc1
  $ Cmts.fmt_before c loc
  $ hvbox indent
      ( hovbox 2
          ( hovbox 4
              ( box_fun_decl_args c 4
                  ( hovbox 4
                      ( str let_op
                      $ fmt_extension_suffix c ext
                      $ fmt_attributes c ~key:"@" at_attrs
                      $ fmt_if rec_flag " rec"
                      $ fmt_or pat_has_cmt "@ " " "
                      $ fmt_pattern c xpat )
                  $ fmt_if_k
                      (not (List.is_empty xargs))
                      ( fmt "@ "
                      $ wrap_fun_decl_args c (fmt_fun_args c xargs) ) )
              $ Option.call ~f:fmt_cstr )
          $ fmt_or_k c.conf.ocp_indent_compat
              (fits_breaks " =" ~hint:(1000, 0) "=")
              (fmt "@;<1 2>=")
          $ pre_body )
      $ fmt "@ " $ body $ Cmts.fmt_after c loc
      $ fmt_attributes c ~pre:(fmt "@;") ~key:"@@" at_at_attrs
      $ (match in_ with Some in_ -> in_ indent | None -> noop)
      $ Option.call ~f:epi )
  $ fmt_docstring c ~pro:(fmt "@\n") doc2

and fmt_module_binding c ctx ~rec_flag ~first pmb =
  update_config_maybe_disabled c pmb.pmb_loc pmb.pmb_attributes
  @@ fun c ->
  let keyword =
    if first then if rec_flag then str "module rec" else str "module"
    else str "and"
  in
  let xargs, xbody =
    sugar_pmod_functor c ~for_functor_kw:false (sub_mod ~ctx pmb.pmb_expr)
  in
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
    (fmt_module c keyword ~eqty:":" pmb.pmb_name xargs (Some xbody) xmty
       pmb.pmb_attributes)

let fmt_toplevel_phrase c ctx = function
  | Ptop_def structure -> fmt_structure c ctx structure
  | Ptop_dir {pdir_name= name; pdir_arg= directive_argument; pdir_loc} ->
      let cmts_before = Cmts.fmt_before c pdir_loc in
      let cmts_after = Cmts.fmt_after c pdir_loc in
      let name = fmt_str_loc c name ~pre:(str "#") in
      let args =
        match directive_argument with
        | None -> noop
        | Some {pdira_desc; pdira_loc; _} ->
            str " "
            $ Cmts.fmt_before c pdira_loc
            $ ( match pdira_desc with
              | Pdir_string s -> str (Printf.sprintf "%S" s)
              | Pdir_int (lit, Some m) -> str (Printf.sprintf "%s%c" lit m)
              | Pdir_int (lit, None) -> str lit
              | Pdir_ident longident -> fmt_longident longident
              | Pdir_bool bool -> str (Bool.to_string bool) )
            $ Cmts.fmt_after c pdira_loc
      in
      cmts_before $ fmt ";;@\n" $ name $ args $ cmts_after

let fmt_use_file c ctx itms = list itms "\n@\n" (fmt_toplevel_phrase c ctx)

(** Entry points *)

let fmt_file ~ctx ~f source cmts conf itms =
  let c = {source; cmts; conf} in
  Ast.init c.conf ;
  match itms with [] -> Cmts.fmt_after c Location.none | l -> f c ctx l

let fmt_signature = fmt_file ~f:fmt_signature ~ctx:Top

let fmt_structure_in_cmt src cmts c s =
  fmt_file ~f:fmt_structure ~ctx:(Pld (PStr s)) src cmts c s

let fmt_structure = fmt_file ~f:fmt_structure ~ctx:Top

let fmt_use_file = fmt_file ~f:fmt_use_file ~ctx:Top
