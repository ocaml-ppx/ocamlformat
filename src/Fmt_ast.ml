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

type block =
  { opn: Fmt.t
  ; pro: Fmt.t option
  ; psp: Fmt.t
  ; bdy: Fmt.t
  ; cls: Fmt.t
  ; esp: Fmt.t
  ; epi: Fmt.t option }

let empty =
  { opn= Fn.const ()
  ; pro= None
  ; psp= Fn.const ()
  ; bdy= Fn.const ()
  ; cls= Fn.const ()
  ; esp= Fn.const ()
  ; epi= None }

(* Debug: catch and report failures at nearest enclosing Ast.t *)

let protect =
  let first = ref true in
  fun ast pp fs ->
    try pp fs with exc ->
      if !first && Conf.debug then (
        let bt = Caml.Printexc.get_backtrace () in
        Format.pp_print_flush fs () ;
        Caml.Format.eprintf "@\nFAIL@\n%a@\n%s@.%!" Ast.dump ast bt ;
        first := false ) ;
      raise exc

let update_config ?(quiet = false) c l =
  let update_one c ({txt; loc}, payload) =
    let result =
      match txt with
      | "ocamlformat" -> (
        match payload with
        | PStr
            [ { pstr_desc=
                  Pstr_eval
                    ( { pexp_desc= Pexp_constant (Pconst_string (str, None))
                      ; pexp_attributes= [] }
                    , [] ) } ] ->
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
        if (not c.conf.quiet) && not quiet then
          !Location.warning_printer loc Caml.Format.err_formatter w ;
        c
  in
  List.fold ~init:c l ~f:update_one

let fmt_expressions c width sub_exp exprs fmt fmt_expr =
  match c.conf.break_collection_expressions with
  | `Fit_or_vertical -> list exprs fmt fmt_expr
  | `Wrap ->
      let grps =
        List.group exprs ~break:(fun x1 x2 ->
            (not (is_simple c.conf width (sub_exp x1)))
            || not (is_simple c.conf width (sub_exp x2)) )
      in
      let fmt_grp exprs = hovbox (-2) (list exprs "@,; " fmt_expr) in
      list grps fmt fmt_grp

let drop_while ~f s =
  let i = ref 0 in
  while !i < String.length s && f !i s.[!i] do
    Int.incr i
  done ;
  String.sub s ~pos:!i ~len:(String.length s - !i)

let maybe_disabled_k c (loc : Location.t) l f k =
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
                Char.is_whitespace c && i < indent_of_first_line ) )
    in
    k (list l "@\n" (fun s -> str s))

let maybe_disabled c loc l f = maybe_disabled_k c loc l f Fn.id

let update_config_maybe_disabled c loc l f =
  let c = update_config c l in
  maybe_disabled c loc l f

let update_config_maybe_disabled_block c loc l f =
  let c = update_config c l in
  maybe_disabled_k c loc l f (fun bdy ->
      {empty with opn= open_vbox 2; bdy; cls= close_box} )

let fmt_recmodule c ctx items f ast =
  let _, items =
    List.fold_map items ~init:c ~f:(fun c i ->
        let c = update_config c (Ast.attributes (ast i)) in
        (c, (i, c)) )
  in
  let grps =
    List.group items ~break:(fun (i1, c1) (i2, c2) ->
        Ast.break_between c.cmts (ast i1, c1.conf) (ast i2, c2.conf) )
  in
  let break_struct = c.conf.break_struct || Poly.(ctx = Top) in
  let fmt_grp ~first:first_grp itms =
    list_fl itms (fun ~first ~last:_ (itm, c) ->
        fmt_if_k (not first) (fmt_or break_struct "@\n" "@ ")
        $ maybe_disabled c (Ast.location (ast itm)) []
          @@ fun c -> f c ctx ~rec_flag:true ~first:(first && first_grp) itm
    )
  in
  hvbox 0
    (list_fl grps (fun ~first ~last grp ->
         fmt_if (break_struct && not first) "\n@\n"
         $ fmt_if ((not break_struct) && not first) "@;<1000 0>"
         $ fmt_grp ~first grp
         $ fits_breaks_if ((not break_struct) && not last) "" "\n" ))

let rec sugar_arrow_typ c ({ast= typ} as xtyp) =
  let ctx = Typ typ in
  let {ptyp_desc; ptyp_loc} = typ in
  match ptyp_desc with
  | Ptyp_arrow (l, t1, t2) ->
      Cmts.relocate c.cmts ~src:ptyp_loc ~before:t1.ptyp_loc
        ~after:t2.ptyp_loc ;
      let rest =
        match t2.ptyp_attributes with
        | [] -> sugar_arrow_typ c (sub_typ ~ctx t2)
        | _ -> [(Nolabel, sub_typ ~ctx t2)]
      in
      (l, sub_typ ~ctx t1) :: rest
  | _ -> [(Nolabel, xtyp)]

let rec sugar_class_arrow_typ c ({ast= typ} as xtyp) =
  let ctx = Cty typ in
  let {pcty_desc; pcty_loc} = typ in
  match pcty_desc with
  | Pcty_arrow (l, t1, t2) ->
      Cmts.relocate c.cmts ~src:pcty_loc ~before:t1.ptyp_loc
        ~after:t2.pcty_loc ;
      let rest =
        match t2.pcty_attributes with
        | [] -> sugar_class_arrow_typ c (sub_cty ~ctx t2)
        | _ -> [(Nolabel, `class_type (sub_cty ~ctx t2))]
      in
      (l, `core_type (sub_typ ~ctx t1)) :: rest
  | _ -> [(Nolabel, `class_type xtyp)]

let rec sugar_or_pat ?(allow_attribute = true) c ({ast= pat} as xpat) =
  let ctx = Pat pat in
  match pat with
  | {ppat_desc= Ppat_or (pat1, pat2); ppat_loc; ppat_attributes= []} ->
      Cmts.relocate c.cmts ~src:ppat_loc ~before:pat1.ppat_loc
        ~after:pat2.ppat_loc ;
      sugar_or_pat ~allow_attribute:false c (sub_pat ~ctx pat1)
      @ sugar_or_pat ~allow_attribute:false c (sub_pat ~ctx pat2)
  | {ppat_desc= Ppat_or (pat1, pat2); ppat_loc} when allow_attribute ->
      Cmts.relocate c.cmts ~src:ppat_loc ~before:pat1.ppat_loc
        ~after:pat2.ppat_loc ;
      [sub_pat ~ctx pat1; sub_pat ~ctx pat2]
  | _ -> [xpat]

type arg_kind =
  | Val of arg_label * pattern xt * expression xt option
  | Newtypes of string loc list

let sugar_fun c ?(will_keep_first_ast_node = true) xexp =
  let rec sugar_fun_ ?(will_keep_first_ast_node = false) ({ast= exp} as xexp)
      =
    let ctx = Exp exp in
    let {pexp_desc; pexp_loc; pexp_attributes} = exp in
    if will_keep_first_ast_node || List.is_empty pexp_attributes then
      match pexp_desc with
      | Pexp_fun (label, default, pattern, body) ->
          if not will_keep_first_ast_node then
            Cmts.relocate c.cmts ~src:pexp_loc ~before:pattern.ppat_loc
              ~after:body.pexp_loc ;
          let xargs, xbody = sugar_fun_ (sub_exp ~ctx body) in
          ( Val
              ( label
              , sub_pat ~ctx pattern
              , Option.map default ~f:(sub_exp ~ctx) )
            :: xargs
          , xbody )
      | Pexp_newtype (name, body) ->
          if not will_keep_first_ast_node then
            Cmts.relocate c.cmts ~src:pexp_loc ~before:body.pexp_loc
              ~after:body.pexp_loc ;
          let xargs, xbody = sugar_fun_ (sub_exp ~ctx body) in
          let xargs =
            match xargs with
            | Newtypes names :: xargs -> Newtypes (name :: names) :: xargs
            | xargs -> Newtypes [name] :: xargs
          in
          (xargs, xbody)
      | _ -> ([], xexp)
    else ([], xexp)
  in
  sugar_fun_ ~will_keep_first_ast_node xexp

let sugar_cl_fun ?(will_keep_first_ast_node = true) c xexp =
  let rec sugar_fun_ ?(will_keep_first_ast_node = false) ({ast= exp} as xexp)
      =
    let ctx = Cl exp in
    let {pcl_desc; pcl_loc; pcl_attributes} = exp in
    if will_keep_first_ast_node || List.is_empty pcl_attributes then
      match pcl_desc with
      | Pcl_fun (label, default, pattern, body) ->
          if not will_keep_first_ast_node then
            Cmts.relocate c.cmts ~src:pcl_loc ~before:pattern.ppat_loc
              ~after:body.pcl_loc ;
          let xargs, xbody = sugar_fun_ (sub_cl ~ctx body) in
          ( Val
              ( label
              , sub_pat ~ctx pattern
              , Option.map default ~f:(sub_exp ~ctx) )
            :: xargs
          , xbody )
      | _ -> ([], xexp)
    else ([], xexp)
  in
  sugar_fun_ ~will_keep_first_ast_node xexp

let sugar_infix c prec xexp =
  let assoc = Option.value_map prec ~default:Non ~f:assoc_of_prec in
  let rec sugar_infix_ ?(relocate = true) xop ((lbl, {ast= exp}) as xexp) =
    assert (Poly.(lbl = Nolabel)) ;
    let ctx = Exp exp in
    match (assoc, exp) with
    | Left, {pexp_desc= Pexp_apply (e0, [(l1, e1); (l2, e2)]); pexp_loc}
      when Poly.(prec = prec_ast (Exp exp)) ->
        let op_args1 = sugar_infix_ None (l1, sub_exp ~ctx e1) in
        let src = pexp_loc in
        let after = e2.pexp_loc in
        ( match op_args1 with
        | (Some {ast= {pexp_loc= before}}, _) :: _
         |(None, (_, {ast= {pexp_loc= before}}) :: _) :: _ ->
            if relocate then Cmts.relocate c.cmts ~src ~before ~after
        | _ ->
            if relocate then
              Cmts.relocate c.cmts ~src ~before:e0.pexp_loc ~after ) ;
        op_args1 @ [(Some (sub_exp ~ctx e0), [(l2, sub_exp ~ctx e2)])]
    | Right, {pexp_desc= Pexp_apply (e0, [(l1, e1); (l2, e2)]); pexp_loc}
      when Poly.(prec = prec_ast (Exp exp)) ->
        let op_args2 =
          sugar_infix_ (Some (sub_exp ~ctx e0)) (l2, sub_exp ~ctx e2)
        in
        let src = pexp_loc in
        let after = e1.pexp_loc in
        ( match List.last op_args2 with
        | Some (_, args2) -> (
          match List.last args2 with
          | Some (_, {ast= {pexp_loc= after}}) -> (
            match xop with
            | Some {ast} ->
                if relocate then
                  Cmts.relocate c.cmts ~src ~before:ast.pexp_loc ~after
            | None ->
                if relocate then
                  Cmts.relocate c.cmts ~src ~before:e1.pexp_loc ~after )
          | None -> (
            match xop with
            | Some {ast} ->
                if relocate then
                  Cmts.relocate c.cmts ~src ~before:ast.pexp_loc ~after
            | None ->
                if relocate then
                  Cmts.relocate c.cmts ~src ~before:e1.pexp_loc ~after ) )
        | _ -> (
          match xop with
          | Some {ast} ->
              if relocate then
                Cmts.relocate c.cmts ~src ~before:ast.pexp_loc ~after
          | None ->
              if relocate then
                Cmts.relocate c.cmts ~src ~before:e1.pexp_loc ~after ) ) ;
        (xop, [(l1, sub_exp ~ctx e1)]) :: op_args2
    | _ -> [(xop, [xexp])]
  in
  sugar_infix_ None ~relocate:false (Nolabel, xexp)

let rec sugar_list_pat c pat =
  let ctx = Pat pat in
  let {ppat_desc; ppat_loc= src} = pat in
  match ppat_desc with
  | Ppat_construct ({txt= Lident "[]"; loc}, None) ->
      Cmts.relocate c.cmts ~src ~before:loc ~after:loc ;
      Some ([], loc)
  | Ppat_construct
      ( {txt= Lident "::"; loc}
      , Some {ppat_desc= Ppat_tuple [hd; tl]; ppat_loc; ppat_attributes= []}
      ) -> (
    match sugar_list_pat c tl with
    | Some (xtl, nil_loc) when List.is_empty tl.ppat_attributes ->
        Some (([src; loc; ppat_loc], sub_pat ~ctx hd) :: xtl, nil_loc)
    | _ -> None )
  | _ -> None

let sugar_list_exp c exp =
  let rec sugar_list_exp_ exp =
    let ctx = Exp exp in
    let {pexp_desc; pexp_loc= src} = exp in
    match pexp_desc with
    | Pexp_construct ({txt= Lident "[]"; loc}, None) ->
        Cmts.relocate c.cmts ~src ~before:loc ~after:loc ;
        Some ([], loc)
    | Pexp_construct
        ( {txt= Lident "::"; loc}
        , Some
            {pexp_desc= Pexp_tuple [hd; tl]; pexp_loc; pexp_attributes= []}
        ) -> (
      match sugar_list_exp_ tl with
      | Some (xtl, nil_loc) when List.is_empty tl.pexp_attributes ->
          Some (([src; loc; pexp_loc], sub_exp ~ctx hd) :: xtl, nil_loc)
      | _ -> None )
    | _ -> None
  in
  let r = sugar_list_exp_ exp in
  assert (Bool.equal (Option.is_some r) (is_sugared_list exp)) ;
  r

let sugar_infix_cons xexp =
  let rec sugar_infix_cons_ ({ast= exp} as xexp) =
    let ctx = Exp exp in
    let {pexp_desc; pexp_loc= l1} = exp in
    match pexp_desc with
    | Pexp_construct
        ( {txt= Lident "::"; loc= l2}
        , Some
            { pexp_desc= Pexp_tuple [hd; tl]
            ; pexp_loc= l3
            ; pexp_attributes= [] } ) -> (
      match sugar_infix_cons_ (sub_exp ~ctx tl) with
      | xtl when List.is_empty tl.pexp_attributes ->
          ([l1; l2; l3], sub_exp ~ctx hd) :: xtl
      | _ -> [([l1; l2; l3], sub_exp ~ctx hd); ([], sub_exp ~ctx tl)] )
    | _ -> [([], xexp)]
  in
  sugar_infix_cons_ xexp

let rec sugar_ite c ({ast= exp} as xexp) =
  let ctx = Exp exp in
  let {pexp_desc; pexp_loc; pexp_attributes} = exp in
  match pexp_desc with
  | Pexp_ifthenelse (cnd, thn, Some els) ->
      Cmts.relocate c.cmts ~src:pexp_loc ~before:cnd.pexp_loc
        ~after:els.pexp_loc ;
      (Some (sub_exp ~ctx cnd), sub_exp ~ctx thn, pexp_attributes)
      :: sugar_ite c (sub_exp ~ctx els)
  | Pexp_ifthenelse (cnd, thn, None) ->
      Cmts.relocate c.cmts ~src:pexp_loc ~before:cnd.pexp_loc
        ~after:thn.pexp_loc ;
      [(Some (sub_exp ~ctx cnd), sub_exp ~ctx thn, pexp_attributes)]
  | _ -> [(None, xexp, pexp_attributes)]

let sugar_sequence c width xexp =
  let rec sugar_sequence_ ?(allow_attribute = true) ({ast= exp} as xexp) =
    let ctx = Exp exp in
    let {pexp_desc; pexp_loc} = exp in
    match pexp_desc with
    | Pexp_sequence (e1, e2) ->
        Cmts.relocate c.cmts ~src:pexp_loc ~before:e1.pexp_loc
          ~after:e2.pexp_loc ;
        if (not allow_attribute) && not (List.is_empty exp.pexp_attributes)
        then [xexp]
        else if Ast.exposed_right_exp Ast.Let_match e1 then
          [sub_exp ~ctx e1; sub_exp ~ctx e2]
        else
          List.append
            (sugar_sequence_ ~allow_attribute:false (sub_exp ~ctx e1))
            (sugar_sequence_ ~allow_attribute:false (sub_exp ~ctx e2))
    | _ -> [xexp]
  in
  List.group (sugar_sequence_ xexp) ~break:(fun xexp1 xexp2 ->
      (not (is_simple c.conf width xexp1))
      || not (is_simple c.conf width xexp2) )

(* The sugar is different when used with the [functor] keyword. The syntax
   M(A : A)(B : B) cannot handle [_] as module name. *)
let rec sugar_functor_type c ~for_functor_kw ({ast= mty} as xmty) =
  let ctx = Mty mty in
  match mty with
  | {pmty_desc= Pmty_functor (arg, arg_mty, body); pmty_loc; pmty_attributes}
    when for_functor_kw
         || (List.is_empty pmty_attributes && not (String.equal arg.txt "_"))
    ->
      let arg =
        if String.equal "*" arg.txt then {arg with txt= ""} else arg
      in
      Cmts.relocate c.cmts ~src:pmty_loc ~before:arg.loc
        ~after:body.pmty_loc ;
      let body = sub_mty ~ctx body in
      let xargs, xbody =
        match pmty_attributes with
        | [] -> sugar_functor_type c ~for_functor_kw body
        | _ -> ([], body)
      in
      ((arg, Option.map arg_mty ~f:(sub_mty ~ctx)) :: xargs, xbody)
  | _ -> ([], xmty)

(* The sugar is different when used with the [functor] keyword. The syntax
   M(A : A)(B : B) cannot handle [_] as module name. *)
let rec sugar_functor c ~for_functor_kw ({ast= me} as xme) =
  let ctx = Mod me in
  match me with
  | {pmod_desc= Pmod_functor (arg, arg_mt, body); pmod_loc; pmod_attributes}
    when for_functor_kw
         || (List.is_empty pmod_attributes && not (String.equal arg.txt "_"))
    ->
      let arg =
        if String.equal "*" arg.txt then {arg with txt= ""} else arg
      in
      Cmts.relocate c.cmts ~src:pmod_loc ~before:arg.loc
        ~after:body.pmod_loc ;
      let xarg_mt = Option.map arg_mt ~f:(sub_mty ~ctx) in
      let ctx = Mod body in
      let body = sub_mod ~ctx body in
      let xargs, xbody_me =
        match pmod_attributes with
        | [] -> sugar_functor c ~for_functor_kw body
        | _ -> ([], body)
      in
      ((arg, xarg_mt) :: xargs, xbody_me)
  | _ -> ([], xme)

let sugar_mod_with c pmty =
  let rec sugar_mod_with_ c ({ast= me} as xme) =
    let ctx = Mty me in
    match me with
    | {pmty_desc= Pmty_with (mt, wcs); pmty_attributes; pmty_loc} ->
        let args, rest =
          match pmty_attributes with
          | [] -> sugar_mod_with_ c (sub_mty ~ctx mt)
          | _ -> ([], sub_mty ~ctx mt)
        in
        ((wcs, pmty_loc) :: args, rest)
    | _ -> ([], xme)
  in
  let l_rev, m = sugar_mod_with_ c pmty in
  (List.rev l_rev, m)

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

let fmt_longident_loc c ?(pre = ("" : _ format))
    ({txt; loc} : Longident.t loc) =
  Cmts.fmt c.cmts loc @@ (fmt pre $ fmt_longident txt)

let fmt_str_loc c ?(pre = ("" : _ format)) ({txt; loc} : string loc) =
  Cmts.fmt c.cmts loc @@ (fmt pre $ str txt)

let fmt_char_escaped c ~loc chr =
  match (c.conf.escape_chars, chr) with
  | `Hexadecimal, _ ->
      fun fs -> Format.fprintf fs "\\x%02x" (Char.to_int chr)
  | `Preserve, _ -> str (Source.char_literal c.source loc)
  | _, '\000' .. '\128' -> str (Char.escaped chr)
  | `Decimal, _ -> str (Char.escaped chr)

let escape_string c str =
  match c.conf.escape_strings with
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
  | Pconst_char x -> wrap "'" "'" @@ fmt_char_escaped ~loc c x
  | Pconst_string (s, Some delim) ->
      str ("{" ^ delim ^ "|") $ str s $ str ("|" ^ delim ^ "}")
  | Pconst_string (s, None) -> (
      let fmt_line s =
        match c.conf.break_string_literals with
        | `Wrap ->
            let words = String.split (escape_string c s) ~on:' ' in
            hovbox_if
              (match words with [] | [_] -> false | _ -> true)
              0
              (list_pn words (fun ?prev:_ curr ?next ->
                   str curr
                   $
                   match (curr, next) with
                   | _, Some "" -> str " "
                   | _, Some _ -> pre_break 1 " \\" 0
                   | _ -> fmt "" ))
        | _ -> str (escape_string c s)
      in
      let fmt_lines lines =
        hvbox 1
          ( str "\""
          $ list_pn lines (fun ?prev:_ curr ?next ->
                fmt_line curr
                $ opt next (fun next ->
                      if String.is_empty next then fmt "\\n"
                      else if Char.equal next.[0] ' ' then
                        fmt "\\n" $ pre_break 0 "\\" (-1) $ if_newline "\\"
                      else fmt "\\n" $ pre_break 0 "\\" 0 ) )
          $ str "\"" $ Option.call ~f:epi )
      in
      let s =
        match (c.conf.break_string_literals, c.conf.escape_strings) with
        | `Never, `Preserve -> Source.string_literal c.source `Preserve loc
        | (`Newlines | `Wrap), `Preserve ->
            Source.string_literal c.source `Normalize loc
        | _ -> s
      in
      match c.conf.break_string_literals with
      | `Newlines | `Wrap -> fmt_lines (String.split ~on:'\n' s)
      | `Never -> str "\"" $ fmt_line s $ str "\"" )

let fmt_variance = function
  | Covariant -> fmt "+"
  | Contravariant -> fmt "-"
  | Invariant -> fmt ""

let break_cases_level c =
  match c.conf.break_cases with `Fit -> 0 | `Nested -> 1 | `All -> 2

let wrap_list c =
  if c.conf.space_around_collection_expressions then wrap "[ " "@ ]"
  else wrap_fits_breaks c.conf "[" "]"

let wrap_array c =
  if c.conf.space_around_collection_expressions then wrap "[| " "@ |]"
  else wrap_fits_breaks c.conf "[|" "|]"

let wrap_record c =
  if c.conf.space_around_collection_expressions then wrap "{ " "@ }"
  else wrap_fits_breaks c.conf "{" "}"

let doc_atrs = Ast.doc_atrs

let fmt_docstring c ?(standalone = false) ?pro ?epi doc =
  list_pn (Option.value ~default:[] doc)
    (fun ?prev:_ ({txt; loc}, floating) ?next ->
      let need_break =
        (not standalone)
        &&
        match (next, floating) with
        | None, true -> true
        | Some (_, true), true -> false
        | Some (_, false), true -> true
        | _, false -> false
      in
      let try_parse str_cmt =
        if not c.conf.parse_docstrings then
          (if c.conf.wrap_comments then fill_text else str) str_cmt
        else
          match Octavius.parse (Lexing.from_string str_cmt) with
          | Error _ ->
              (if c.conf.wrap_comments then fill_text else str) str_cmt
          | Ok parsed ->
              if Conf.debug then (
                Octavius.print Caml.Format.str_formatter parsed ;
                Caml.Format.eprintf "%s%!"
                  (Caml.Format.flush_str_formatter ()) ) ;
              let space_i i =
                let spaces = ['\t'; '\n'; '\011'; '\012'; '\r'; ' '] in
                let is_space = List.mem ~equal:Char.equal spaces in
                0 <= i && i < String.length str_cmt && is_space str_cmt.[i]
              in
              fmt_if (space_i 0) " "
              $ Fmt_odoc.fmt parsed
              $ fmt_if (space_i (String.length str_cmt - 1)) " "
      in
      Cmts.fmt c.cmts loc
      @@ vbox_if (Option.is_none pro) 0
           ( Option.call ~f:pro $ fmt "(**" $ try_parse txt $ fmt "*)"
           $ fmt_if need_break "\n"
           $ fmt_or_k (Option.is_some next) (fmt "@\n") (Option.call ~f:epi)
           ) )

let fmt_extension_suffix c ext =
  opt ext (fun name -> str "%" $ fmt_str_loc c name)

let field_alias ~field:(li1 : Longident.t) (li2 : Longident.t) =
  match (li1, li2) with
  | Ldot (_, x), Lident y -> String.equal x y
  | Lident x, Lident y -> String.equal x y
  | _ -> false

let rec fmt_attribute c pre = function
  | ( {txt= ("ocaml.doc" | "ocaml.text") as txt; loc= {loc_ghost= true}}
    , PStr
        [ { pstr_desc=
              Pstr_eval
                ( { pexp_desc= Pexp_constant (Pconst_string (doc, None))
                  ; pexp_attributes= [] }
                , [] ) } ] ) ->
      fmt_or (String.equal txt "ocaml.text") "@ " " "
      $ fmt "(**" $ str doc $ fmt "*)"
  | ({loc} as name), pld ->
      let cmts_last =
        match pld with
        | PStr [] -> Cmts.fmt_after c.cmts loc
        | PStr [{pstr_desc= Pstr_eval ({pexp_loc; _}, []); pstr_loc; _}] ->
            Cmts.fmt_after c.cmts pexp_loc $ Cmts.fmt_after c.cmts pstr_loc
        | _ -> fmt ""
      in
      let protect_token =
        match pld with PTyp t -> exposed_right_typ t | _ -> false
      in
      Cmts.fmt_before c.cmts loc
      $ hvbox 2
          (wrap "[" "]"
             ( str pre $ fmt_str_loc c name
             $ fmt_payload c (Pld pld) pld
             $ fmt_if protect_token " " ))
      $ cmts_last

and fmt_extension c ctx key (({loc} as ext), pld) =
  match (pld, ctx) with
  | PStr [({pstr_desc= Pstr_value _; _} as si)], (Pld _ | Str _ | Top) ->
      fmt_structure_item c ~last:true ~ext (sub_str ~ctx si)
  | _ ->
      let cmts_last =
        match pld with
        | PStr [] -> Cmts.fmt_after c.cmts loc
        | PStr [{pstr_desc= Pstr_eval ({pexp_loc; _}, []); pstr_loc; _}] ->
            Cmts.fmt_after c.cmts pexp_loc $ Cmts.fmt_after c.cmts pstr_loc
        | _ -> fmt ""
      in
      let protect_token =
        match pld with PTyp t -> exposed_right_typ t | _ -> false
      in
      Cmts.fmt_before c.cmts loc
      $ wrap "[" "]"
          ( str key $ fmt_str_loc c ext
          $ fmt_payload c (Pld pld) pld
          $ fmt_if protect_token " " )
      $ cmts_last

and fmt_attributes c ?(pre = fmt "") ?(suf = fmt "") ?(box = true) ~key
    attrs =
  let num = List.length attrs in
  if num = 0 then fmt ""
  else
    let split = num > 1 in
    let box = split && box in
    pre
    $ hvbox_if box 0
        (list_fl attrs (fun ~first ~last atr ->
             fmt_or_k first (open_hvbox 0) (fmt "@ ")
             $ fmt_attribute c key atr
             $ fmt_if_k last (close_box $ suf) ))

and fmt_payload c ctx pld =
  protect (Pld pld)
  @@
  match pld with
  | PStr mex ->
      fmt_if (not (List.is_empty mex)) "@ " $ fmt_structure c ctx mex
  | PSig mty -> fmt ":@ " $ fmt_signature c ctx mty
  | PTyp typ -> fmt ":@ " $ fmt_core_type c (sub_typ ~ctx typ)
  | PPat (pat, exp) ->
      fmt "?@ "
      $ fmt_pattern c (sub_pat ~ctx pat)
      $ opt exp (fun exp ->
            fmt " when " $ fmt_expression c (sub_exp ~ctx exp) )

and fmt_core_type c ?(box = true) ?(in_type_declaration = false) ?pro
    ({ast= typ} as xtyp) =
  protect (Typ typ)
  @@
  let {ptyp_desc; ptyp_attributes; ptyp_loc} = typ in
  update_config_maybe_disabled c ptyp_loc ptyp_attributes
  @@ fun c ->
  ( match (ptyp_desc, pro) with
  | Ptyp_arrow _, Some _ when c.conf.ocp_indent_compat -> fmt "@,"
  | _, Some pro -> str pro $ fmt "@ "
  | _ -> fmt "" )
  $
  let doc, atrs = doc_atrs ptyp_attributes in
  Cmts.fmt c.cmts ptyp_loc
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
  | Ptyp_any -> fmt "_"
  | Ptyp_arrow _ ->
      let arg_label lbl =
        match lbl with
        | Nolabel -> fmt ""
        | Labelled l -> str l $ fmt ":"
        | Optional l -> fmt "?" $ str l $ fmt ":"
      in
      let xt1N = sugar_arrow_typ c xtyp in
      hvbox_if box 0
        ( ( match pro with
          | Some pro when c.conf.ocp_indent_compat ->
              str pro
              $ fits_breaks " "
                  (String.make (Int.max 1 (3 - String.length pro)) ' ')
          | _ -> fits_breaks "" "   " )
        $ list xt1N "@ -> " (fun (lI, xtI) ->
              hvbox 0 (arg_label lI $ fmt_core_type c xtI) ) )
  | Ptyp_constr (lid, []) -> fmt_longident_loc c lid
  | Ptyp_constr (lid, [t1]) ->
      fmt_core_type c (sub_typ ~ctx t1) $ fmt "@ " $ fmt_longident_loc c lid
  | Ptyp_constr (lid, t1N) ->
      wrap_fits_breaks c.conf "(" ")"
        (list t1N "@,, " (sub_typ ~ctx >> fmt_core_type c))
      $ fmt "@ " $ fmt_longident_loc c lid
  | Ptyp_extension ext -> hvbox 2 (fmt_extension c ctx "%" ext)
  | Ptyp_package pty -> hvbox 0 (fmt "module@ " $ fmt_package_type c ctx pty)
  | Ptyp_poly ([], _) ->
      impossible "produced by the parser, handled elsewhere"
  | Ptyp_poly (a1N, t) ->
      hovbox_if box 0
        ( list a1N "@ " (fun ty -> fmt_str_loc c ~pre:"'" ty)
        $ fmt ".@ "
        $ fmt_core_type c ~box:false (sub_typ ~ctx t) )
  | Ptyp_tuple typs ->
      hvbox 0
        (wrap_fits_breaks_if ~space:false c.conf parens "(" ")"
           (list typs "@ * " (sub_typ ~ctx >> fmt_core_type c)))
  | Ptyp_var s ->
      fmt "'"
      (* [' a'] is a valid type variable, the space is required to not lex
         as a char. https://github.com/ocaml/ocaml/pull/2034 *)
      $ fmt_if (String.length s > 1 && Char.equal s.[1] '\'') " "
      $ str s
  | Ptyp_variant (rfs, flag, lbls) ->
      let row_fields rfs =
        match rfs with
        | [] -> Cmts.fmt_within c.cmts ~pro:(fmt "") ptyp_loc
        | _ ->
            list rfs
              ( if in_type_declaration && Poly.(c.conf.type_decl = `Sparse)
              then "@;<1000 0>| "
              else "@ | " )
              (fmt_row_field c ctx)
      in
      let protect_token =
        match List.last rfs with
        | None -> false
        | Some (Rinherit _) -> false
        | Some (Rtag (_, _, _, l)) -> (
          match List.last l with
          | None -> false
          | Some x -> exposed_right_typ x )
      in
      hvbox 0
        ( fits_breaks "[" "["
        $ ( match (flag, lbls, rfs) with
          | Closed, None, [Rinherit _] -> fmt " | " $ row_fields rfs
          | Closed, None, _ -> fits_breaks "" " " $ row_fields rfs
          | Open, None, _ -> fmt "> " $ row_fields rfs
          | Closed, Some [], _ -> fmt "< " $ row_fields rfs
          | Closed, Some ls, _ ->
              fmt "< " $ row_fields rfs $ fmt " > "
              $ list ls "@ " (fmt "`" >$ str)
          | Open, Some _, _ -> impossible "not produced by parser" )
        $ fits_breaks (if protect_token then " ]" else "]") "@ ]" )
  | Ptyp_object ([], o_c) ->
      fmt "<@ "
      $ fmt_if Poly.(o_c = Open) "..@ "
      $ Cmts.fmt_within c.cmts ~pro:(fmt "") ptyp_loc
      $ fmt ">"
  | Ptyp_object (fields, closedness) ->
      hvbox 0
        (wrap "< " " >"
           ( list fields "@ ; " (function
               | Otag (lab_loc, attrs, typ) ->
                   (* label loc * attributes * core_type -> object_field *)
                   let doc, atrs = doc_atrs attrs in
                   let fmt_cmts = Cmts.fmt c.cmts lab_loc.loc in
                   fmt_cmts
                   @@ hvbox 4
                        ( hvbox 2
                            ( fmt_str_loc c lab_loc
                            $ fmt_if Poly.(c.conf.field_space = `Loose) " "
                            $ fmt ":@ "
                            $ fmt_core_type c (sub_typ ~ctx typ) )
                        $ fmt_docstring c ~pro:(fmt "@;<2 0>") doc
                        $ fmt_attributes c ~pre:(fmt " ") ~key:"@" atrs )
               | Oinherit typ -> fmt_core_type c (sub_typ ~ctx typ) )
           $ fmt_if Poly.(closedness = Open) "@ ; .." ))
  | Ptyp_class (lid, []) -> fmt_longident_loc c ~pre:"#" lid
  | Ptyp_class (lid, [t1]) ->
      fmt_core_type c (sub_typ ~ctx t1)
      $ fmt "@ "
      $ fmt_longident_loc c ~pre:"#" lid
  | Ptyp_class (lid, t1N) ->
      wrap_fits_breaks c.conf "(" ")"
        (list t1N "@,, " (sub_typ ~ctx >> fmt_core_type c))
      $ fmt "@ "
      $ fmt_longident_loc c ~pre:"#" lid )
  $ fmt_docstring c ~pro:(fmt "@ ") doc

and fmt_package_type c ctx (lid, cnstrs) =
  fmt_longident_loc c lid
  $ fmt_if (not (List.is_empty cnstrs)) "@;<1 2>"
  $ hvbox 0
      (list_fl cnstrs (fun ~first ~last:_ (lid, typ) ->
           fmt_or first "with type " "@;<1 1>and type "
           $ fmt_longident_loc c lid $ fmt " = "
           $ fmt_core_type c (sub_typ ~ctx typ) ))

and fmt_row_field c ctx = function
  | Rtag (name, atrs, const, typs) ->
      let c = update_config c atrs in
      let doc, atrs = doc_atrs atrs in
      hvbox 0
        ( fmt_str_loc c ~pre:"`" name
        $ fmt_if (not (const && List.is_empty typs)) " of@ "
        $ fmt_if (const && not (List.is_empty typs)) " & "
        $ list typs "@ & " (sub_typ ~ctx >> fmt_core_type c)
        $ fmt_attributes c ~key:"@" atrs
        $ fmt_docstring c ~pro:(fmt "@;<2 0>") doc )
  | Rinherit typ -> fmt_core_type c (sub_typ ~ctx typ)

and fmt_pattern c ?pro ?parens ({ctx= ctx0; ast= pat} as xpat) =
  protect (Pat pat)
  @@
  let ctx = Pat pat in
  let {ppat_desc; ppat_attributes; ppat_loc} = pat in
  update_config_maybe_disabled c ppat_loc ppat_attributes
  @@ fun c ->
  let parens = match parens with Some b -> b | None -> parenze_pat xpat in
  let spc = break_unless_newline 1 0 in
  ( match ppat_desc with
  | Ppat_or _ -> Fn.id
  | Ppat_construct ({txt; loc}, _) when Poly.(txt <> Longident.Lident "::")
    ->
      fun k ->
        Cmts.fmt c.cmts ~pro:spc ppat_loc
        @@ Cmts.fmt c.cmts ~pro:spc loc
        @@ (Option.call ~f:pro $ k)
  | _ ->
      fun k -> Cmts.fmt c.cmts ~pro:spc ppat_loc @@ (Option.call ~f:pro $ k)
  )
  @@ ( if List.is_empty ppat_attributes then Fn.id
     else fun k ->
       wrap "(" ")" (k $ fmt_attributes c ~key:"@" ppat_attributes) )
  @@
  match ppat_desc with
  | Ppat_any -> fmt "_"
  | Ppat_var {txt; loc} ->
      Cmts.fmt c.cmts loc @@ wrap_if (is_symbol_id txt) "( " " )" (str txt)
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
           $ Cmts.fmt c.cmts loc
               (wrap_if (is_symbol_id txt) "( " " )" (str txt)) ))
  | Ppat_constant const -> fmt_constant c ~loc:ppat_loc const
  | Ppat_interval (l, u) -> (
      (* we need to reconstruct locations for both side of the interval *)
      let toks =
        Source.tokens_at c.source ppat_loc ~filter:(function
          | Parser.CHAR _ | Parser.DOTDOT
           |Parser.(INT _ | STRING _ | FLOAT _) ->
              true
          | _ -> false )
      in
      match toks with
      | [ (Parser.(CHAR _ | INT _ | STRING _ | FLOAT _), loc1)
        ; (Parser.DOTDOT, _)
        ; (Parser.(CHAR _ | INT _ | STRING _ | FLOAT _), loc2) ] ->
          fmt_constant ~loc:loc1 c l
          $ fmt " .. "
          $ fmt_constant ~loc:loc2 c u
      | _ ->
          impossible
            "Ppat_interval is only produced by the sequence of 3 tokens: \
             CONSTANT-DOTDOT-CONSTANT " )
  | Ppat_tuple pats ->
      let parens =
        parens || Poly.(c.conf.parens_tuple_patterns = `Always)
      in
      let wrap_multiline =
        if c.conf.indicate_multiline_delimiters then
          wrap_if_breaks "( " "@ )"
        else wrap_if_breaks "(" ")"
      in
      hvbox 0
        (wrap_multiline
           (wrap_if_fits_and parens "(" ")"
              (list pats "@,, " (sub_pat ~ctx >> fmt_pattern c))))
  | Ppat_construct (lid, None) -> (
    match lid.txt with
    | Lident (("()" | "[]") as txt) ->
        let opn = txt.[0] and cls = txt.[1] in
        Cmts.fmt c.cmts lid.loc
          (hvbox 0
             (wrap_k (char opn) (char cls)
                (Cmts.fmt_within c.cmts ~pro:(fmt " ") ~epi:(fmt " ")
                   ppat_loc)))
    | _ -> fmt_longident_loc c lid )
  | Ppat_construct
      ( {txt= Lident "::"; loc}
      , Some {ppat_desc= Ppat_tuple [x; y]; ppat_attributes= []} ) -> (
    match sugar_list_pat c pat with
    | Some (loc_xpats, nil_loc) ->
        hvbox 0
          (wrap_list c
             ( list loc_xpats "@,; " (fun (locs, xpat) ->
                   Cmts.fmt_list c.cmts locs @@ fmt_pattern c xpat )
             $ Cmts.fmt c.cmts ~pro:(fmt " ") ~epi:(fmt "") nil_loc
               @@ fmt "" ))
    | None ->
        hvbox 0
          (wrap_if parens "(" ")"
             ( fmt_pattern c (sub_pat ~ctx x)
             $ Cmts.fmt c.cmts ~pro:(fmt " ") ~epi:(fmt "") loc
                 (fmt "@ :: ")
             $ fmt_pattern c (sub_pat ~ctx y) )) )
  | Ppat_construct (lid, Some pat) ->
      cbox 2
        (wrap_if parens "(" ")"
           ( fmt_longident_loc c lid $ fmt "@ "
           $ fmt_pattern c (sub_pat ~ctx pat) ))
  | Ppat_variant (lbl, None) -> fmt "`" $ str lbl
  | Ppat_variant (lbl, Some pat) ->
      cbox 2
        (wrap_if parens "(" ")"
           (fmt "`" $ str lbl $ fmt "@ " $ fmt_pattern c (sub_pat ~ctx pat)))
  | Ppat_record (flds, closed_flag) ->
      let fmt_field (lid1, pat) =
        let {ppat_desc; ppat_loc} = pat in
        hvbox 0
          ( Cmts.fmt c.cmts lid1.loc @@ Cmts.fmt c.cmts ppat_loc
          @@
          match ppat_desc with
          | Ppat_var {txt= txt'}
            when field_alias ~field:lid1.txt (Longident.parse txt')
                 && List.is_empty pat.ppat_attributes ->
              cbox 2 (fmt_longident_loc c lid1)
          | Ppat_constraint ({ppat_desc= Ppat_var {txt= txt'; _}}, t)
            when field_alias ~field:lid1.txt (Longident.parse txt')
                 && List.is_empty pat.ppat_attributes ->
              cbox 2
                ( fmt_longident_loc c lid1 $ fmt " : "
                $ fmt_core_type c (sub_typ ~ctx:(Pat pat) t) )
          | _ ->
              cbox 2
                ( fmt_longident_loc c lid1
                $ fmt_if Poly.(c.conf.field_space = `Loose) " "
                $ fmt "=@ "
                $ cbox 0 (fmt_pattern c (sub_pat ~ctx pat)) ) )
      in
      hvbox 0
        (wrap_if parens "(" ")"
           (wrap_record c
              ( list flds "@,; " fmt_field
              $ fmt_if Poly.(closed_flag = Open) "; _" )))
  | Ppat_array [] ->
      hvbox 0
        (wrap_fits_breaks c.conf "[|" "|]" (Cmts.fmt_within c.cmts ppat_loc))
  | Ppat_array pats ->
      hvbox 0
        (wrap_array c
           (list pats "@;<0 1>; " (sub_pat ~ctx >> fmt_pattern c)))
  | Ppat_or _ ->
      let nested =
        match ctx0 with
        | Pat {ppat_desc= Ppat_or _} -> true
        | Exp {pexp_desc= Pexp_match _ | Pexp_try _ | Pexp_function _} ->
            true
        | _ -> false
      in
      let xpats = sugar_or_pat c xpat in
      let pro0 =
        Option.call ~f:pro
        $ fits_breaks
            (if parens then "(" else "")
            (if nested then "" else "( ")
      in
      let proI =
        match ctx0 with
        | Exp {pexp_desc= Pexp_function _ | Pexp_match _ | Pexp_try _}
          when break_cases_level c = 0 ->
            if c.conf.indicate_nested_or_patterns then or_newline "| " " |"
            else or_newline "| " "| "
        | _ -> break_unless_newline 1 0 $ fmt "| "
      in
      let pro2 =
        fmt_or_k
          (break_cases_level c > 1)
          (break_unless_newline 1000 0 $ fmt "| ")
          proI
      in
      let is_simple {ppat_desc} =
        match ppat_desc with
        | Ppat_any | Ppat_constant _ | Ppat_var _
         |Ppat_variant (_, (None | Some {ppat_desc= Ppat_any}))
         |Ppat_construct (_, (None | Some {ppat_desc= Ppat_any})) ->
            true
        | _ -> false
      in
      hvbox 0
        ( list_fl
            (List.group xpats ~break:(fun {ast= p1} {ast= p2} ->
                 break_cases_level c > 0
                 || (not (is_simple p1))
                 || not (is_simple p2) ))
            (fun ~first:first_grp ~last:_ xpat_grp ->
              list_fl xpat_grp (fun ~first ~last xpat ->
                  let pro =
                    if first_grp && first then pro0 $ open_hovbox (-2)
                    else if first then proI $ open_hovbox (-2)
                    else pro2
                  in
                  (* side effects of Cmts.fmt_before before [fmt_pattern] is
                     important *)
                  let leading_cmt =
                    let loc = xpat.ast.ppat_loc in
                    if Cmts.has_before c.cmts loc then
                      let loc_before = Cmts.fmt_before c.cmts loc in
                      fmt "@;<1000 0>" $ loc_before
                    else fmt ""
                  in
                  leading_cmt $ fmt_pattern c ~pro xpat
                  $ fmt_if_k last close_box ) )
        $ fits_breaks
            (if parens then ")" else "")
            (if nested then "" else "@;<1 2>)") )
  | Ppat_constraint
      ( {ppat_desc= Ppat_unpack name; ppat_attributes= []}
      , ({ptyp_desc= Ptyp_package pty; ptyp_attributes= []} as typ) ) ->
      let ctx = Typ typ in
      wrap_if parens "(" ")"
        ( fmt "module " $ fmt_str_loc c name $ fmt "@ : "
        $ fmt_package_type c ctx pty )
  | Ppat_constraint (pat, typ) ->
      hvbox 2
        (wrap_if parens "(" ")"
           ( fmt_pattern c (sub_pat ~ctx pat)
           $ ( match ctx0 with
             | Exp {pexp_desc= Pexp_let _} -> fmt "@ : "
             | _ -> fmt " :@ " )
           $ fmt_core_type c (sub_typ ~ctx typ) ))
  | Ppat_type lid -> fmt_longident_loc c ~pre:"#" lid
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
  | Ppat_extension ext -> hvbox 2 (fmt_extension c ctx "%" ext)
  | Ppat_open (lid, pat) ->
      cbox 0
        ( fmt_longident_loc c lid $ fmt ".("
        $ fmt_pattern c (sub_pat ~ctx pat)
        $ fmt ")" )

and fmt_fun_args c ?(pro = fmt "") args =
  let fmt_fun_arg = function
    | Val (Nolabel, xpat, None) -> fmt_pattern c xpat
    | Val
        ( Labelled l
        , ( { ast=
                { ppat_desc=
                    ( Ppat_var {txt}
                    | Ppat_constraint
                        ({ppat_desc= Ppat_var {txt}; ppat_attributes= []}, _)
                      )
                ; ppat_attributes= [] } } as xpat )
        , None )
      when String.equal l txt ->
        cbox 0 (fmt "~" $ fmt_pattern c xpat)
    | Val (Labelled l, xpat, None) ->
        cbox 0 (fmt "~" $ str l $ fmt ":" $ fmt_pattern c xpat)
    | Val
        ( Optional l
        , ( { ast=
                { ppat_desc=
                    ( Ppat_var {txt}
                    | Ppat_constraint
                        ({ppat_desc= Ppat_var {txt}; ppat_attributes= []}, _)
                      )
                ; ppat_attributes= [] } } as xpat )
        , None )
      when String.equal l txt ->
        cbox 0 (fmt "?" $ fmt_pattern c xpat)
    | Val (Optional l, xpat, None) ->
        cbox 0 (fmt "?" $ str l $ fmt ":" $ fmt_pattern c xpat)
    | Val
        ( Optional l
        , ({ast= {ppat_desc= Ppat_var {txt}; ppat_attributes= []}} as xpat)
        , Some xexp )
      when String.equal l txt ->
        cbox 0
          ( fmt "?(" $ fmt_pattern c xpat $ fmt " =@;<1 2>"
          $ hovbox 2 (fmt_expression c xexp)
          $ fmt ")" )
    | Val
        ( Optional l
        , ( { ast=
                { ppat_desc= Ppat_constraint ({ppat_desc= Ppat_var {txt}}, _)
                ; ppat_attributes= [] } } as xpat )
        , Some xexp )
      when String.equal l txt ->
        cbox 0
          ( fmt "?("
          $ fmt_pattern c ~parens:false xpat
          $ fmt " =@;<1 2>" $ fmt_expression c xexp $ fmt ")" )
    | Val (Optional l, xpat, Some xexp) ->
        cbox 0
          ( fmt "?" $ str l $ fmt ":("
          $ fmt_pattern c ~parens:false xpat
          $ fmt " =@;<1 2>" $ fmt_expression c xexp $ fmt ")" )
    | Val ((Labelled _ | Nolabel), _, Some _) ->
        impossible "not accepted by parser"
    | Newtypes [] -> impossible "not accepted by parser"
    | Newtypes names ->
        cbox 0
          (wrap "(" ")"
             (fmt "type " $ list names "@ " (fun name -> fmt_str_loc c name)))
  in
  fmt_if_k
    (not (List.is_empty args))
    (pro $ list args "@;" (fun x -> hovbox 0 (fmt_fun_arg x)))

and fmt_body c ?ext ({ast= body} as xbody) =
  let ctx = Exp body in
  let parens = parenze_exp xbody in
  match body with
  | {pexp_desc= Pexp_function cs; pexp_attributes; pexp_loc} ->
      update_config_maybe_disabled c pexp_loc pexp_attributes
      @@ fun c ->
      fmt "@ "
      $ Cmts.fmt c.cmts pexp_loc
          (wrap_if parens "(" ")"
             ( fmt "function"
             $ fmt_extension_suffix c ext
             $ fmt_attributes c ~key:"@" pexp_attributes
             $ close_box $ fmt "@ " $ fmt_cases c ctx cs ))
  | _ ->
      close_box $ fmt "@ " $ fmt_expression c ~eol:(fmt "@;<1000 0>") xbody

and fmt_index_op c ctx ~parens ?set {txt= s, opn, cls; loc} l is =
  wrap_if parens "(" ")"
    (hovbox 0
       ( fmt_expression c (sub_exp ~ctx l)
       $ Cmts.fmt_before c.cmts loc
       $ str (Printf.sprintf "%s%c" s opn)
       $ Cmts.fmt_after c.cmts loc
       $ list is "@,, " (fun i -> fmt_expression c (sub_exp ~ctx i))
       $ str (Printf.sprintf "%c" cls)
       $
       match set with
       | None -> fmt ""
       | Some e -> fmt "@ <- " $ fmt_expression c (sub_exp ~ctx e) ))

and fmt_expression c ?(box = true) ?epi ?eol ?parens ?ext
    ({ast= exp} as xexp) =
  protect (Exp exp)
  @@
  let {pexp_desc; pexp_loc; pexp_attributes} = exp in
  update_config_maybe_disabled c pexp_loc pexp_attributes
  @@ fun c ->
  let fmt_cmts = Cmts.fmt c.cmts ?eol pexp_loc in
  let fmt_atrs = fmt_attributes c ~pre:(fmt " ") ~key:"@" pexp_attributes in
  let parens = match parens with Some b -> b | None -> parenze_exp xexp in
  let width xe = String.length (Cmts.preserve (fmt_expression c) xe) in
  let fmt_label lbl sep =
    match lbl with
    | Nolabel -> fmt ""
    | Labelled l -> fmt "~" $ str l $ fmt sep
    | Optional l -> fmt "?" $ str l $ fmt sep
  in
  let fmt_label_arg ?(box = box) ?epi ?parens (lbl, ({ast= arg} as xarg)) =
    match (lbl, arg.pexp_desc) with
    | (Labelled l | Optional l), Pexp_ident {txt= Lident i; loc}
      when String.equal l i && List.is_empty arg.pexp_attributes ->
        Cmts.fmt c.cmts loc
        @@ Cmts.fmt c.cmts ?eol arg.pexp_loc
        @@ fmt_label lbl ""
    | _ ->
        hvbox_if box 2
          (fmt_label lbl ":@," $ fmt_expression c ~box ?epi ?parens xarg)
  in
  let fmt_op_args op_args =
    let fmt_arg ~last_op ~first:_ ~last lbl_xarg =
      let _, ({ast= arg} as xarg) = lbl_xarg in
      let parens =
        ((not last_op) && exposed_right_exp Ast.Non_apply arg)
        || parenze_exp xarg
      in
      fmt_label_arg
        ?box:
          ( match (snd lbl_xarg).ast.pexp_desc with
          | Pexp_fun _ | Pexp_function _ -> Some (not last)
          | _ -> None )
        ~parens lbl_xarg
      $ fmt_if (not last) "@ "
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
        | (_, {ast= a0}) :: _ -> last && is_not_indented a0
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
        | Exp {pexp_desc= Pexp_apply (e, _)} when is_infix e -> prec_ast ast
        | Exp
            ( { pexp_desc=
                  Pexp_construct
                    ({txt= Lident "::"}, Some {pexp_desc= Pexp_tuple [_; _]})
              } as exp )
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
      parens || Poly.equal c.conf.infix_precedence `Parens
    in
    let fmt_op_arg_group ~first:first_grp ~last:last_grp args =
      list_fl args (fun ~first ~last (fmt_cmts, op_args) ->
          let very_first = first_grp && first in
          let very_last = last_grp && last in
          fmt_if_k very_first
            (fits_breaks_if parens_or_nested "("
               ( if parens_or_forced then
                 if c.conf.indicate_multiline_delimiters then "( " else "("
               else "" ))
          $ fmt_cmts
          $ fmt_if_k first
              (open_hovbox (if first_grp && parens then -2 else 0))
          $ fmt_op_args ~first:very_first op_args ~last:very_last
          $ fmt_if_k last close_box
          $ fmt_or_k very_last
              (fits_breaks_if parens_or_nested ")"
                 ( if parens_or_forced then
                   if c.conf.indicate_multiline_delimiters then "@ )"
                   else "@,)"
                 else "" ))
              (break_unless_newline 1 0) )
    in
    let op_args_grouped =
      match c.conf.break_infix with
      | `Wrap ->
          List.group op_args ~break:(fun (_, (_, args1)) (_, (_, args2)) ->
              let exists_not_simple args =
                List.exists args ~f:(fun (_, arg) ->
                    not (is_simple c.conf width arg) )
              in
              exists_not_simple args1 || exists_not_simple args2 )
      | `Fit_or_vertical -> List.map ~f:(fun x -> [x]) op_args
    in
    hvbox 0 (list_fl op_args_grouped fmt_op_arg_group $ fmt_atrs)
  in
  let ctx = Exp exp in
  let fmt_args_grouped e0 a1N =
    let all = (Nolabel, e0) :: a1N in
    let groups =
      if c.conf.wrap_fun_args then
        List.group all ~break:(fun (_, a1) (_, a2) ->
            (not (is_simple c.conf width (sub_exp ~ctx a1)))
            || not (is_simple c.conf width (sub_exp ~ctx a2)) )
      else List.map all ~f:(fun x -> [x])
    in
    list_fl groups (fun ~first:first_grp ~last:last_grp args ->
        list_pn args (fun ?prev (lbl, arg) ?next ->
            let ({ast} as xarg) = sub_exp ~ctx arg in
            let openbox =
              fmt_if_k (Option.is_none prev)
                (open_hovbox (if first_grp then 2 else 0))
            in
            let consecutive_prefix_ops =
              is_prefix ast
              &&
              match next with
              | Some (_, {pexp_desc= Pexp_apply (op, _)}) -> is_prefix op
              | _ -> false
            in
            let spc =
              consecutive_prefix_ops || (not last_grp)
              || Option.is_some next
            in
            openbox
            $ hovbox 2
                (fmt_label_arg
                   ?box:
                     ( match ast.pexp_desc with
                     | Pexp_fun _ | Pexp_function _ -> Some false
                     | _ -> None )
                   ?epi:
                     ( match (lbl, next) with
                     | _, None -> None
                     | Nolabel, _ -> Some (fits_breaks "" "@;<1000 -1>")
                     | _ -> Some (fits_breaks "" "@;<1000 -3>") )
                   (lbl, xarg))
            $ fmt_if_k (Option.is_none next) close_box
            $ fmt_if_k spc (break_unless_newline 1 0) ) )
  in
  hvbox_if box 0 @@ fmt_cmts
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
                            (({pexp_desc= Pexp_fun _; _} as call_fun), []); _
                      } as pld ) ] ) }
      , e2 ) ->
      let xargs, xbody = sugar_fun c (sub_exp ~ctx:(Str pld) call_fun) in
      hvbox 0
        (wrap_if parens "(" ")"
           ( hvbox 2
               (wrap "[" "]"
                  ( str "%"
                  $ hovbox 2
                      ( fmt_str_loc c name $ fmt " "
                      $ ( fmt "fun "
                        $ fmt_attributes c ~suf:(fmt " ")
                            call_fun.pexp_attributes ~key:"@"
                        $ fmt_fun_args c xargs $ fmt "@ ->" ) )
                  $ fmt "@ " $ fmt_expression c xbody ))
           $ fmt "@ ;@ "
           $ list
               (sugar_sequence c width (sub_exp ~ctx e2))
               " ;@;<1000 0>"
               (fun grp -> list grp " ;@ " (fmt_expression c)) ))
  | Pexp_apply
      ( {pexp_desc= Pexp_ident {txt= Lident "|>"; loc}; pexp_attributes= []}
      , [ (Nolabel, e0)
        ; ( Nolabel
          , { pexp_desc=
                Pexp_extension
                  ( name
                  , PStr
                      [ ( { pstr_desc=
                              Pstr_eval
                                ( ({pexp_desc= Pexp_fun _; _} as retn_fun)
                                , [] ); _ } as pld ) ] ) } ) ] ) ->
      let xargs, xbody = sugar_fun c (sub_exp ~ctx:(Str pld) retn_fun) in
      hvbox 0
        (wrap_fits_breaks_if ~space:false c.conf parens "(" ")"
           ( fmt_expression c (sub_exp ~ctx e0)
           $ fmt "@\n"
           $ Cmts.fmt c.cmts loc (fmt "|>@\n")
           $ hvbox 2
               (wrap "[" "]"
                  ( str "%"
                  $ hovbox 2
                      ( fmt_str_loc c name $ fmt " "
                      $ ( fmt "fun "
                        $ fmt_attributes c ~suf:(fmt " ")
                            retn_fun.pexp_attributes ~key:"@"
                        $ fmt_fun_args c xargs $ fmt "@ ->" ) )
                  $ fmt "@ " $ fmt_expression c xbody )) ))
  | Pexp_apply
      ( {pexp_desc= Pexp_ident ident; pexp_attributes= []; pexp_loc}
      , (Nolabel, s) :: indices )
    when Option.is_some (index_op_get_sugar ident indices) ->
      let op, indices =
        Option.value_exn (index_op_get_sugar ident indices)
      in
      Cmts.relocate c.cmts ~src:pexp_loc ~before:ident.loc ~after:ident.loc ;
      fmt_index_op c ctx ~parens op s indices
  | Pexp_apply
      ( {pexp_desc= Pexp_ident ident; pexp_attributes= []; pexp_loc}
      , (Nolabel, s) :: indices_and_e )
    when Option.is_some (index_op_set_sugar ident indices_and_e) ->
      let op, indices, e =
        Option.value_exn (index_op_set_sugar ident indices_and_e)
      in
      Cmts.relocate c.cmts ~src:pexp_loc ~before:ident.loc ~after:ident.loc ;
      fmt_index_op c ctx ~parens op s indices ~set:e
  | Pexp_apply
      ( { pexp_desc= Pexp_ident {txt= Lident ":="; loc}
        ; pexp_attributes= []
        ; pexp_loc }
      , [(Nolabel, r); (Nolabel, v)] )
    when is_simple c.conf width (sub_exp ~ctx r) ->
      Cmts.relocate c.cmts ~src:pexp_loc ~before:loc ~after:loc ;
      wrap_if parens "(" ")"
        (hovbox 0
           ( fmt_expression c (sub_exp ~ctx r)
           $ Cmts.fmt c.cmts loc (fmt " :=@;<1 2>")
           $ hvbox 2 (fmt_expression c (sub_exp ~ctx v)) ))
  | Pexp_apply
      ( { pexp_desc=
            Pexp_ident
              {txt= Lident (("~-" | "~-." | "~+" | "~+.") as op); loc}
        ; pexp_loc
        ; pexp_attributes= [] }
      , [(Nolabel, e1)] ) ->
      let op =
        if Location.width loc = String.length op - 1 then
          String.sub op ~pos:1 ~len:(String.length op - 1)
        else op
      in
      let spc = if exposed_left_exp e1 then fmt "@ " else fmt "" in
      wrap_if parens "(" ")"
        ( Cmts.fmt c.cmts pexp_loc
          @@ hvbox 2 (str op $ spc $ fmt_expression c (sub_exp ~ctx e1))
        $ fmt_atrs )
  | Pexp_apply
      ( ( { pexp_desc= Pexp_ident {txt= Lident maybe_hash; loc}
          ; pexp_attributes= []
          ; pexp_loc } as op )
      , [(Nolabel, l); (Nolabel, ({pexp_desc= Pexp_ident _} as r))] )
    when String.is_prefix ~prefix:"#" maybe_hash ->
      Cmts.relocate c.cmts ~src:pexp_loc ~before:loc ~after:loc ;
      wrap_if parens "(" ")"
        ( fmt_expression c (sub_exp ~ctx l)
        $ fmt_expression c (sub_exp ~ctx op)
        $ fmt_expression c (sub_exp ~ctx r) )
  | Pexp_apply
      ( {pexp_desc= Pexp_ident {txt= Lident id}; pexp_attributes= []}
      , [(Nolabel, _); (Nolabel, _)] )
    when is_infix_id id ->
      let op_args = sugar_infix c (prec_ast (Exp exp)) xexp in
      fmt_op_args
        (List.map op_args ~f:(fun (op, args) ->
             match op with
             | Some ({ast= {pexp_loc}} as op) ->
                 (* side effects of Cmts.fmt_before before fmt_expression is
                    important *)
                 let fmt_cmts = Cmts.fmt_before c.cmts pexp_loc in
                 let fmt_op = fmt_expression c op in
                 (fmt_cmts, (fmt_op, args))
             | None -> (fmt "", (fmt "", args)) ))
  | Pexp_apply
      ( {pexp_desc= Pexp_ident {txt= Lident id; loc}; pexp_loc}
      , (Nolabel, s) :: (Nolabel, i) :: _ )
    when Option.is_some (index_op_get id) -> (
    match index_op_get id with
    | Some index_op ->
        Cmts.relocate c.cmts ~src:pexp_loc ~before:loc ~after:loc ;
        fmt_index_op c ctx ~parens {txt= index_op; loc} s [i]
    | None -> impossible "previous match" )
  | Pexp_apply
      ( {pexp_desc= Pexp_ident {txt= Lident id; loc}; pexp_loc}
      , (Nolabel, s) :: (Nolabel, i) :: (Nolabel, e) :: _ )
    when Option.is_some (index_op_set id) -> (
    match index_op_set id with
    | Some index_op ->
        Cmts.relocate c.cmts ~src:pexp_loc ~before:loc ~after:loc ;
        fmt_index_op c ctx ~parens {txt= index_op; loc} s [i] ~set:e
    | None -> impossible "previous match" )
  | Pexp_apply (e0, [(Nolabel, e1)]) when is_prefix e0 ->
      hvbox 2
        (wrap_fits_breaks_if ~space:false c.conf parens "(" ")"
           ( fmt_expression c ~box (sub_exp ~ctx e0)
           $ fmt_expression c ~box (sub_exp ~ctx e1)
           $ fmt_atrs ))
  | Pexp_apply (e0, e1N1) -> (
      let wrap = if c.conf.wrap_fun_args then Fn.id else hvbox 2 in
      match List.rev e1N1 with
      | (lbl, ({pexp_desc= Pexp_fun _; pexp_loc} as eN1)) :: rev_e1N
        when List.for_all rev_e1N ~f:(fun (_, eI) ->
                 is_simple c.conf (fun _ -> 0) (sub_exp ~ctx eI) ) ->
          let e1N = List.rev rev_e1N in
          (* side effects of Cmts.fmt c.cmts before sugar_fun is important *)
          let fmt_cmts = Cmts.fmt c.cmts pexp_loc in
          let xargs, xbody = sugar_fun c (sub_exp ~ctx eN1) in
          hvbox 0
            (wrap_if parens "(" ")"
               ( hovbox 0
                   ( hovbox 2
                       ( wrap
                           ( fmt_args_grouped e0 e1N $ fmt "@ "
                           $ fmt_label lbl ":"
                           $ fmt_cmts
                             @@ hvbox 0
                                  ( fmt "(fun "
                                  $ fmt_attributes c ~key:"@"
                                      eN1.pexp_attributes ~suf:(fmt " ")
                                  $ fmt_fun_args c xargs $ fmt "@ ->" ) )
                       $ fmt
                           ( match xbody.ast.pexp_desc with
                           | Pexp_function _ -> "@ "
                           | _ -> "@;<1 2>" )
                       $ fmt_expression c
                           ?box:
                             ( match xbody.ast.pexp_desc with
                             | Pexp_fun _ | Pexp_function _ -> Some false
                             | _ -> None )
                           xbody )
                   $ fmt_or_k c.conf.indicate_multiline_delimiters
                       (fits_breaks ")" "@ )") (fits_breaks ")" "@,)") )
               $ fmt_atrs ))
      | ( lbl
        , ( { pexp_desc= Pexp_function [{pc_lhs; pc_guard= None; pc_rhs}]
            ; pexp_loc } as eN ) )
        :: rev_e1N
        when List.for_all rev_e1N ~f:(fun (_, eI) ->
                 is_simple c.conf (fun _ -> 0) (sub_exp ~ctx eI) ) ->
          let e1N = List.rev rev_e1N in
          let ctx = Exp eN in
          (* side effects of Cmts.fmt_before before [fmt_pattern] is
             important *)
          let leading_cmt = Cmts.fmt_before c.cmts pc_lhs.ppat_loc in
          hvbox 2
            (wrap_if parens "(" ")"
               ( hovbox 4
                   ( wrap
                       ( fmt_args_grouped e0 e1N $ fmt "@ "
                       $ Cmts.fmt_before c.cmts pexp_loc
                       $ fmt_label lbl ":" $ fmt "(function"
                       $ fmt_attributes c ~pre:(fmt " ") ~key:"@"
                           eN.pexp_attributes )
                   $ fmt "@ " $ leading_cmt
                   $ hvbox 0
                       ( fmt_pattern c ~pro:(if_newline "| ")
                           (sub_pat ~ctx pc_lhs)
                       $ fmt "@ ->" )
                   $ fmt "@ "
                   $ cbox 0 (fmt_expression c (sub_exp ~ctx pc_rhs))
                   $ fmt_or_k c.conf.indicate_multiline_delimiters
                       (fits_breaks ")" " )") (fmt ")")
                   $ Cmts.fmt_after c.cmts pexp_loc )
               $ fmt_atrs ))
      | (lbl, ({pexp_desc= Pexp_function cs; pexp_loc} as eN)) :: rev_e1N
        when List.for_all rev_e1N ~f:(fun (_, eI) ->
                 is_simple c.conf (fun _ -> 0) (sub_exp ~ctx eI) ) ->
          let e1N = List.rev rev_e1N in
          let ctx'' = Exp eN in
          hvbox
            (if c.conf.wrap_fun_args then 2 else 4)
            (wrap_if parens "(" ")"
               ( hovbox 2
                   (wrap
                      ( fmt_args_grouped e0 e1N $ fmt "@ "
                      $ Cmts.fmt_before c.cmts pexp_loc
                      $ fmt_label lbl ":" $ fmt "(function"
                      $ fmt_attributes c ~pre:(fmt " ") ~key:"@"
                          eN.pexp_attributes ))
               $ fmt "@ " $ fmt_cases c ctx'' cs
               $ fmt_or_k c.conf.indicate_multiline_delimiters
                   (fits_breaks ")" " )") (fmt ")")
               $ Cmts.fmt_after c.cmts pexp_loc
               $ fmt_atrs ))
      | _ ->
          wrap_if parens "(" ")"
            (hvbox 2 (fmt_args_grouped e0 e1N1) $ fmt_atrs) )
  | Pexp_array [] ->
      hvbox 0
        ( wrap_fits_breaks c.conf "[|" "|]" (Cmts.fmt_within c.cmts pexp_loc)
        $ fmt_atrs )
  | Pexp_array e1N ->
      hvbox 0
        ( wrap_array c
            (fmt_expressions c width (sub_exp ~ctx) e1N "@;<0 1>; "
               (sub_exp ~ctx >> fmt_expression c))
        $ fmt_atrs )
  | Pexp_assert e0 ->
      let paren_body = parenze_exp (sub_exp ~ctx e0) in
      hovbox 0
        (wrap_if parens "(" ")"
           (hvbox 0
              ( hvbox 2
                  ( fmt_or paren_body "assert (@," "assert@ "
                  $ fmt_expression c ~parens:false (sub_exp ~ctx e0) )
              $ fmt_or_k c.conf.indicate_multiline_delimiters
                  (fits_breaks_if paren_body ")" "@ )")
                  (fits_breaks_if paren_body ")" "@,)")
              $ fmt_atrs )))
  | Pexp_constant const ->
      wrap_if
        (parens || not (List.is_empty pexp_attributes))
        "(" ")"
        (fmt_constant c ~loc:pexp_loc ?epi const $ fmt_atrs)
  | Pexp_constraint
      ( {pexp_desc= Pexp_pack me; pexp_attributes= []}
      , {ptyp_desc= Ptyp_package pty; ptyp_attributes= []} ) ->
      let {opn; pro; psp; bdy; cls; esp; epi} =
        fmt_module_expr c (sub_mod ~ctx me)
      in
      opn
      $ wrap_fits_breaks ~space:false c.conf "(" ")"
          ( fmt "module " $ Option.call ~f:pro $ psp $ bdy $ cls $ esp
          $ Option.call ~f:epi $ fmt "@ : "
          $ fmt_package_type c ctx pty
          $ fmt_atrs )
  | Pexp_constraint (e, t) ->
      hvbox 2
        (wrap_fits_breaks ~space:false c.conf "(" ")"
           ( fmt_expression c (sub_exp ~ctx e)
           $ fmt "@ : "
           $ fmt_core_type c (sub_typ ~ctx t)
           $ fmt_atrs ))
  | Pexp_construct (lid, None) -> (
    match lid.txt with
    | Lident (("()" | "[]") as txt) ->
        let opn = txt.[0] and cls = txt.[1] in
        Cmts.fmt c.cmts lid.loc
        @@ hvbox 0
             (wrap_if
                (not (List.is_empty pexp_attributes))
                "(" ")"
                ( wrap_k (char opn) (char cls)
                    (Cmts.fmt_within c.cmts ~pro:(fmt " ") ~epi:(fmt " ")
                       pexp_loc)
                $ fmt_atrs ))
    | Lident "::" ->
        wrap_if parens "(" ")"
          (wrap "(" ")" (fmt_longident_loc c lid $ fmt_atrs))
    | _ -> wrap_if parens "(" ")" (fmt_longident_loc c lid $ fmt_atrs) )
  | Pexp_construct
      ( {txt= Lident "::"}
      , Some {pexp_desc= Pexp_tuple [_; _]; pexp_attributes= []} ) -> (
    match sugar_list_exp c exp with
    | Some (loc_xes, nil_loc) ->
        hvbox 0
          (wrap_if
             (not (List.is_empty pexp_attributes))
             "(" ")"
             ( wrap_list c
                 ( fmt_expressions c width snd loc_xes "@,; "
                     (fun (locs, xexp) ->
                       Cmts.fmt_list c.cmts ~eol:(fmt "@;<1 2>") locs
                       @@ fmt_expression c xexp )
                 $ Cmts.fmt c.cmts ~pro:(fmt "@ ") ~epi:(fmt "") nil_loc
                   @@ fmt "" )
             $ fmt_atrs ))
    | None ->
        let loc_args = sugar_infix_cons xexp in
        fmt_op_args
          (List.mapi loc_args ~f:(fun i (locs, arg) ->
               let fmt_cmts =
                 match locs with
                 | [] -> fmt ""
                 | locs -> list locs "" (Cmts.fmt_before c.cmts)
               in
               let fmt_op = match i with 0 -> fmt "" | _ -> fmt "::" in
               (fmt_cmts, (fmt_op, [(Nolabel, arg)])) )) )
  | Pexp_construct (({txt= Lident "::"} as lid), Some arg) ->
      wrap_if parens "(" ")"
        ( hvbox 2
            ( fmt_or_k c.conf.indicate_multiline_delimiters
                (wrap "( " " )" (fmt_longident_loc c lid))
                (wrap "(" ")" (fmt_longident_loc c lid))
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
           ( fmt "`" $ str s
           $ opt arg (fmt "@ " >$ (sub_exp ~ctx >> fmt_expression c))
           $ fmt_atrs ))
  | Pexp_field (exp, lid) ->
      hvbox 2
        (wrap_if parens "(" ")"
           ( fmt_expression c (sub_exp ~ctx exp)
           $ fmt "@,." $ fmt_longident_loc c lid $ fmt_atrs ))
  | Pexp_newtype _ | Pexp_fun _ ->
      let xargs, xbody = sugar_fun c xexp in
      hvbox_if box
        (if Option.is_none eol then 2 else 1)
        ( fmt_if parens "("
        $ ( open_hovbox 2
          $ ( hovbox 4
                ( fmt "fun "
                $ fmt_attributes c ~key:"@" pexp_attributes ~suf:(fmt " ")
                $ hvbox_if
                    (not c.conf.wrap_fun_args)
                    0 (fmt_fun_args c xargs)
                $ fmt "@ " )
            $ fmt "->" )
          $ fmt_body c ?ext xbody )
        $ fmt_or_k c.conf.indicate_multiline_delimiters
            (fits_breaks_if parens ")" "@ )")
            (fits_breaks_if parens ")" "@,)") )
  | Pexp_function cs ->
      wrap_if parens "(" ")"
        ( hvbox 2
            ( fmt "function"
            $ fmt_extension_suffix c ext
            $ fmt_attributes c ~key:"@" pexp_attributes )
        $ fmt "@ "
        $ hvbox 0 (fmt_cases c ctx cs) )
  | Pexp_ident {txt; loc} ->
      let wrap, wrap_ident =
        if is_symbol exp && not (List.is_empty pexp_attributes) then
          (wrap "( " " )", true)
        else if is_symbol exp then (wrap_if parens "( " " )", false)
        else (wrap_if parens "(" ")", false)
      in
      Cmts.fmt c.cmts loc
      @@ wrap
           ( wrap_if wrap_ident "(" ")"
               (fmt_longident txt $ Cmts.fmt_within c.cmts loc)
           $ fmt_atrs )
  | Pexp_ifthenelse _ ->
      let cnd_exps = sugar_ite c xexp in
      hvbox 0
        (wrap_fits_breaks_if ~space:false c.conf parens "(" ")"
           (list_fl cnd_exps
              (fun ~first ~last (xcnd, xbch, pexp_attributes) ->
                let parens_bch = parenze_exp xbch in
                match c.conf.if_then_else with
                | `Compact ->
                    hovbox 0
                      ( hovbox
                          (if first && parens then 0 else 2)
                          ( ( match xcnd with
                            | Some xcnd ->
                                hvbox
                                  (if parens then -2 else 0)
                                  ( hvbox
                                      (if parens then 0 else 2)
                                      ( fmt_if (not first) "else "
                                      $ fmt "if"
                                      $ fmt_if_k first
                                          (fmt_extension_suffix c ext)
                                      $ fmt_attributes c ~pre:(fmt " ")
                                          ~key:"@" pexp_attributes
                                      $ fmt "@ " $ fmt_expression c xcnd )
                                  $ fmt "@ then" )
                            | None -> fmt "else" )
                          $ fmt_if parens_bch " (" $ fmt "@ "
                          $ fmt_expression c ~box:false ~parens:false xbch
                          )
                      $ fmt_if parens_bch
                          ( if c.conf.indicate_multiline_delimiters then " )"
                          else ")" ) )
                    $ fmt_if (not last) "@ "
                | `Keyword_first ->
                    opt xcnd (fun xcnd ->
                        hvbox 2
                          ( fmt_or_k first
                              (fmt "if" $ fmt_extension_suffix c ext)
                              (fmt "else if")
                          $ fmt_attributes c ~pre:(fmt " ") ~key:"@"
                              pexp_attributes
                          $ str " " $ fmt_expression c xcnd )
                        $ fmt "@ " )
                    $ hvbox 2
                        ( fmt_or (Option.is_some xcnd) "then" "else"
                        $ fmt_if parens_bch " (" $ fmt "@ "
                        $ fmt_expression c ~box:false ~parens:false xbch
                        $ fmt_if parens_bch
                            ( if c.conf.indicate_multiline_delimiters then
                              " )"
                            else ")" ) )
                    $ fmt_if (not last) "@ " )))
  | Pexp_let (rec_flag, bindings, body) ->
      wrap_if
        (parens || not (List.is_empty pexp_attributes))
        "(" ")"
        (vbox 0
           ( hvbox 0
               (list_fl bindings (fun ~first ~last binding ->
                    fmt_value_binding c ~rec_flag ~first
                      ?ext:(if first then ext else None)
                      ctx binding
                      ~in_:(fun indent ->
                        fmt_if_k last (break 1 (-indent) $ fmt "in") )
                    $ fmt_if (not last)
                        ( match c.conf.let_and with
                        | `Sparse -> "@;<1000 0>"
                        | `Compact -> "@ " ) ))
           $ fmt "@;<1000 0>"
           $ hvbox 0 (fmt_expression c (sub_exp ~ctx body)) ))
      $ fmt_atrs
  | Pexp_letexception (ext_cstr, exp) ->
      hvbox 0
        ( wrap_if
            (parens || not (List.is_empty pexp_attributes))
            "(" ")"
            ( hvbox 0
                ( fmt_exception ~pre:(fmt "let exception@ ") c (fmt ": ")
                    ctx ext_cstr
                $ fmt "@ in" )
            $ fmt "@;<1000 0>"
            $ fmt_expression c (sub_exp ~ctx exp) )
        $ fmt_atrs )
  | Pexp_letmodule (name, pmod, exp) ->
      let keyword = fmt "let module" $ fmt_extension_suffix c ext in
      let xargs, xbody =
        sugar_functor c ~for_functor_kw:false (sub_mod ~ctx pmod)
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
      hvbox 0
        ( wrap_if
            (parens || not (List.is_empty pexp_attributes))
            "(" ")"
            ( hvbox 2
                (fmt_module c keyword name xargs (Some xbody) true xmty []
                   ~epi:(fmt "in"))
            $ fmt "@;<1000 0>"
            $ fmt_expression c (sub_exp ~ctx exp) )
        $ fmt_atrs )
  | Pexp_open (flag, name, e0) ->
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
        | _ -> override
      in
      let force_fit_if =
        match (let_open, xexp.ctx) with
        | `Short, _ when not override -> true
        | _, Exp {pexp_desc= Pexp_apply _ | Pexp_construct _} ->
            not force_break_if
        | _ -> false
      in
      let fits_breaks = fits_breaks ~force_fit_if ~force_break_if
      and fits_breaks_if = fits_breaks_if ~force_fit_if ~force_break_if in
      let opn, cls =
        let can_skip_parens =
          match e0.pexp_desc with
          | Pexp_array _ | Pexp_record _ -> true
          | Pexp_tuple _ -> Poly.(c.conf.parens_tuple = `Always)
          | _ -> (
            match sugar_list_exp c e0 with Some _ -> true | None -> false )
        in
        if can_skip_parens then (".", "") else (".(", ")")
      in
      hvbox 0
        ( fits_breaks_if parens "" "("
        $ fits_breaks "" (if override then "let open! " else "let open ")
        $ fmt_longident_loc c name $ fits_breaks opn " in"
        $ fmt_or_k force_fit_if (fmt "@;<0 2>")
            (fits_breaks "" "@;<1000 0>")
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
        match cs with
        | []
         |_ :: _ :: _
         |[ { pc_lhs=
                { ppat_desc=
                    Ppat_or _ | Ppat_alias ({ppat_desc= Ppat_or _}, _) } }
          ] ->
            None
        | [x] ->
            if Poly.(c.conf.single_case = `Compact) then Some x else None
      in
      match compact with
      | None ->
          let leading_cmt = Cmts.fmt_before c.cmts e0.pexp_loc in
          hvbox 0
            (wrap_fits_breaks_if ~space:false c.conf parens "(" ")"
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
          let leading_cmt = Cmts.fmt_before c.cmts pc_lhs.ppat_loc in
          let parens_here, parens_for_exp =
            if c.conf.leading_nested_match_parens then (false, None)
            else (parenze_exp xpc_rhs, Some false)
          in
          wrap_fits_breaks_if ~space:false c.conf parens "(" ")"
            (hovbox 2
               ( hvbox 0
                   ( str keyword
                   $ fmt_extension_suffix c ext
                   $ fmt_attributes c ~key:"@" pexp_attributes
                   $ hvbox 0
                       (fmt "@;<1 -1>" $ fmt_expression c (sub_exp ~ctx e0))
                   $ fmt "@," )
               $ fmt "@;<0 -2>"
               $ hvbox 0
                   ( break_unless_newline 1 0 $ fmt "with@ " $ leading_cmt
                   $ hvbox 0
                       ( fmt_pattern c ~pro:(if_newline "| ")
                           (sub_pat ~ctx pc_lhs)
                       $ opt pc_guard (fun g ->
                             fmt "@ when "
                             $ fmt_expression c (sub_exp ~ctx g) ) )
                   $ fmt "@ ->" $ fmt_if parens_here " (" )
               $ fmt "@ "
               $ cbox 0 (fmt_expression c ?parens:parens_for_exp xpc_rhs)
               $ fmt_if parens_here
                   ( if c.conf.indicate_multiline_delimiters then " )"
                   else ")" ) )) )
  | Pexp_pack me ->
      let {opn; pro; psp; bdy; cls; esp; epi} =
        fmt_module_expr c (sub_mod ~ctx me)
      in
      opn
      $ wrap_fits_breaks ~space:false c.conf "(" ")"
          ( fmt "module " $ Option.call ~f:pro $ psp $ bdy $ cls $ esp
          $ Option.call ~f:epi $ fmt_atrs )
  | Pexp_record (flds, default) ->
      let fmt_field (lid1, f) =
        hvbox 0
          (let leading_cmt = Cmts.fmt_before c.cmts lid1.loc in
           leading_cmt
           $
           match f.pexp_desc with
           | Pexp_ident {txt= txt'; loc}
             when field_alias ~field:lid1.txt txt'
                  && List.is_empty f.pexp_attributes ->
               Cmts.fmt c.cmts loc @@ cbox 2 (fmt_longident_loc c lid1)
           | Pexp_constraint
               (({pexp_desc= Pexp_ident {txt= txt'; loc}} as e), t)
             when field_alias ~field:lid1.txt txt'
                  && List.is_empty f.pexp_attributes ->
               Cmts.fmt c.cmts loc
               @@ fmt_expression c (sub_exp ~ctx:(Exp f) e)
               $ fmt " : "
               $ fmt_core_type c (sub_typ ~ctx:(Exp f) t)
           | _ ->
               cbox 2
                 ( fmt_longident_loc c lid1
                 $ fmt_if Poly.(c.conf.field_space = `Loose) " "
                 $ fmt "=@ "
                 $ cbox 0 (fmt_expression c (sub_exp ~ctx f)) ))
      in
      hvbox 0
        ( wrap_record c
            (hovbox (-2)
               ( opt default (fun d ->
                     hvbox 2
                       (fmt_expression c (sub_exp ~ctx d) $ fmt "@;<1 -2>")
                 )
               $ ( fmt_if (Option.is_some default) "with@;<1 2>"
                 $ hvbox (-2) (list flds "@,; " fmt_field) ) ))
        $ fmt_atrs )
  | Pexp_sequence (e1, e2) when Option.is_some ext ->
      let parens1 =
        match e1.pexp_desc with Pexp_sequence _ -> Some true | _ -> None
      in
      hvbox 0
        (hvbox_if parens 2
           ( wrap_fits_breaks_if ~space:false c.conf parens "(" ")"
               ( fmt_expression c ?parens:parens1 (sub_exp ~ctx e1)
               $ fmt " ;"
               $ fmt_extension_suffix c ext
               $ fmt "@ "
               $ fmt_expression c (sub_exp ~ctx e2) )
           $ fmt_atrs ))
  | Pexp_sequence _ ->
      hvbox 0
        (hvbox_if parens 2
           ( wrap_fits_breaks_if ~space:false c.conf parens "(" ")"
               (list
                  (sugar_sequence c width xexp)
                  ( match c.conf.sequence_style with
                  | `Separator -> " ;@;<1000 0>"
                  | `Terminator -> ";@;<1000 0>" )
                  (fun grp ->
                    list grp
                      ( match c.conf.sequence_style with
                      | `Separator when c.conf.break_sequences ->
                          " ;@;<1000 0>"
                      | `Separator -> " ;@ "
                      | `Terminator when c.conf.break_sequences ->
                          ";@;<1000 0>"
                      | `Terminator -> ";@ " )
                      (fmt_expression c) ))
           $ fmt_atrs ))
  | Pexp_setfield (e1, lid, e2) ->
      hvbox 0
        (wrap_fits_breaks_if ~space:false c.conf parens "(" ")"
           ( fmt_expression c (sub_exp ~ctx e1)
           $ fmt "." $ fmt_longident_loc c lid $ fmt "@ <- "
           $ fmt_expression c (sub_exp ~ctx e2)
           $ fmt_atrs ))
  | Pexp_tuple es ->
      let parens =
        match xexp.ctx with
        | Str {pstr_desc= Pstr_eval _} -> false
        | _ -> parens || Poly.(c.conf.parens_tuple = `Always)
      in
      let no_parens_if_break =
        match xexp.ctx with
        | Exp {pexp_desc= Pexp_extension _} -> true
        | Pld _ -> true
        | Str {pstr_desc= Pstr_eval _} -> true
        | _ -> false
      in
      let wrap =
        if parens then wrap_fits_breaks c.conf "(" ")"
        else if no_parens_if_break then Fn.id
        else if c.conf.indicate_multiline_delimiters then
          wrap_if_breaks "( " "@ )"
        else wrap_if_breaks "(" ")"
      in
      hvbox 0
        (wrap (list es "@,, " (sub_exp ~ctx >> fmt_expression c)) $ fmt_atrs)
  | Pexp_lazy e ->
      hvbox 2
        (wrap_fits_breaks_if ~space:false c.conf parens "(" ")"
           (fmt "lazy@ " $ fmt_expression c (sub_exp ~ctx e) $ fmt_atrs))
  | Pexp_extension
      ( ext
      , PStr
          [ ( { pstr_desc=
                  Pstr_eval
                    ( ( { pexp_desc=
                            ( Pexp_while _ | Pexp_for _ | Pexp_match _
                            | Pexp_try _ | Pexp_let _ | Pexp_ifthenelse _
                            | Pexp_sequence _ | Pexp_new _
                            | Pexp_letmodule _ | Pexp_object _
                            | Pexp_function _ )
                        ; pexp_attributes= [] } as e1 )
                    , _ ) } as str ) ] )
    when List.is_empty pexp_attributes
         && ( Poly.(c.conf.extension_sugar = `Always)
            || Source.extension_using_sugar ~name:ext ~payload:e1 ) ->
      hvbox 0
        ( fmt_expression c ~box ?eol ~parens ~ext (sub_exp ~ctx:(Str str) e1)
        $ fmt_atrs )
  | Pexp_extension ext ->
      hvbox 0
        (wrap_fits_breaks_if ~space:false c.conf parens "(" ")"
           (hvbox 2 (fmt_extension c ctx "%" ext) $ fmt_atrs))
  | Pexp_for (p1, e1, e2, dir, e3) ->
      hvbox 0
        (wrap_fits_breaks_if ~space:false c.conf parens "(" ")"
           ( hovbox 0
               ( hvbox 2
                   ( hvbox 0
                       ( fmt "for"
                       $ fmt_extension_suffix c ext
                       $ fmt "@;<1 2>"
                       $ hovbox 0
                           ( fmt_pattern c (sub_pat ~ctx p1)
                           $ fmt "@ =@;<1 2>"
                           $ fmt_expression c (sub_exp ~ctx e1)
                           $ fmt
                               ( if Poly.(dir = Upto) then "@ to "
                               else "@ downto " )
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
        (wrap_fits_breaks_if ~space:false c.conf parens "(" ")"
           ( hovbox 0
               ( hvbox 2
                   ( hvbox 0
                       ( fmt "while"
                       $ fmt_extension_suffix c ext
                       $ fmt "@;<1 2>"
                       $ fmt_expression c (sub_exp ~ctx e1)
                       $ fmt "@;do" )
                   $ fmt "@;<1000 0>"
                   $ fmt_expression c (sub_exp ~ctx e2) )
               $ fmt "@;<1000 0>done" )
           $ fmt_atrs ))
  | Pexp_unreachable -> fmt "."
  | Pexp_send (exp, meth) ->
      hvbox 2
        (wrap_if parens "(" ")"
           ( fmt_expression c (sub_exp ~ctx exp)
           $ fmt "@,#" $ fmt_str_loc c meth $ fmt_atrs ))
  | Pexp_new {txt; loc} ->
      Cmts.fmt c.cmts loc
      @@ hvbox 2
           (wrap_if parens "(" ")"
              ( fmt "new"
              $ fmt_extension_suffix c ext
              $ fmt "@ " $ fmt_longident txt $ fmt_atrs ))
  | Pexp_object {pcstr_self; pcstr_fields} ->
      hvbox 0
        (wrap_if parens "(" ")"
           ( fmt_class_structure c ~ctx ?ext pcstr_self pcstr_fields
           $ fmt_atrs ))
  | Pexp_override l -> (
      let field ({txt; loc}, f) =
        let txt = Longident.parse txt in
        match f.pexp_desc with
        | Pexp_ident {txt= txt'; loc} when field_alias ~field:txt txt' ->
            Cmts.fmt c.cmts ~eol:(fmt "@;<1 3>") loc @@ fmt_longident txt'
        | Pexp_constraint
            (({pexp_desc= Pexp_ident {txt= txt'; loc}} as e), t)
          when field_alias ~field:txt txt' ->
            Cmts.fmt c.cmts ~eol:(fmt "@;<1 3>") loc
            @@ fmt_expression c (sub_exp ~ctx:(Exp f) e)
            $ fmt " : "
            $ fmt_core_type c (sub_typ ~ctx:(Exp f) t)
        | _ ->
            Cmts.fmt c.cmts ~eol:(fmt "@;<1 3>") loc @@ fmt_longident txt
            $ fmt " = "
            $ fmt_expression c (sub_exp ~ctx f)
      in
      match l with
      | [] -> wrap "{<" ">}" (Cmts.fmt_within c.cmts pexp_loc)
      | _ ->
          hvbox 0
            (wrap_if parens "(" ")"
               (wrap_fits_breaks ~space:false c.conf "{<" ">}"
                  (list l "@;<0 1>; " field))) )
  | Pexp_setinstvar (name, expr) ->
      hvbox 0
        (wrap_fits_breaks_if ~space:false c.conf parens "(" ")"
           ( fmt_str_loc c name $ fmt " <-@;<1 2>"
           $ hvbox 2 (fmt_expression c (sub_exp ~ctx expr)) ))
  | Pexp_poly _ ->
      impossible "only used for methods, handled during method formatting"

and fmt_class_structure c ~ctx ?ext self_ fields =
  let fields =
    List.sort fields
      ~compare:
        (Comparable.lift Location.compare_start ~f:(fun x -> x.pcf_loc))
  in
  let _, fields =
    List.fold_map fields ~init:c ~f:(fun c i ->
        let c =
          match i.pcf_desc with
          | Pcf_attribute atr -> update_config c [atr]
          | _ -> c
        in
        (c, (i, c)) )
  in
  let cmts_after_self =
    match fields with
    | [] -> Cmts.fmt_after c.cmts self_.ppat_loc
    | _ -> fmt ""
  in
  let self_ =
    match self_ with
    | {ppat_desc= Ppat_any; ppat_attributes= []} -> None
    | s -> Some s
  in
  hvbox 2
    ( hvbox 0
        ( fmt "object"
        $ fmt_extension_suffix c ext
        $ opt self_ (fun self_ ->
              fmt "@;" $ wrap "(" ")" (fmt_pattern c (sub_pat ~ctx self_))
          ) )
    $ cmts_after_self
    $ ( match fields with
      | ({pcf_desc= Pcf_attribute a}, _) :: _
        when Option.is_some (fst (doc_atrs [a])) ->
          fmt "\n"
      | _ -> fmt "" )
    $ fmt_if Poly.(fields <> []) "@;<1000 0>"
    $ hvbox 0
        (list fields "\n@\n" (fun (cf, c) ->
             maybe_disabled c cf.pcf_loc []
             @@ fun c -> fmt_class_field c ctx cf )) )
  $ fmt_or_k Poly.(fields <> []) (fmt "@\n") (fmt "@ ")
  $ fmt "end"

and fmt_class_signature c ~ctx ~parens ?ext self_ fields =
  let fields =
    List.sort fields
      ~compare:
        (Comparable.lift Location.compare_start ~f:(fun x -> x.pctf_loc))
  in
  let _, fields =
    List.fold_map fields ~init:c ~f:(fun c i ->
        let c =
          match i.pctf_desc with
          | Pctf_attribute atr -> update_config c [atr]
          | _ -> c
        in
        (c, (i, c)) )
  in
  let cmts_after_self =
    match fields with
    | [] -> Cmts.fmt_after c.cmts self_.ptyp_loc
    | _ -> fmt ""
  in
  let self_ =
    match self_ with
    | {ptyp_desc= Ptyp_any; ptyp_attributes= []} -> None
    | s -> Some s
  in
  hvbox 0
    (wrap_if parens "(" ")"
       ( hvbox 2
           ( hvbox 0
               ( fmt "object"
               $ fmt_extension_suffix c ext
               $ opt self_ (fun self_ ->
                     fmt "@;"
                     $ wrap "(" ")" (fmt_core_type c (sub_typ ~ctx self_))
                 ) )
           $ cmts_after_self
           $ ( match fields with
             | ({pctf_desc= Pctf_attribute a}, _) :: _
               when Option.is_some (fst (doc_atrs [a])) ->
                 fmt "\n"
             | _ -> fmt "" )
           $ fmt_if Poly.(fields <> []) "@;<1000 0>"
           $ hvbox 0
               (list fields "\n@\n" (fun (cf, c) ->
                    maybe_disabled c cf.pctf_loc []
                    @@ fun c -> fmt_class_type_field c ctx cf )) )
       $ fmt_or_k Poly.(fields <> []) (fmt "@\n") (fmt "@ ")
       $ fmt "end" ))

and fmt_class_type c ?(box = true) ({ast= typ} as xtyp) =
  protect (Cty typ)
  @@
  let {pcty_desc; pcty_loc; pcty_attributes} = typ in
  update_config_maybe_disabled c pcty_loc pcty_attributes
  @@ fun c ->
  let doc, atrs = doc_atrs pcty_attributes in
  Cmts.fmt c.cmts pcty_loc
  @@ ( if List.is_empty atrs then Fn.id
     else fun k -> k $ fmt_attributes c ~key:"@" atrs )
  @@
  let parens = parenze_cty xtyp in
  ( hvbox_if box 0 @@ wrap_if parens "(" ")"
  @@
  let ctx = Cty typ in
  match pcty_desc with
  | Pcty_constr (name, params) ->
      let params = List.map params ~f:(fun x -> (x, Invariant)) in
      fmt_class_params c ctx ~epi:(fmt "@ ") params
      $ fmt_longident_loc c name
  | Pcty_signature {pcsig_self; pcsig_fields} ->
      fmt_class_signature c ~ctx ~parens pcsig_self pcsig_fields
  | Pcty_arrow (_, _, _) ->
      let arg_label lbl =
        match lbl with
        | Nolabel -> fmt ""
        | Labelled l -> str l $ fmt ":"
        | Optional l -> fmt "?" $ str l $ fmt ":"
      in
      let xt1N = sugar_class_arrow_typ c (sub_cty ~ctx typ) in
      hvbox_if box 0
        (list xt1N "@;-> " (fun (lI, xtI) ->
             hvbox 0
               ( arg_label lI
               $
               match xtI with
               | `core_type ct -> fmt_core_type c ct
               | `class_type ct -> fmt_class_type c ct ) ))
  | Pcty_extension ext -> fmt_extension c ctx "%" ext
  | Pcty_open (flag, lid, cl) ->
      hvbox 0
        ( str "let open"
        $ fmt_if Poly.(flag = Override) "!"
        $ str " " $ fmt_longident_loc c lid $ fmt " in@;<1000 0>"
        $ fmt_class_type c (sub_cty ~ctx cl) ) )
  $ fmt_docstring c ~pro:(fmt "@ ") doc

and fmt_class_expr c ?eol ?(box = true) ({ast= exp} as xexp) =
  protect (Cl exp)
  @@
  let {pcl_desc; pcl_loc; pcl_attributes} = exp in
  update_config_maybe_disabled c pcl_loc pcl_attributes
  @@ fun c ->
  let parens = parenze_cl xexp in
  let fmt_label lbl sep =
    match lbl with
    | Nolabel -> fmt ""
    | Labelled l -> fmt "~" $ str l $ fmt sep
    | Optional l -> fmt "?" $ str l $ fmt sep
  in
  let fmt_label_arg ?(box = box) ?epi ?parens (lbl, ({ast= arg} as xarg)) =
    match (lbl, arg.pexp_desc) with
    | (Labelled l | Optional l), Pexp_ident {txt= Lident i; loc}
      when String.equal l i ->
        Cmts.fmt c.cmts loc
        @@ Cmts.fmt c.cmts ?eol arg.pexp_loc
        @@ fmt_label lbl ""
    | _ ->
        hvbox_if box 2
          (fmt_label lbl ":@," $ fmt_expression c ~box ?epi ?parens xarg)
  in
  let ctx = Cl exp in
  let width xe = String.length (Cmts.preserve (fmt_expression c) xe) in
  let fmt_args_grouped e0 a1N =
    (* TODO: consider [e0] when grouping *)
    fmt_class_expr c (sub_cl ~ctx e0)
    $ fmt "@ "
    $ list_fl
        (List.group a1N ~break:(fun (_, a1) (_, a2) ->
             (not (is_simple c.conf width (sub_exp ~ctx a1)))
             || not (is_simple c.conf width (sub_exp ~ctx a2)) ))
        (fun ~first:first_grp ~last:last_grp args ->
          list_pn args (fun ?prev (lbl, arg) ?next ->
              let ({ast} as xarg) = sub_exp ~ctx arg in
              let openbox =
                fmt_if_k (Option.is_none prev)
                  (open_hovbox (if first_grp then 2 else 0))
              in
              let consecutive_prefix_ops =
                is_prefix ast
                &&
                match next with
                | Some (_, e) -> exposed_left_exp e
                | _ -> false
              in
              let spc =
                consecutive_prefix_ops || (not last_grp)
                || Option.is_some next
              in
              openbox
              $ hovbox 2
                  (fmt_label_arg
                     ?box:
                       ( match ast.pexp_desc with
                       | Pexp_fun _ | Pexp_function _ -> Some false
                       | _ -> None )
                     ?epi:
                       ( match (lbl, next) with
                       | _, None -> None
                       | Nolabel, _ -> Some (fits_breaks "" "@;<1000 -1>")
                       | _ -> Some (fits_breaks "" "@;<1000 -3>") )
                     (lbl, xarg))
              $ fmt_if_k (Option.is_none next) close_box
              $ fmt_if_k spc (break_unless_newline 1 0) ) )
  in
  let fmt_cmts = Cmts.fmt c.cmts ?eol pcl_loc in
  let fmt_atrs = fmt_attributes c ~pre:(fmt " ") ~key:"@" pcl_attributes in
  hvbox_if box 0 @@ fmt_cmts
  @@
  match pcl_desc with
  | Pcl_constr (name, params) ->
      let params = List.map params ~f:(fun x -> (x, Invariant)) in
      fmt_class_params c ctx ~epi:(fmt "@ ") params
      $ fmt_longident_loc c name $ fmt_atrs
  | Pcl_structure {pcstr_fields; pcstr_self} ->
      hvbox 0
        (wrap_if parens "(" ")"
           ( fmt_class_structure c ~ctx ?ext:None pcstr_self pcstr_fields
           $ fmt_atrs ))
  | Pcl_fun _ ->
      let xargs, xbody = sugar_cl_fun c xexp in
      hvbox_if box
        (if Option.is_none eol then 2 else 1)
        ( fmt_if parens "("
        $ ( open_hovbox 2
          $ ( hovbox 4
                ( fmt "fun "
                $ fmt_attributes c ~key:"@" pcl_attributes ~suf:(fmt " ")
                $ hvbox_if
                    (not c.conf.wrap_fun_args)
                    0 (fmt_fun_args c xargs)
                $ fmt "@ " )
            $ fmt "->" )
          $ close_box $ fmt "@ "
          $ fmt_class_expr c ~eol:(fmt "@;<1000 0>") xbody )
        $ fmt_or_k c.conf.indicate_multiline_delimiters
            (fits_breaks_if parens ")" "@ )")
            (fits_breaks_if parens ")" "@,)") )
  | Pcl_apply (e0, e1N1) ->
      wrap_if parens "(" ")" (hvbox 2 (fmt_args_grouped e0 e1N1) $ fmt_atrs)
  | Pcl_let (rec_flag, bindings, body) ->
      wrap_if parens "(" ")"
        (vbox 0
           ( hvbox 0
               (list_fl bindings (fun ~first ~last binding ->
                    fmt_value_binding c ~rec_flag ~first
                      ?ext:(if first then None else None)
                      ctx binding
                      ~in_:(fun indent ->
                        fmt_if_k last (break 1 (-indent) $ fmt "in") )
                    $ fmt_if (not last)
                        ( match c.conf.let_and with
                        | `Sparse -> "@;<1000 0>"
                        | `Compact -> "@ " ) ))
           $ fmt "@;<1000 0>"
           $ hvbox 0 (fmt_class_expr c (sub_cl ~ctx body)) ))
      $ fmt_atrs
  | Pcl_constraint (e, t) ->
      hvbox 2
        (wrap_fits_breaks ~space:false c.conf "(" ")"
           ( fmt_class_expr c (sub_cl ~ctx e)
           $ fmt "@ : "
           $ fmt_class_type c (sub_cty ~ctx t) ))
      $ fmt_atrs
  | Pcl_extension ext -> fmt_extension c ctx "%" ext $ fmt_atrs
  | Pcl_open (flag, lid, cl) ->
      hvbox 0
        ( str "let open"
        $ fmt_if Poly.(flag = Override) "!"
        $ str " " $ fmt_longident_loc c lid $ fmt " in@;<1000 0>"
        $ fmt_class_expr c (sub_cl ~ctx cl)
        $ fmt_atrs )

and fmt_class_field c ctx (cf : class_field) =
  let {pcf_desc; pcf_loc; pcf_attributes} = cf in
  update_config_maybe_disabled c pcf_loc pcf_attributes
  @@ fun c ->
  let fmt_cmts = Cmts.fmt c.cmts ?eol:None pcf_loc in
  let doc, atrs = doc_atrs pcf_attributes in
  let fmt_atrs = fmt_attributes c ~pre:(fmt " ") ~key:"@@" atrs in
  let fmt_kind = function
    | Cfk_virtual typ ->
        ([fmt "@ : " $ fmt_core_type c (sub_typ ~ctx typ)], fmt "", fmt "")
    | Cfk_concrete
        ( _
        , { pexp_desc=
              Pexp_poly
                (e, Some ({ptyp_desc= Ptyp_poly (poly_args, _)} as poly))
          ; pexp_loc } ) -> (
        let rec cleanup names e args' =
          match (e, args') with
          | {pexp_desc= Pexp_constraint (e, t)}, [] ->
              Some (List.rev names, t, e)
          | ( {pexp_desc= Pexp_newtype (({txt} as newtyp), body)}
            , {txt= txt'} :: args )
            when String.equal txt txt' ->
              cleanup (newtyp :: names) body args
          | _ -> None
        in
        match cleanup [] e poly_args with
        | Some (args, t, e) ->
            ( match args with
            | [] ->
                Cmts.relocate c.cmts ~src:pexp_loc ~before:e.pexp_loc
                  ~after:e.pexp_loc
            | x :: _ ->
                Cmts.relocate c.cmts ~src:pexp_loc ~before:x.loc
                  ~after:e.pexp_loc ) ;
            ( [ fmt "@ : " $ fmt "type "
                $ list args "@ " (fun name -> fmt_str_loc c name)
              ; fmt_core_type c ~pro:"." (sub_typ ~ctx t) ]
            , fmt "@;<1 2>="
            , fmt "@ " $ fmt_expression c (sub_exp ~ctx e) )
        | None ->
            ( [fmt "@ : " $ fmt_core_type c (sub_typ ~ctx poly)]
            , fmt "@;<1 2>="
            , fmt "@ " $ fmt_expression c (sub_exp ~ctx e) ) )
    | Cfk_concrete (_, {pexp_desc= Pexp_poly (e, poly); pexp_loc}) ->
        let xargs, xbody =
          match poly with
          | None ->
              sugar_fun c ~will_keep_first_ast_node:false (sub_exp ~ctx e)
          | Some _ -> ([], sub_exp ~ctx e)
        in
        let ty, e =
          match (xbody.ast, poly) with
          | {pexp_desc= Pexp_constraint (e, t); pexp_loc}, None ->
              Cmts.relocate c.cmts ~src:pexp_loc ~before:t.ptyp_loc
                ~after:e.pexp_loc ;
              (Some t, sub_exp ~ctx e)
          | {pexp_desc= Pexp_constraint _}, Some _ -> (poly, xbody)
          | _, poly -> (poly, xbody)
        in
        Cmts.relocate c.cmts ~src:pexp_loc ~before:e.ast.pexp_loc
          ~after:e.ast.pexp_loc ;
        ( [ fmt_if (not (List.is_empty xargs)) "@;<1 2>"
            $ hvbox_if (not c.conf.wrap_fun_args) 0 (fmt_fun_args c xargs)
          ; opt ty (fun t -> fmt "@ : " $ fmt_core_type c (sub_typ ~ctx t))
          ]
        , fmt "@;<1 2>="
        , fmt "@ " $ fmt_expression c e )
    | Cfk_concrete (_, e) ->
        let ty, e =
          match e with
          | {pexp_desc= Pexp_constraint (e, t)} -> (Some t, e)
          | _ -> (None, e)
        in
        ( [opt ty (fun t -> fmt "@ : " $ fmt_core_type c (sub_typ ~ctx t))]
        , fmt "@;<1 2>="
        , fmt "@ " $ fmt_expression c (sub_exp ~ctx e) )
  in
  let virtual_or_override = function
    | Cfk_virtual _ -> fmt "@ virtual"
    | Cfk_concrete (Override, _) -> fmt "!"
    | Cfk_concrete (Fresh, _) -> fmt ""
  in
  fmt_cmts
  @@ ( fmt_docstring c ~epi:(fmt "@\n") doc
     $ ( hvbox 0
       @@
       match pcf_desc with
       | Pcf_inherit (override, cl, parent) ->
           hovbox 2
             ( fmt "inherit"
             $ fmt_if Poly.(override = Override) "!"
             $ fmt "@ "
             $ ( fmt_class_expr c (sub_cl ~ctx cl)
               $ opt parent (fun p -> fmt " as " $ fmt_str_loc c p) ) )
       | Pcf_method (name, priv, kind) ->
           let l, eq, expr = fmt_kind kind in
           hvbox 2
             ( hovbox 2
                 ( hovbox 4
                     ( fmt "method" $ virtual_or_override kind
                     $ fmt_if Poly.(priv = Private) "@ private"
                     $ fmt "@ " $ fmt_str_loc c name $ list l "" Fn.id )
                 $ eq )
             $ expr )
       | Pcf_val (name, mut, kind) ->
           let l, eq, expr = fmt_kind kind in
           hvbox 2
             ( hovbox 2
                 ( hvbox 4
                     ( fmt "val" $ virtual_or_override kind
                     $ fmt_if Poly.(mut = Mutable) "@ mutable"
                     $ fmt "@ " $ fmt_str_loc c name $ list l "" Fn.id )
                 $ eq )
             $ expr )
       | Pcf_constraint (t1, t2) ->
           fmt "constraint" $ fmt "@ "
           $ fmt_core_type c (sub_typ ~ctx t1)
           $ fmt " = "
           $ fmt_core_type c (sub_typ ~ctx t2)
       | Pcf_initializer e ->
           fmt "initializer" $ fmt "@ " $ fmt_expression c (sub_exp ~ctx e)
       | Pcf_attribute atr ->
           let doc, atrs = doc_atrs [atr] in
           fmt_docstring c ~standalone:true ~epi:(fmt "") doc
           $ fmt_attributes c ~key:"@@@" atrs
       | Pcf_extension ext -> fmt_extension c ctx "%%" ext )
     $ fmt_atrs )

and fmt_class_type_field c ctx (cf : class_type_field) =
  let {pctf_desc; pctf_loc; pctf_attributes} = cf in
  update_config_maybe_disabled c pctf_loc pctf_attributes
  @@ fun c ->
  let fmt_cmts = Cmts.fmt c.cmts ?eol:None pctf_loc in
  let doc, atrs = doc_atrs pctf_attributes in
  let fmt_atrs = fmt_attributes c ~pre:(fmt " ") ~key:"@@" atrs in
  fmt_cmts
    ( fmt_docstring c ~epi:(fmt "@\n") doc
    $ ( hvbox 0
      @@
      match pctf_desc with
      | Pctf_inherit ct ->
          hovbox 2
            (fmt "inherit" $ fmt "@ " $ fmt_class_type c (sub_cty ~ctx ct))
      | Pctf_method (name, priv, virt, ty) ->
          hovbox 2
            ( fmt "method"
            $ fmt_if Poly.(virt = Virtual) "@ virtual"
            $ fmt_if Poly.(priv = Private) "@ private"
            $ fmt "@ " $ fmt_str_loc c name $ fmt " :@ "
            $ fmt_core_type c (sub_typ ~ctx ty) )
      | Pctf_val (name, mut, virt, ty) ->
          hovbox 2
            ( fmt "val"
            $ fmt_if Poly.(virt = Virtual) "@ virtual"
            $ fmt_if Poly.(mut = Mutable) "@ mutable"
            $ fmt "@ " $ fmt_str_loc c name $ fmt " :@ "
            $ fmt_core_type c (sub_typ ~ctx ty) )
      | Pctf_constraint (t1, t2) ->
          fmt "constraint" $ fmt "@ "
          $ fmt_core_type c (sub_typ ~ctx t1)
          $ fmt " = "
          $ fmt_core_type c (sub_typ ~ctx t2)
      | Pctf_attribute atr ->
          let doc, atrs = doc_atrs [atr] in
          fmt_docstring c ~standalone:true ~epi:(fmt "") doc
          $ fmt_attributes c ~key:"@@@" atrs
      | Pctf_extension ext -> fmt_extension c ctx "%%" ext )
    $ fmt_atrs )

and fmt_cases c ctx cs =
  list_fl cs (fun ~first ~last:_ {pc_lhs; pc_guard; pc_rhs} ->
      let xrhs = sub_exp ~ctx pc_rhs in
      let indent =
        match (ctx, pc_rhs.pexp_desc) with
        | ( Exp {pexp_desc= Pexp_function _ | Pexp_match _ | Pexp_try _}
          , (Pexp_match _ | Pexp_try _) ) ->
            2
        | _ -> c.conf.cases_exp_indent
      in
      let parens_here, parens_for_exp =
        if c.conf.leading_nested_match_parens then (false, None)
        else (parenze_exp xrhs, Some false)
      in
      (* side effects of Cmts.fmt_before before [fmt_lhs] is important *)
      let leading_cmt = Cmts.fmt_before c.cmts pc_lhs.ppat_loc in
      let fmt_lhs =
        let xlhs = sub_pat ~ctx pc_lhs in
        let paren_lhs =
          match pc_lhs.ppat_desc with
          | Ppat_or _ when Option.is_some pc_guard -> true
          | _ -> parenze_pat xlhs
        in
        let fmt_arrow_close_box =
          fmt_or_k
            (break_cases_level c > 0)
            (fmt_or_k parens_here (fmt "@;<1 2>->@]")
               (fmt "@;<1 -2>->@]@;<0 3>"))
            ( fmt "@;<1 -2>->@]"
            $ fmt_or_k parens_here (fmt " (") (fmt "@;<0 -1>") )
        in
        let pro =
          fmt_or_k
            (break_cases_level c > 1)
            (break_unless_newline 1000 0 $ fmt "| ")
            (if first then if_newline "| " else fmt "| ")
        in
        hovbox 4
          ( open_hovbox (if break_cases_level c = 0 then 2 else 0)
          $ hvbox 0
              ( fmt_pattern c ~pro ~parens:paren_lhs xlhs
              $ opt pc_guard (fun g ->
                    fmt "@;<1 2>when " $ fmt_expression c (sub_exp ~ctx g)
                ) )
          $ fmt_if_k (indent <= 2) fmt_arrow_close_box )
        $ fmt_if_k (indent > 2) fmt_arrow_close_box
      in
      fmt_if (not first) "@ " $ leading_cmt
      $ cbox_if
          (break_cases_level c = 0)
          indent
          ( hvbox_if (break_cases_level c = 0) indent fmt_lhs
          $ ( match (break_cases_level c > 0, indent > 2, parens_here) with
            | false, _, _ -> fmt "@ "
            | true, false, false -> fmt "@;<1 2>"
            | true, false, true -> fmt " (@;<1 2>"
            | true, true, false -> fmt " "
            | true, true, true -> fmt " (@;<1 4>" )
          $ hovbox 0
              ( hovbox 0 (fmt_expression c ?parens:parens_for_exp xrhs)
              $ fmt_or_k c.conf.indicate_multiline_delimiters
                  (fmt_if parens_here "@ )")
                  (fmt_if parens_here "@,)") ) ) )

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
  let doc, atrs = doc_atrs pval_attributes in
  let doc_before =
    match c.conf.doc_comments with `Before -> true | `After -> false
  in
  hvbox 0
    ( fmt_if_k doc_before (fmt_docstring c ~epi:(fmt "@\n") doc)
    $ hvbox 2
        ( str pre $ fmt " "
        $ Cmts.fmt c.cmts loc
            (wrap_if (is_symbol_id txt) "( " " )" (str txt))
        $ fmt " "
        $ fmt_core_type c ~pro:":" (sub_typ ~ctx pval_type)
        $ list_fl pval_prim (fun ~first ~last:_ s ->
              fmt_if first "@ =" $ fmt " \"" $ str s $ fmt "\"" ) )
    $ fmt_attributes c ~pre:(fmt "@;<1 2>") ~box:false ~key:"@@" atrs
    $ fmt_if_k (not doc_before) (fmt_docstring c ~pro:(fmt "@\n") doc) )

and fmt_tydcl_params c ctx params =
  fmt_if_k
    (not (List.is_empty params))
    (hvbox 0
       ( wrap_fits_breaks_if ~space:false c.conf
           (List.length params > 1)
           "(" ")"
           (list params "@,, " (fun (ty, vc) ->
                fmt_variance vc $ fmt_core_type c (sub_typ ~ctx ty) ))
       $ fmt " " ))

and fmt_class_params c ctx ~epi params =
  fmt_if_k
    (not (List.is_empty params))
    (hvbox 0
       ( wrap_fits_breaks c.conf "[" "]"
           (list_fl params (fun ~first ~last (ty, vc) ->
                fmt_if (first && exposed_left_typ ty) " "
                $ fmt_if_k (not first) (fmt "@,, ")
                $ fmt_variance vc
                $ fmt_core_type c (sub_typ ~ctx ty)
                $ fmt_if (last && exposed_right_typ ty) " " ))
       $ epi ))

and fmt_private_flag flag = fmt_if Poly.(flag = Private) "@ private"

and fmt_type_declaration c ?(pre = "") ?(suf = ("" : _ format)) ?(brk = suf)
    ctx ?fmt_name ?(eq = "=") decl =
  let fmt_manifest ~priv manifest =
    opt manifest (fun typ ->
        fmt " " $ str eq $ fmt_private_flag priv $ fmt "@ "
        $ fmt_core_type c ~in_type_declaration:true (sub_typ ~ctx typ) )
  in
  let fmt_manifest_kind mfst priv kind =
    match kind with
    | Ptype_abstract -> fmt_manifest ~priv mfst
    | Ptype_variant [] ->
        hvbox 2
          (fmt_manifest ~priv:Public mfst $ fmt " =" $ fmt_private_flag priv)
        $ fmt "@ |"
    | Ptype_variant ctor_decls ->
        hvbox 2
          (fmt_manifest ~priv:Public mfst $ fmt " =" $ fmt_private_flag priv)
        $ fmt "@ "
        $ list_fl ctor_decls (fmt_constructor_declaration c ctx)
    | Ptype_record lbl_decls ->
        hvbox 2
          (fmt_manifest ~priv:Public mfst $ fmt " =" $ fmt_private_flag priv)
        $ fmt "@ "
        $ hvbox 0
            (wrap_record c
               (list_fl lbl_decls (fun ~first ~last x ->
                    fmt_if (not first)
                      ( match c.conf.type_decl with
                      | `Sparse -> "@;<1000 0>; "
                      | `Compact -> "@,; " )
                    $ fmt_label_declaration c ctx x
                    $ fmt_if (last && exposed_right_typ x.pld_type) " " )))
    | Ptype_open ->
        fmt_manifest ~priv:Public mfst
        $ fmt " =" $ fmt_private_flag priv $ fmt " .."
  in
  let fmt_cstrs cstrs =
    fmt_if_k
      (not (List.is_empty cstrs))
      ( fmt "@ "
      $ hvbox 0
          (list cstrs "@ " (fun (t1, t2, loc) ->
               Cmts.fmt c.cmts loc
                 (hvbox 2
                    ( fmt "constraint@ "
                    $ fmt_core_type c (sub_typ ~ctx t1)
                    $ fmt " =@ "
                    $ fmt_core_type c (sub_typ ~ctx t2) )) )) )
  in
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
  let doc, atrs = doc_atrs ptype_attributes in
  Cmts.fmt c.cmts loc @@ Cmts.fmt c.cmts ptype_loc
  @@ hvbox 0
       ( fmt_docstring c ~epi:(fmt "@\n") doc
       $ hvbox 0
           ( hvbox 2
               ( str pre
               $ fmt_tydcl_params c ctx ptype_params
               $ (match fmt_name with Some pp -> pp | None -> str txt)
               $ fmt_manifest_kind ptype_manifest ptype_private ptype_kind
               $ fmt_cstrs ptype_cstrs )
           $ fmt_attributes c ~pre:(fmt "@ ") ~key:"@@" atrs ) )
  $ fmt brk

and fmt_label_declaration c ctx lbl_decl =
  let {pld_mutable; pld_name; pld_type; pld_loc; pld_attributes} =
    lbl_decl
  in
  update_config_maybe_disabled c pld_loc pld_attributes
  @@ fun c ->
  let doc, atrs = doc_atrs pld_attributes in
  let fmt_cmts = Cmts.fmt c.cmts ~eol:(break_unless_newline 1 2) pld_loc in
  fmt_cmts
  @@ hvbox 4
       ( hvbox 3
           ( hvbox 2
               ( fmt_if Poly.(pld_mutable = Mutable) "mutable "
               $ fmt_str_loc c pld_name
               $ fmt_if Poly.(c.conf.field_space = `Loose) " "
               $ fmt ":@ "
               $ fmt_core_type c (sub_typ ~ctx pld_type) )
           $ fmt_attributes c ~pre:(fmt "@;<1 1>") ~box:false ~key:"@" atrs
           )
       $ fmt_docstring c ~pro:(fmt "@;<2 0>") doc )

and fmt_constructor_declaration c ctx ~first ~last:_ cstr_decl =
  let {pcd_name= {txt; loc}; pcd_args; pcd_res; pcd_attributes; pcd_loc} =
    cstr_decl
  in
  update_config_maybe_disabled c pcd_loc pcd_attributes
  @@ fun c ->
  let doc, atrs = doc_atrs pcd_attributes in
  fmt_if (not first)
    ( match c.conf.type_decl with
    | `Sparse -> "@;<1000 0>"
    | `Compact -> "@ " )
  $ Cmts.fmt_before c.cmts pcd_loc
  $ Cmts.fmt_before c.cmts loc
  $ fmt_or_k first (if_newline "| ") (fmt "| ")
  $ hovbox 2
      ( hvbox 2
          ( Cmts.fmt c.cmts loc
              (wrap_if (is_symbol_id txt) "( " " )" (str txt))
          $ fmt_constructor_arguments_result c ctx pcd_args pcd_res )
      $ fmt_attributes c ~pre:(fmt "@;") ~key:"@" atrs
      $ fmt_docstring c ~pro:(fmt "@;<2 0>") doc )
  $ Cmts.fmt_after c.cmts ?pro:None ~epi:(fmt "@ ") pcd_loc

and fmt_constructor_arguments c ctx pre args =
  match args with
  | Pcstr_tuple [] -> fmt ""
  | Pcstr_tuple typs ->
      fmt pre $ hvbox 0 (list typs "@ * " (sub_typ ~ctx >> fmt_core_type c))
  | Pcstr_record lds ->
      fmt pre
      $ wrap_record c (list lds "@,; " (fmt_label_declaration c ctx))

and fmt_constructor_arguments_result c ctx args res =
  let pre : _ format = if Option.is_none res then " of@ " else " :@ " in
  let before_type : _ format =
    match args with Pcstr_tuple [] -> ": " | _ -> "-> "
  in
  fmt_constructor_arguments c ctx pre args
  $ opt res (fun typ ->
        fmt "@ " $ fmt before_type $ fmt_core_type c (sub_typ ~ctx typ) )

and fmt_type_extension c ctx te =
  let { ptyext_params
      ; ptyext_path= lid
      ; ptyext_private
      ; ptyext_constructors
      ; ptyext_attributes } =
    te
  in
  let c = update_config c ptyext_attributes in
  let doc, atrs = doc_atrs ptyext_attributes in
  hvbox 2
    ( fmt_docstring c ~epi:(fmt "@,") doc
    $ hvbox 2
        ( fmt "type "
        $ fmt_tydcl_params c ctx ptyext_params
        $ fmt_longident_loc c lid $ fmt " +="
        $ fmt_private_flag ptyext_private
        $ fmt "@ "
        $ hvbox 0
            ( if_newline "| "
            $ list ptyext_constructors "@ | " (fun ctor ->
                  let has_res =
                    match ctor.pext_kind with
                    | Pext_decl (_, r) -> Option.is_some r
                    | Pext_rebind _ -> false
                  in
                  hvbox 0
                    (fmt_extension_constructor c
                       (if has_res then fmt " :@ " else fmt " of@ ")
                       ctx ctor) ) ) )
    $ fmt_attributes c ~pre:(fmt "@ ") ~key:"@@" atrs )

and fmt_exception ~pre c sep ctx te =
  let atrs, te =
    (* This won't be needed once https://github.com/ocaml/ocaml/pull/1705 is
       merged in the compiler and ocaml-migrate-parsetree. Until then, this
       heuristic will try to discriminate between attributes belonging to
       the constructor, and the one belonging to the exception construct. *)
    let at, atat =
      List.partition_tf
        ~f:(fun (s, _) ->
          match s.txt with
          | "deprecated" | "ocaml.deprecated" -> true
          | _ -> false )
        te.pext_attributes
    in
    (atat, {te with pext_attributes= at})
  in
  let doc, atrs = doc_atrs atrs in
  hvbox 2
    ( fmt_docstring c ~epi:(fmt "@,") doc
    $ hvbox 2 (pre $ fmt_extension_constructor c sep ctx te)
    $ fmt_attributes c ~pre:(fmt "@ ") ~key:"@@" atrs )

and fmt_extension_constructor c sep ctx ec =
  let {pext_name; pext_kind; pext_attributes; pext_loc} = ec in
  update_config_maybe_disabled c pext_loc pext_attributes
  @@ fun c ->
  let doc, atrs = doc_atrs pext_attributes in
  Cmts.fmt c.cmts pext_loc
  @@ hvbox 4
       ( hvbox 2
           ( fmt_str_loc c pext_name
           $
           match pext_kind with
           | Pext_decl ((Pcstr_tuple [] | Pcstr_record []), None) -> fmt ""
           | Pext_decl ((Pcstr_tuple [] | Pcstr_record []), Some res) ->
               sep $ fmt_core_type c (sub_typ ~ctx res)
           | Pext_decl (args, res) ->
               fmt_constructor_arguments_result c ctx args res
           | Pext_rebind lid -> fmt " = " $ fmt_longident_loc c lid )
       $ fmt_attributes c ~pre:(fmt "@ ") ~key:"@" atrs
           ~suf:
             ( match pext_kind with
             | Pext_decl ((Pcstr_tuple [] | Pcstr_record []), None)
              |Pext_decl (_, None)
              |Pext_rebind _ ->
                 fmt ""
             | Pext_decl ((Pcstr_tuple [] | Pcstr_record []), Some _)
              |Pext_decl (_, Some _) ->
                 fmt " " )
       $ fmt_docstring c ~pro:(fmt "@;<2 0>") doc )

and fmt_module_type c ({ast= mty} as xmty) =
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
      let before = Cmts.fmt_before c.cmts pmty_loc in
      let within = Cmts.fmt_within c.cmts ~pro:(fmt "") pmty_loc in
      let after = Cmts.fmt_after c.cmts pmty_loc in
      { opn= open_hvbox 0
      ; pro=
          Some
            ( before
            $ fmt_docstring c ~epi:(fmt "@,") doc
            $ fmt "sig" $ fmt_if empty " " )
      ; psp= fmt_if (not empty) "@;<1000 2>"
      ; bdy= within $ fmt_signature c ctx s
      ; cls= close_box
      ; esp= fmt_if (not empty) "@;<1000 0>"
      ; epi=
          Some
            ( fmt "end" $ after
            $ fmt_attributes c ~key:"@" atrs ~pre:(fmt "@ ") ) }
  | Pmty_functor _ ->
      let xargs, mt2 = sugar_functor_type c ~for_functor_kw:true xmty in
      let blk = fmt_module_type c mt2 in
      { blk with
        pro=
          Some
            ( fmt "functor"
            $ fmt_attributes c ~pre:(fmt " ") ~key:"@" pmty_attributes
            $ fmt "@;<1 2>"
            $ list xargs "@;<1 2>" (fun (name, mt1) ->
                  let mt1 = Option.map ~f:(fmt_module_type c) mt1 in
                  wrap "(" ")"
                    (hovbox 0
                       ( fmt_str_loc c name
                       $ opt mt1 (fun mt1 ->
                             let {opn; pro; psp; bdy; cls; esp; epi} =
                               mt1
                             in
                             opn $ fmt " :" $ Option.call ~f:pro $ psp
                             $ fmt "@;<1 2>" $ bdy $ esp
                             $ Option.call ~f:epi $ cls ) )) )
            $ fmt "@;<1 2>-> " $ Option.call ~f:blk.pro )
      ; epi= Some (Option.call ~f:blk.epi $ Cmts.fmt_after c.cmts pmty_loc)
      }
  | Pmty_with _ ->
      let wcs, mt = sugar_mod_with c (sub_mty ~ctx mty) in
      let {opn; pro; psp; bdy; cls; esp; epi} = fmt_module_type c mt in
      { empty with
        bdy=
          hvbox 0
            (wrap_if parens "(" ")"
               ( opn $ Option.call ~f:pro $ psp $ bdy $ esp
               $ Option.call ~f:epi $ cls
               $ list_fl wcs (fun ~first:_ ~last:_ (wcs_and, loc) ->
                     Cmts.fmt c.cmts loc
                     @@ list_fl wcs_and (fun ~first ~last:_ wc ->
                            fmt_or first "@ with" "@;<1 1>and"
                            $ fmt_with_constraint c ctx wc ) ) ))
      ; epi=
          Some
            ( fmt_attributes c ~key:"@" pmty_attributes ~pre:(fmt "@ ")
            $ Cmts.fmt_after c.cmts pmty_loc ) }
  | Pmty_typeof me -> (
      let blk = fmt_module_expr c (sub_mod ~ctx me) in
      match blk.pro with
      | Some pro ->
          { blk with
            pro=
              Some
                ( Cmts.fmt_before c.cmts pmty_loc
                $ (fmt_if parens "(" $ fmt "module type of " $ pro) )
          ; epi=
              Some
                ( Option.call ~f:blk.epi
                $ Cmts.fmt_after c.cmts pmty_loc
                $ fmt_if parens ")"
                $ fmt_attributes c ~key:"@" pmty_attributes ~pre:(fmt "@ ")
                ) }
      | _ ->
          { blk with
            bdy=
              Cmts.fmt c.cmts pmty_loc
              @@ hvbox 2
                   (fmt_if parens "(" $ fmt "module type of@ " $ blk.bdy)
          ; epi=
              Some
                ( Option.call ~f:blk.epi
                $ Cmts.fmt_after c.cmts pmty_loc
                $ fmt_if parens ")"
                $ fmt_attributes c ~key:"@" pmty_attributes ~pre:(fmt "@ ")
                ) } )
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
  let _, itms =
    List.fold_map itms ~init:c ~f:(fun c i ->
        let c =
          match i.psig_desc with
          | Psig_attribute atr -> update_config c [atr]
          | _ -> c
        in
        (c, (i, c)) )
  in
  let grps =
    List.group itms ~break:(fun (itmI, cI) (itmJ, cJ) ->
        Ast.break_between c.cmts (Sig itmI, cI.conf) (Sig itmJ, cJ.conf) )
  in
  let fmt_grp itms =
    list itms "@\n" (fun (i, c) ->
        maybe_disabled c i.psig_loc []
        @@ fun c -> fmt_signature_item c (sub_sig ~ctx i) )
  in
  hvbox 0 (list grps "\n@;<1000 0>" fmt_grp)

and fmt_signature_item c {ast= si} =
  protect (Sig si)
  @@
  let fmt_cmts_before =
    Cmts.fmt_before c.cmts ~epi:(fmt "\n@\n") ~eol:(fmt "\n@\n")
      ~adj:(fmt "@\n") si.psig_loc
  and fmt_cmts_after =
    Cmts.fmt_after c.cmts ~pro:(fmt "\n@\n") si.psig_loc
  in
  wrap_k fmt_cmts_before fmt_cmts_after
  @@
  let ctx = Sig si in
  match si.psig_desc with
  | Psig_attribute atr ->
      let doc, atrs = doc_atrs [atr] in
      fmt_docstring c ~standalone:true ~epi:(fmt "") doc
      $ fmt_attributes c ~key:"@@@" atrs
  | Psig_exception exc ->
      hvbox 2
        (fmt_exception ~pre:(fmt "exception@ ") c (fmt " of@ ") ctx exc)
  | Psig_extension (ext, atrs) ->
      hvbox 0
        ( fmt_extension c ctx "%%" ext
        $ fmt_attributes c ~pre:(fmt "@ ") ~key:"@@" atrs )
  | Psig_include {pincl_mod; pincl_attributes; pincl_loc} ->
      update_config_maybe_disabled c pincl_loc pincl_attributes
      @@ fun c ->
      let doc, atrs = doc_atrs pincl_attributes in
      let keyword, {opn; pro; psp; bdy; cls; esp; epi} =
        match pincl_mod with
        | {pmty_desc= Pmty_typeof me} ->
            let blk = fmt_module_expr c (sub_mod ~ctx me) in
            ( fmt "include module type of"
              $ fmt_or (Option.is_some blk.pro) " " "@ "
            , blk )
        | _ -> (fmt "include@ ", fmt_module_type c (sub_mty ~ctx pincl_mod))
      in
      hvbox 0
        ( fmt_docstring c ~epi:(fmt "@,") doc
        $ opn
        $ hvbox 2 (keyword $ Option.call ~f:pro $ psp $ bdy)
        $ cls $ esp $ Option.call ~f:epi
        $ fmt_attributes c ~key:"@@" atrs )
  | Psig_modtype mtd -> fmt_module_type_declaration c ctx mtd
  | Psig_module md ->
      hvbox 0 (fmt_module_declaration c ctx ~rec_flag:false ~first:true md)
  | Psig_open od -> fmt_open_description c od
  | Psig_recmodule mds ->
      fmt_recmodule c ctx mds fmt_module_declaration (fun x ->
          Mty x.pmd_type )
  | Psig_type (rec_flag, decls) ->
      hvbox 0
        (list_fl decls (fun ~first ~last decl ->
             let pre =
               if first then
                 if Poly.(rec_flag = Recursive) then "type "
                 else "type nonrec "
               else "and "
             and brk : _ format = if not last then "\n" else "" in
             fmt_type_declaration c ~pre ~brk ctx decl
             $ fmt_if (not last) "@ " ))
  | Psig_typext te -> fmt_type_extension c ctx te
  | Psig_value vd -> fmt_value_description c ctx vd
  | Psig_class cl -> fmt_class_types c ctx ~pre:"class" ~sep:":" cl
  | Psig_class_type cl ->
      fmt_class_types c ctx ~pre:"class type" ~sep:"=" cl

and fmt_class_types c ctx ~pre ~sep (cls : class_type class_infos list) =
  list_fl cls (fun ~first ~last:_ cl ->
      let {pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes}
          =
        cl
      in
      update_config_maybe_disabled c pci_loc pci_attributes
      @@ fun c ->
      let doc, atrs = doc_atrs pci_attributes in
      fmt_if (not first) "\n@\n"
      $ Cmts.fmt c.cmts pci_loc @@ fmt_docstring c ~epi:(fmt "@\n") doc
      $ hovbox 2
          ( hvbox 2
              ( str (if first then pre else "and")
              $ fmt_if Poly.(pci_virt = Virtual) "@ virtual"
              $ fmt "@ "
              $ fmt_class_params c ctx ~epi:(fmt "@ ") pci_params
              $ fmt_str_loc c pci_name $ fmt "@ " $ str sep )
          $ fmt "@;"
          $ fmt_class_type c (sub_cty ~ctx pci_expr)
          $ fmt_attributes c ~pre:(fmt "@;") ~key:"@@" atrs ) )

and fmt_class_exprs c ctx (cls : class_expr class_infos list) =
  list_fl cls (fun ~first ~last:_ cl ->
      let {pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes}
          =
        cl
      in
      update_config_maybe_disabled c pci_loc pci_attributes
      @@ fun c ->
      let xargs, xbody =
        match pci_expr.pcl_attributes with
        | [] ->
            sugar_cl_fun c ~will_keep_first_ast_node:false
              (sub_cl ~ctx pci_expr)
        | _ -> ([], sub_cl ~ctx pci_expr)
      in
      let ty, e =
        match xbody.ast with
        | {pcl_desc= Pcl_constraint (e, t)} -> (Some t, sub_cl ~ctx e)
        | _ -> (None, xbody)
      in
      let doc, atrs = doc_atrs pci_attributes in
      fmt_if (not first) "\n@\n"
      $ Cmts.fmt c.cmts pci_loc @@ fmt_docstring c ~epi:(fmt "@\n") doc
      $ hovbox 2
          ( hovbox 2
              ( str (if first then "class" else "and")
              $ fmt_if Poly.(pci_virt = Virtual) "@ virtual"
              $ fmt "@ "
              $ fmt_class_params c ctx ~epi:(fmt "@ ") pci_params
              $ fmt_str_loc c pci_name
              $ ( fmt_fun_args c ~pro:(fmt "@;") xargs
                $ opt ty (fun t ->
                      fmt " :@ " $ fmt_class_type c (sub_cty ~ctx t) )
                $ fmt "@ =" ) )
          $ fmt "@;" $ fmt_class_expr c e )
      $ fmt_attributes c ~pre:(fmt "@;") ~key:"@@" atrs )

and fmt_module c ?epi keyword name xargs xbody colon xmty attributes =
  let doc, atrs = doc_atrs attributes in
  let arg_blks =
    List.map xargs ~f:(fun (name, xarg) ->
        (name, Option.map ~f:(fmt_module_type c) xarg) )
  in
  let { opn= opn_t
      ; pro= pro_t
      ; psp= psp_t
      ; bdy= bdy_t
      ; cls= cls_t
      ; esp= esp_t
      ; epi= epi_t } =
    Option.value_map xmty ~default:empty ~f:(fun xmty ->
        let blk = fmt_module_type c xmty in
        { blk with
          pro=
            Some
              ( fmt_or colon " :" " ="
              $ fmt_if (Option.is_some blk.pro) " "
              $ Option.call ~f:blk.pro )
        ; psp= fmt_if (Option.is_none blk.pro) "@;<1 2>" $ blk.psp } )
  in
  let { opn= opn_b
      ; pro= pro_b
      ; psp= psp_b
      ; bdy= bdy_b
      ; cls= cls_b
      ; esp= esp_b
      ; epi= epi_b } =
    Option.value_map xbody ~default:empty ~f:(fmt_module_expr c)
  in
  hvbox 0
    ( fmt_docstring c ~epi:(fmt "@\n") doc
    $ opn_b
    $ (if Option.is_some epi_t then open_hovbox else open_hvbox) 0
    $ opn_t
    $ fmt_if_k (Option.is_some pro_t) (open_hvbox 0)
    $ ( match arg_blks with
      | (_, Some {opn; pro= Some _}) :: _ -> opn $ open_hvbox 0
      | _ -> fmt "" )
    $ hvbox 4
        ( keyword $ fmt " " $ fmt_str_loc c name
        $ list_pn arg_blks (fun ?prev:_ (name, arg_mtyp) ?next ->
              ( match arg_mtyp with
              | Some {pro= None} -> fmt "@ @[<hv 2>("
              | _ -> fmt "@ (" )
              $ fmt_str_loc c name
              $ opt arg_mtyp (fun {pro; psp; bdy; cls; esp; epi} ->
                    fmt " : " $ Option.call ~f:pro
                    $ fmt_if_k (Option.is_some pro) close_box
                    $ psp $ bdy
                    $ fmt_if_k (Option.is_some pro) cls
                    $ esp
                    $ ( match next with
                      | Some (_, Some {opn; pro= Some _}) ->
                          opn $ open_hvbox 0
                      | _ -> fmt "" )
                    $ Option.call ~f:epi )
              $
              match arg_mtyp with
              | Some {pro= None} -> fmt ")@]"
              | _ -> fmt ")" ) )
    $ Option.call ~f:pro_t
    $ fmt_if_k (Option.is_some pro_t) close_box
    $ psp_t $ bdy_t $ cls_t $ esp_t $ Option.call ~f:epi_t
    $ fmt_if (Option.is_some xbody) " ="
    $ fmt_if (Option.is_some pro_b) "@ "
    $ Option.call ~f:pro_b $ close_box $ psp_b
    $ fmt_if (Option.is_none pro_b && Option.is_some xbody) "@ "
    $ bdy_b $ cls_b $ esp_b $ Option.call ~f:epi_b
    $ fmt_attributes c ~pre:(fmt "@ ") ~key:"@@" atrs
    $ fmt_if_k (Option.is_some epi) (fmt_or (Option.is_some epi_b) " " "@ ")
    $ Option.call ~f:epi )

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
    else sugar_functor_type c ~for_functor_kw:false (sub_mty ~ctx pmd_type)
  in
  let colon =
    match xmty.ast.pmty_desc with Pmty_alias _ -> false | _ -> true
  in
  Cmts.fmt c.cmts pmd_loc
    (fmt_module c keyword pmd_name xargs None colon (Some xmty)
       pmd_attributes)

and fmt_module_type_declaration c ctx pmtd =
  let {pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc} = pmtd in
  update_config_maybe_disabled c pmtd_loc pmtd_attributes
  @@ fun c ->
  fmt_module c (fmt "module type") pmtd_name [] None false
    (Option.map pmtd_type ~f:(sub_mty ~ctx))
    pmtd_attributes

and fmt_open_description c
    {popen_lid; popen_override; popen_attributes; popen_loc} =
  update_config_maybe_disabled c popen_loc popen_attributes
  @@ fun c ->
  let doc, atrs = doc_atrs popen_attributes in
  fmt_docstring c ~epi:(fmt "@,") doc
  $ fmt "open"
  $ fmt_if Poly.(popen_override = Override) "!"
  $ fmt " "
  $ fmt_longident_loc c popen_lid
  $ fmt_attributes c ~pre:(fmt " ") ~key:"@@" atrs

and fmt_with_constraint c ctx = function
  | Pwith_type (ident, td) ->
      fmt " type "
      $ fmt_type_declaration c ctx ~fmt_name:(fmt_longident_loc c ident) td
  | Pwith_module (m1, m2) ->
      fmt " module " $ fmt_longident_loc c m1 $ fmt " = "
      $ fmt_longident_loc c m2
  | Pwith_typesubst (lid, td) ->
      fmt " type "
      $ fmt_type_declaration c ~eq:":=" ctx
          ~fmt_name:(fmt_longident_loc c lid) td
  | Pwith_modsubst (m1, m2) ->
      fmt " module " $ fmt_longident_loc c m1 $ fmt " := "
      $ fmt_longident_loc c m2

and maybe_generative c ~ctx m =
  match m with
  | {pmod_desc= Pmod_structure []; pmod_attributes= []} -> empty
  | _ -> fmt_module_expr c (sub_mod ~ctx m)

and fmt_module_expr c ({ast= m} as xmod) =
  let ctx = Mod m in
  let {pmod_desc; pmod_loc; pmod_attributes} = m in
  update_config_maybe_disabled_block c pmod_loc pmod_attributes
  @@ fun c ->
  let parens = parenze_mod xmod in
  match pmod_desc with
  | Pmod_apply (({pmod_desc= Pmod_ident _} as me_f), me_a) ->
      let doc, atrs = doc_atrs pmod_attributes in
      let { opn= opn_f
          ; pro= pro_f
          ; psp= psp_f
          ; bdy= bdy_f
          ; cls= cls_f
          ; esp= esp_f
          ; epi= epi_f } =
        fmt_module_expr c (sub_mod ~ctx me_f)
      in
      let ( {opn= opn_a; pro= pro_a; bdy= bdy_a; cls= cls_a; epi= epi_a} as
          blk_a ) =
        maybe_generative c ~ctx me_a
      in
      let fmt_rator =
        fmt_docstring c ~epi:(fmt "@,") doc
        $ opn_f $ psp_f $ Option.call ~f:pro_f $ bdy_f $ cls_f $ esp_f
        $ Option.call ~f:epi_f $ fmt "@ " $ fmt "("
      in
      if Option.is_some pro_a then
        { blk_a with
          opn= opn_a
        ; pro=
            Some
              ( Cmts.fmt_before c.cmts pmod_loc
              $ open_hvbox 2 $ fmt_rator $ close_box $ Option.call ~f:pro_a
              )
        ; cls= cls_a
        ; epi=
            Some
              ( Option.call ~f:epi_a $ fmt ")"
              $ fmt_attributes c ~pre:(fmt " ") ~key:"@" atrs
              $ Cmts.fmt_after c.cmts pmod_loc ) }
      else
        { blk_a with
          opn= open_hvbox 2 $ opn_a
        ; bdy=
            Cmts.fmt_before c.cmts pmod_loc
            $ open_hvbox 2 $ fmt_rator $ bdy_a
        ; cls= close_box $ cls_a $ close_box
        ; epi=
            Some
              ( Option.call ~f:epi_a $ fmt ")"
              $ fmt_attributes c ~pre:(fmt " ") ~key:"@" atrs
              $ Cmts.fmt_after c.cmts pmod_loc ) }
  | Pmod_apply (me_f, me_a) ->
      let doc, atrs = doc_atrs pmod_attributes in
      let { opn= opn_f
          ; pro= pro_f
          ; psp= psp_f
          ; bdy= bdy_f
          ; cls= cls_f
          ; esp= esp_f
          ; epi= epi_f } =
        fmt_module_expr c (sub_mod ~ctx me_f)
      in
      let { opn= opn_a
          ; pro= pro_a
          ; psp= psp_a
          ; bdy= bdy_a
          ; cls= cls_a
          ; esp= esp_a
          ; epi= epi_a } =
        maybe_generative c ~ctx me_a
      in
      let has_epi =
        Cmts.has_after c.cmts pmod_loc || not (List.is_empty atrs)
      in
      { empty with
        opn= opn_a $ opn_f $ open_hvbox 2
      ; bdy=
          hvbox 2
            ( Cmts.fmt_before c.cmts pmod_loc
            $ fmt_docstring c ~epi:(fmt "@,") doc
            $ wrap_if parens "(" ")"
                (Option.call ~f:pro_f $ psp_f $ bdy_f $ esp_f)
            $ Option.call ~f:epi_f $ fmt "@ " $ fmt "("
            $ Option.call ~f:pro_a $ psp_a $ bdy_a $ esp_a
            $ Option.call ~f:epi_a $ fmt ")" )
      ; cls= close_box $ cls_f $ cls_a
      ; epi=
          Option.some_if has_epi
            ( Cmts.fmt_after c.cmts pmod_loc
            $ fmt_attributes c ~pre:(fmt " ") ~key:"@" atrs ) }
  | Pmod_constraint (me, mt) ->
      let doc, atrs = doc_atrs pmod_attributes in
      let { opn= opn_e
          ; pro= pro_e
          ; psp= psp_e
          ; bdy= bdy_e
          ; cls= cls_e
          ; esp= esp_e
          ; epi= epi_e } =
        fmt_module_expr c (sub_mod ~ctx me)
      in
      let { opn= opn_t
          ; pro= pro_t
          ; psp= psp_t
          ; bdy= bdy_t
          ; cls= cls_t
          ; esp= esp_t
          ; epi= epi_t } =
        fmt_module_type c (sub_mty ~ctx mt)
      in
      let has_epi =
        Cmts.has_after c.cmts pmod_loc || not (List.is_empty atrs)
      in
      { opn= opn_t $ opn_e $ open_hvbox 2
      ; pro=
          Some
            ( Cmts.fmt_before c.cmts pmod_loc
            $ fmt_docstring c ~epi:(fmt "@,") doc
            $ fmt "(" )
      ; psp= fmt "@,"
      ; bdy=
          hvbox 2
            ( Option.call ~f:pro_e $ psp_e $ bdy_e $ esp_e
            $ Option.call ~f:epi_e $ fmt " :@ " $ Option.call ~f:pro_t
            $ psp_t $ bdy_t $ esp_t $ Option.call ~f:epi_t )
          $ fmt_or_k c.conf.indicate_multiline_delimiters
              (fits_breaks ")" " )") (fmt ")")
      ; cls= close_box $ cls_e $ cls_t
      ; esp= fmt ""
      ; epi=
          Option.some_if has_epi
            ( Cmts.fmt_after c.cmts pmod_loc
            $ fmt_attributes c ~pre:(fmt " ") ~key:"@" atrs ) }
  | Pmod_functor _ ->
      let xargs, me = sugar_functor c ~for_functor_kw:true xmod in
      let doc, atrs = doc_atrs pmod_attributes in
      let { opn= opn_e
          ; pro= pro_e
          ; psp= psp_e
          ; bdy= bdy_e
          ; cls= cls_e
          ; esp= esp_e
          ; epi= epi_e } =
        fmt_module_expr c me
      in
      { opn= opn_e
      ; pro= None
      ; psp= fmt ""
      ; bdy=
          Cmts.fmt c.cmts pmod_loc
          @@ ( fmt_docstring c ~epi:(fmt "@,") doc
             $ hvbox 0
                 (wrap_if parens "(" ")"
                    ( fmt "functor"
                    $ fmt_attributes c ~pre:(fmt " ") ~key:"@" atrs
                    $ fmt "@;<1 2>"
                    $ list xargs "@;<1 2>" (fun (name, mt) ->
                          let { opn= opn_t
                              ; pro= pro_t
                              ; psp= psp_t
                              ; bdy= bdy_t
                              ; cls= cls_t
                              ; esp= esp_t
                              ; epi= epi_t } =
                            Option.value_map mt ~default:empty
                              ~f:(fmt_module_type c)
                          in
                          wrap "(" ")"
                            (hovbox 0
                               ( fmt_str_loc c name
                               $ opt mt (fun _ ->
                                     opn_t $ fmt "@ :"
                                     $ Option.call ~f:pro_t $ psp_t
                                     $ fmt "@;<1 2>" $ bdy_t $ esp_t
                                     $ Option.call ~f:epi_t $ cls_t ) )) )
                    $ fmt "@;<1 2>->" $ fmt "@;<1 2>"
                    $ hvbox 0
                        ( Option.call ~f:pro_e $ psp_e $ bdy_e $ esp_e
                        $ Option.call ~f:epi_e ) )) )
      ; cls= cls_e
      ; esp= fmt ""
      ; epi= None }
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
            ( Cmts.fmt_before c.cmts pmod_loc
            $ fmt_docstring c ~epi:(fmt "@,") doc )
      ; bdy= fmt_longident_loc c lid
      ; cls= close_box
      ; epi=
          Option.some_if has_epi
            ( Cmts.fmt_after c.cmts pmod_loc
            $ fmt_attributes c ~pre:(fmt " ") ~key:"@" atrs ) }
  | Pmod_structure sis ->
      let empty =
        List.is_empty sis && not (Cmts.has_within c.cmts pmod_loc)
      in
      let doc, atrs = doc_atrs pmod_attributes in
      let before = Cmts.fmt_before c.cmts pmod_loc in
      let within = Cmts.fmt_within c.cmts ~pro:(fmt "") pmod_loc in
      let after = Cmts.fmt_after c.cmts pmod_loc in
      { opn= open_hvbox 0
      ; pro=
          Some
            ( before
            $ fmt_docstring c ~epi:(fmt "@,") doc
            $ fmt "struct" $ fmt_if empty " " )
      ; psp=
          fmt_if_k (not empty)
            (fmt_or c.conf.break_struct "@;<1000 2>" "@;<1 2>")
      ; bdy= within $ fmt_structure c ctx sis
      ; cls= close_box
      ; esp=
          fmt_if_k (not empty)
            (fmt_or c.conf.break_struct "@;<1000 0>" "@;<1 0>")
      ; epi=
          Some
            ( fmt "end" $ after
            $ fmt_attributes c ~pre:(fmt " ") ~key:"@" atrs ) }
  | Pmod_unpack
      { pexp_desc=
          Pexp_constraint
            (e1, {ptyp_desc= Ptyp_package pty; ptyp_attributes= []})
      ; pexp_attributes= [] } ->
      let doc, atrs = doc_atrs pmod_attributes in
      let has_epi =
        Cmts.has_after c.cmts pmod_loc || not (List.is_empty atrs)
      in
      { empty with
        pro=
          Some
            ( Cmts.fmt_before c.cmts pmod_loc
            $ fmt_docstring c ~epi:(fmt "@,") doc )
      ; bdy=
          Cmts.fmt c.cmts pmod_loc
          @@ hvbox 2
               (wrap_fits_breaks ~space:false c.conf "(" ")"
                  ( fmt "val "
                  $ fmt_expression c (sub_exp ~ctx e1)
                  $ fmt "@;<1 2>: "
                  $ fmt_package_type c ctx pty ))
      ; epi=
          Option.some_if has_epi
            ( Cmts.fmt_after c.cmts pmod_loc
            $ fmt_attributes c ~pre:(fmt " ") ~key:"@" atrs ) }
  | Pmod_unpack e1 ->
      let doc, atrs = doc_atrs pmod_attributes in
      let has_epi =
        Cmts.has_after c.cmts pmod_loc || not (List.is_empty atrs)
      in
      let has_pro = Cmts.has_before c.cmts pmod_loc || Option.is_some doc in
      { empty with
        pro=
          Option.some_if has_pro
            ( Cmts.fmt_before c.cmts pmod_loc
            $ fmt_docstring c ~epi:(fmt "@,") doc )
      ; bdy=
          Cmts.fmt c.cmts pmod_loc
          @@ hvbox 2
               (wrap_fits_breaks ~space:false c.conf "(" ")"
                  (fmt "val " $ fmt_expression c (sub_exp ~ctx e1)))
      ; epi=
          Option.some_if has_epi
            ( Cmts.fmt_after c.cmts pmod_loc
            $ fmt_attributes c ~pre:(fmt " ") ~key:"@" atrs ) }
  | Pmod_extension x1 ->
      let doc, atrs = doc_atrs pmod_attributes in
      let has_pro = Cmts.has_before c.cmts pmod_loc || Option.is_some doc in
      let has_epi =
        Cmts.has_after c.cmts pmod_loc || not (List.is_empty atrs)
      in
      { empty with
        pro=
          Option.some_if has_pro
            ( Cmts.fmt_before c.cmts pmod_loc
            $ fmt_docstring c ~epi:(fmt "@,") doc )
      ; bdy= Cmts.fmt c.cmts pmod_loc @@ fmt_extension c ctx "%" x1
      ; epi=
          Option.some_if has_epi
            ( Cmts.fmt_after c.cmts pmod_loc
            $ fmt_attributes c ~pre:(fmt " ") ~key:"@" atrs ) }

and fmt_use_file c ctx itms =
  list itms "\n@\n" (fun item -> fmt_toplevel_phrase c ctx item)

and fmt_toplevel_phrase c ctx = function
  | Ptop_def structure -> fmt_structure c ctx structure
  | Ptop_dir (dir, directive_argument) -> (
      fmt ";;@\n" $ str "#" $ str dir
      $
      match directive_argument with
      | Pdir_none -> fmt ""
      | Pdir_string s -> fmt " " $ str (Printf.sprintf "%S" s)
      | Pdir_int (lit, Some m) ->
          fmt " " $ str (Printf.sprintf "%s%c" lit m)
      | Pdir_int (lit, None) -> fmt " " $ str lit
      | Pdir_ident longident -> fmt " " $ fmt_longident longident
      | Pdir_bool bool -> fmt " " $ str (Bool.to_string bool) )

and fmt_structure c ctx itms =
  let _, itms =
    List.fold_map itms ~init:c ~f:(fun c i ->
        let c =
          match i.pstr_desc with
          | Pstr_attribute atr -> update_config c [atr]
          | _ -> c
        in
        (c, (i, c)) )
  in
  let grps =
    List.group itms ~break:(fun (itmI, cI) (itmJ, cJ) ->
        Ast.break_between c.cmts (Str itmI, cI.conf) (Str itmJ, cJ.conf) )
  in
  let break_struct = c.conf.break_struct || Poly.(ctx = Top) in
  let fmt_grp ~last:last_grp itms =
    list_fl itms (fun ~first ~last (itm, c) ->
        fmt_if_k (not first) (fmt_or break_struct "@\n" "@ ")
        $ maybe_disabled c itm.pstr_loc []
          @@ fun c ->
          fmt_structure_item c ~last:(last && last_grp) (sub_str ~ctx itm)
    )
  in
  hvbox 0
    (list_fl grps (fun ~first ~last grp ->
         fmt_if (break_struct && not first) "\n@\n"
         $ fmt_if ((not break_struct) && not first) "@;<1000 0>"
         $ fmt_grp ~last grp
         $ fits_breaks_if ((not break_struct) && not last) "" "\n" ))

and fmt_structure_item c ~last:last_item ?ext {ctx; ast= si} =
  protect (Str si)
  @@
  let skip_double_semi =
    match ctx with Pld (PStr [_]) -> true | _ -> false
  in
  let ctx = Str si in
  let fmt_cmts_before =
    Cmts.fmt_before c.cmts ~epi:(fmt "\n@\n") ~eol:(fmt "\n@\n")
      ~adj:(fmt "@\n") si.pstr_loc
  and fmt_cmts_after =
    Cmts.fmt_after c.cmts ~pro:(fmt "\n@\n") si.pstr_loc
  in
  wrap_k fmt_cmts_before fmt_cmts_after
  @@
  match si.pstr_desc with
  | Pstr_attribute atr ->
      let doc, atrs = doc_atrs [atr] in
      fmt_docstring c ~standalone:true ~epi:(fmt "") doc
      $ fmt_attributes c ~key:"@@@" atrs
  | Pstr_eval (exp, atrs) ->
      let doc, atrs = doc_atrs atrs in
      fmt_if (not skip_double_semi) ";;@\n"
      $ fmt_docstring c doc
      $ cbox 0 (fmt_expression c (sub_exp ~ctx exp))
      $ fmt_attributes c ~pre:(fmt " ") ~key:"@@" atrs
  | Pstr_exception extn_constr ->
      hvbox 2
        (fmt_exception ~pre:(fmt "exception@ ") c (fmt ": ") ctx extn_constr)
  | Pstr_include {pincl_mod; pincl_attributes; pincl_loc} ->
      update_config_maybe_disabled c pincl_loc pincl_attributes
      @@ fun c ->
      let doc, atrs = doc_atrs pincl_attributes in
      let {opn; pro; psp; bdy; cls; esp; epi} =
        fmt_module_expr c (sub_mod ~ctx pincl_mod)
      in
      opn
      $ fmt_docstring c ~epi:(fmt "@\n") doc
      $ ( hvbox 2 (fmt "include " $ Option.call ~f:pro)
        $ psp $ bdy $ cls $ esp $ Option.call ~f:epi
        $ fmt_attributes c ~pre:(fmt " ") ~key:"@@" atrs )
  | Pstr_module binding ->
      fmt_module_binding c ctx ~rec_flag:false ~first:true binding
  | Pstr_open open_descr -> fmt_open_description c open_descr
  | Pstr_primitive vd -> fmt_value_description c ctx vd
  | Pstr_recmodule bindings ->
      fmt_recmodule c ctx bindings
        (fun c ctx ~rec_flag ~first b ->
          (* To ignore the ?epi parameter *)
          fmt_module_binding c ctx ~rec_flag ~first b )
        (fun x -> Mod x.pmb_expr)
  | Pstr_type (rec_flag, decls) ->
      vbox 0
        (list_fl decls (fun ~first ~last decl ->
             let pre =
               if first then
                 if Poly.(rec_flag = Recursive) then "type "
                 else "type nonrec "
               else "and "
             and brk : _ format = if not last then "\n" else "" in
             fmt_type_declaration c ~pre ~brk ctx decl
             $ fmt_if (not last) "@ " ))
  | Pstr_typext te -> fmt_type_extension c ctx te
  | Pstr_value (rec_flag, bindings) ->
      let _, bindings =
        List.fold_map bindings ~init:c ~f:(fun c b ->
            let c = update_config ~quiet:true c b.pvb_attributes in
            (c, (b, c)) )
      in
      let grps =
        List.group bindings ~break:(fun (itmI, cI) (itmJ, cJ) ->
            (not (List.is_empty itmI.pvb_attributes))
            || (not (List.is_empty itmJ.pvb_attributes))
            || Ast.break_between c.cmts
                 (Exp itmI.pvb_expr, cI.conf)
                 (Exp itmJ.pvb_expr, cJ.conf) )
      in
      let fmt_grp ~first:first_grp ~last:last_grp bindings =
        list_fl bindings (fun ~first ~last (binding, c) ->
            fmt_if (not first) "@\n"
            $ fmt_value_binding c ~rec_flag ~first:(first && first_grp)
                ?ext:(if first && first_grp then ext else None)
                ctx binding
                ?epi:
                  ( match c.conf.let_binding_spacing with
                  | `Compact -> None
                  | `Sparse ->
                      Some
                        (fits_breaks
                           ~force_fit_if:(last && last_grp && last_item)
                           "" "\n")
                  | `Double_semicolon ->
                      Option.some_if (last && last_grp)
                        (fits_breaks "" "@;<1000 -2>;;") ) )
      in
      hvbox 0
        (list_fl grps (fun ~first ~last grp ->
             fmt_grp ~first ~last grp $ fmt_if (not last) "\n@\n" ))
  | Pstr_modtype mtd -> fmt_module_type_declaration c ctx mtd
  | Pstr_extension (ext, atrs) ->
      let doc, atrs = doc_atrs atrs in
      fmt_docstring c doc
      $ fmt_extension c ctx "%%" ext
      $ fmt_attributes c ~pre:(fmt " ") ~key:"@@" atrs
  | Pstr_class_type cl ->
      fmt_class_types c ctx ~pre:"class type" ~sep:"=" cl
  | Pstr_class cls -> fmt_class_exprs c ctx cls

and fmt_value_binding c ~rec_flag ~first ?ext ?in_ ?epi ctx binding =
  let {pvb_pat; pvb_expr; pvb_attributes; pvb_loc} = binding in
  update_config_maybe_disabled c pvb_loc pvb_attributes
  @@ fun c ->
  let doc1, atrs = doc_atrs pvb_attributes in
  let doc2, atrs = doc_atrs atrs in
  let xpat, xargs, fmt_cstr, xbody =
    let ({ast= pat} as xpat) =
      match (pvb_pat.ppat_desc, pvb_expr.pexp_desc) with
      (* recognize and undo the pattern of code introduced by
         ocaml/ocaml@fd0dc6a0fbf73323c37a73ea7e8ffc150059d6ff to fix
         https://caml.inria.fr/mantis/view.php?id=7344 *)
      | ( Ppat_constraint
            ( ({ppat_desc= Ppat_var _} as pat)
            , {ptyp_desc= Ptyp_poly ([], typ1)} )
        , Pexp_constraint (_, typ2) )
        when Poly.equal typ1 typ2 ->
          Cmts.relocate c.cmts ~src:pvb_pat.ppat_loc ~before:pat.ppat_loc
            ~after:pat.ppat_loc ;
          sub_pat ~ctx:(Pat pvb_pat) pat
      | _ -> sub_pat ~ctx pvb_pat
    in
    let pat_is_extension {ppat_desc} =
      match ppat_desc with Ppat_extension _ -> true | _ -> false
    in
    let ({ast= body} as xbody) = sub_exp ~ctx pvb_expr in
    if
      (not (List.is_empty xbody.ast.pexp_attributes))
      || pat_is_extension pat
    then (xpat, [], None, xbody)
    else
      let sugar_polynewtype pat body =
        let ctx = Pat pat in
        match pat.ppat_desc with
        | Ppat_constraint (pat2, {ptyp_desc= Ptyp_poly (pvars, _typ)}) ->
            let rec sugar_polynewtype_ xpat pvars0 pvars body =
              let ctx = Exp body in
              match (pvars, body.pexp_desc) with
              | [], Pexp_constraint (exp, typ) ->
                  Some (xpat, pvars0, sub_typ ~ctx typ, sub_exp ~ctx exp)
              | ( {txt= pvar; loc= loc1} :: pvars
                , Pexp_newtype ({txt= nvar; loc= loc2}, exp) )
                when String.equal pvar nvar ->
                  Cmts.relocate c.cmts ~src:loc2 ~before:loc1 ~after:loc1 ;
                  sugar_polynewtype_ xpat pvars0 pvars exp
              | _ -> None
            in
            Cmts.relocate c.cmts ~src:pat.ppat_loc ~before:pat2.ppat_loc
              ~after:pat2.ppat_loc ;
            sugar_polynewtype_ (sub_pat ~ctx pat2) pvars pvars body
        | _ -> None
      in
      match sugar_polynewtype pat body with
      (* format
       *    let f: 'r 's. 'r 's t = fun (type r) -> fun (type s) -> (e : r s t)
       * as let f: type r s. r s t = e *)
      | Some (xpat, pvars, xtyp, xbody) ->
          let fmt_cstr =
            fmt "@ : type "
            $ list pvars " " (fun name -> fmt_str_loc c name)
            $ fmt ".@ " $ fmt_core_type c xtyp
          in
          (xpat, [], Some fmt_cstr, xbody)
      | None ->
          let xpat =
            match xpat.ast.ppat_desc with
            | Ppat_constraint (p, {ptyp_desc= Ptyp_poly ([], _)}) ->
                sub_pat ~ctx:xpat.ctx p
            | _ -> xpat
          in
          let xargs, ({ast= body} as xbody) =
            match pat with
            | {ppat_desc= Ppat_var _; ppat_attributes= []; _} ->
                sugar_fun c ~will_keep_first_ast_node:false xbody
            | _ -> ([], xbody)
          in
          let fmt_cstr, xbody =
            let ctx = Exp body in
            match (body.pexp_desc, pat.ppat_desc) with
            | ( Pexp_constraint
                  ({pexp_desc= Pexp_pack _}, {ptyp_desc= Ptyp_package _})
              , _ )
             |Pexp_constraint _, Ppat_constraint _ ->
                (None, xbody)
            | Pexp_constraint (exp, typ), _ ->
                Cmts.relocate c.cmts ~src:body.pexp_loc ~before:exp.pexp_loc
                  ~after:exp.pexp_loc ;
                ( Some
                    (fmt "@ " $ fmt_core_type c ~pro:":" (sub_typ ~ctx typ))
                , sub_exp ~ctx exp )
            | _ -> (None, xbody)
          in
          (xpat, xargs, fmt_cstr, xbody)
  in
  let indent =
    match xbody.ast with {pexp_desc= Pexp_fun _} -> 1 | _ -> 2
  in
  fmt_docstring c ~epi:(fmt "@\n") doc1
  $ Cmts.fmt_before c.cmts pvb_loc
  $ hvbox indent
      ( open_hovbox 2
      $ ( hovbox 4
            ( fmt_or first "let" "and"
            $ fmt_extension_suffix c ext
            $ fmt_attributes c ~key:"@" atrs
            $ fmt_if (first && Poly.(rec_flag = Recursive)) " rec"
            $ fmt " " $ fmt_pattern c xpat
            $ fmt_if (not (List.is_empty xargs)) "@ "
            $ hvbox_if (not c.conf.wrap_fun_args) 0 (fmt_fun_args c xargs)
            $ Option.call ~f:fmt_cstr )
        $ fmt "@;<1 2>=" )
      $ fmt_body c ?ext xbody
      $ Cmts.fmt_after c.cmts pvb_loc
      $ (match in_ with Some in_ -> in_ indent | None -> Fn.const ())
      $ Option.call ~f:epi )
  $ fmt_docstring c ~pro:(fmt "@ ") doc2

and fmt_module_binding c ?epi ~rec_flag ~first ctx pmb =
  let {pmb_name; pmb_expr; pmb_attributes; pmb_loc} = pmb in
  update_config_maybe_disabled c pmb_loc pmb_attributes
  @@ fun c ->
  let keyword =
    if first then if rec_flag then str "module rec" else str "module"
    else str "and"
  in
  let xargs, xbody =
    sugar_functor c ~for_functor_kw:false (sub_mod ~ctx pmb_expr)
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
  Cmts.fmt c.cmts pmb_loc
    (fmt_module c ?epi keyword pmb_name xargs (Some xbody) true xmty
       pmb_attributes)

(** Entry points *)

let fmt_signature s cmts c itms =
  let c = {source= s; cmts; conf= c} in
  Ast.init c.conf ;
  match itms with
  | [] -> Cmts.fmt_after c.cmts Location.none
  | l -> fmt_signature c Top l

let fmt_structure s cmts c itms =
  let c = {source= s; cmts; conf= c} in
  Ast.init c.conf ;
  match itms with
  | [] -> Cmts.fmt_after c.cmts Location.none
  | l -> fmt_structure c Top l

let fmt_use_file s cmts c itms =
  let c = {source= s; cmts; conf= c} in
  Ast.init c.conf ;
  match itms with
  | [] -> Cmts.fmt_after c.cmts Location.none
  | l -> fmt_use_file c Top l
