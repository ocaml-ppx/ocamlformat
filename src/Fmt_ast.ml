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

(* Debug: catch and report failures at nearest enclosing Ast.t *)

let protect =
  let first = ref true in
  fun ast pp fs ->
    try pp fs with exc ->
      if !first then (
        let bt = Caml.Printexc.get_backtrace () in
        Format.pp_print_flush fs () ;
        Caml.Format.eprintf "@\nFAIL@\n%a@\n%s@.%!" Ast.dump ast bt ;
        first := false ) ;
      raise exc

let rec sugar_arrow_typ c ({ast= typ} as xtyp) =
  let ctx = Typ typ in
  let {ptyp_desc; ptyp_loc} = typ in
  match ptyp_desc with
  | Ptyp_arrow (l, t1, t2) ->
      Cmts.relocate c.cmts ~src:ptyp_loc ~before:t1.ptyp_loc
        ~after:t2.ptyp_loc ;
      (l, sub_typ ~ctx t1) :: sugar_arrow_typ c (sub_typ ~ctx t2)
  | _ -> [(Nolabel, xtyp)]

let rec sugar_class_arrow_typ c ({ast= typ} as xtyp) =
  let ctx = Cty typ in
  let {pcty_desc; pcty_loc} = typ in
  match pcty_desc with
  | Pcty_arrow (l, t1, t2) ->
      Cmts.relocate c.cmts ~src:pcty_loc ~before:t1.ptyp_loc
        ~after:t2.pcty_loc ;
      (l, `core_type (sub_typ ~ctx t1))
      :: sugar_class_arrow_typ c (sub_cty ~ctx t2)
  | _ -> [(Nolabel, `class_type xtyp)]

let rec sugar_or_pat ?(allow_attribute= true) c ({ast= pat} as xpat) =
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

let sugar_fun c pat xexp =
  let rec sugar_fun_ ({ast= exp} as xexp) =
    let ctx = Exp exp in
    let {pexp_desc; pexp_loc} = exp in
    match pexp_desc with
    | Pexp_fun (label, default, pattern, body) ->
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
  in
  match pat with
  | Some {ppat_desc= Ppat_any | Ppat_constraint _ | Ppat_constant _} ->
      ([], xexp)
  | Some {ppat_attributes} when not (List.is_empty ppat_attributes) ->
      ([], xexp)
  | _ -> sugar_fun_ xexp

let sugar_cl_fun c pat xexp =
  let rec sugar_fun_ ({ast= exp} as xexp) =
    let ctx = Cl exp in
    let {pcl_desc; pcl_loc} = exp in
    match pcl_desc with
    | Pcl_fun (label, default, pattern, body) ->
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
  in
  match pat with
  | Some {ppat_desc= Ppat_any | Ppat_constraint _} -> ([], xexp)
  | None | Some {ppat_attributes= []} -> sugar_fun_ xexp
  | _ -> ([], xexp)

let sugar_infix c prec xexp =
  let assoc = Option.value_map prec ~default:Non ~f:assoc_of_prec in
  let rec sugar_infix_ xop ((lbl, {ast= exp}) as xexp) =
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
            Cmts.relocate c.cmts ~src ~before ~after
        | _ -> Cmts.relocate c.cmts ~src ~before:e0.pexp_loc ~after ) ;
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
                Cmts.relocate c.cmts ~src ~before:ast.pexp_loc ~after
            | None -> Cmts.relocate c.cmts ~src ~before:e1.pexp_loc ~after )
          | None ->
            match xop with
            | Some {ast} ->
                Cmts.relocate c.cmts ~src ~before:ast.pexp_loc ~after
            | None -> Cmts.relocate c.cmts ~src ~before:e1.pexp_loc ~after )
        | _ ->
          match xop with
          | Some {ast} ->
              Cmts.relocate c.cmts ~src ~before:ast.pexp_loc ~after
          | None -> Cmts.relocate c.cmts ~src ~before:e1.pexp_loc ~after ) ;
        (xop, [(l1, sub_exp ~ctx e1)]) :: op_args2
    | _ -> [(xop, [xexp])]
  in
  sugar_infix_ None (Nolabel, xexp)

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
  let rec sugar_sequence_ ?(allow_attribute= true) ({ast= exp} as xexp) =
    let ctx = Exp exp in
    let {pexp_desc; pexp_loc} = exp in
    match pexp_desc with
    | Pexp_sequence (e1, e2) ->
        Cmts.relocate c.cmts ~src:pexp_loc ~before:e1.pexp_loc
          ~after:e2.pexp_loc ;
        if
          (not allow_attribute) && (not (List.is_empty exp.pexp_attributes))
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
      || (not (is_simple c.conf width xexp2)) )

(* The sugar is different when used with the [functor] keyword. The syntax
   M(A : A)(B : B) cannot handle [_] as module name. *)
let rec sugar_functor_type c ~for_functor_kw ({ast= mty} as xmty) =
  let ctx = Mty mty in
  match mty with
  | {pmty_desc= Pmty_functor (arg, arg_mty, body); pmty_loc; pmty_attributes}
    when for_functor_kw
         || List.is_empty pmty_attributes
            && (not (String.equal arg.txt "_")) ->
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
         || List.is_empty pmod_attributes
            && (not (String.equal arg.txt "_")) ->
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

(* In several places, naked newlines (i.e. not "@\n") are used to avoid
   trailing space in open lines. *)
(* In several places, a break such as "@;<1000 0>" is used to force the
   enclosing box to break across multiple lines. *)

let rec fmt_longident (li: Longident.t) =
  match li with
  | Lident "~+" -> str "+"
  | Lident id -> str id
  | Ldot (li, id) ->
      cbox 0
        ( fmt_longident li $ fmt ".@,"
        $ wrap_if (is_symbol_id id) "( " " )" (str id) )
  | Lapply (li1, li2) ->
      cbox 0 (fmt_longident li1 $ wrap "(" ")" (fmt_longident li2))

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
  | Pconst_string (s, None) ->
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
          $ list_pn lines (fun ?prev curr ?next ->
                let drop = function ' ' | '\t' -> true | _ -> false in
                let line =
                  if Option.is_none prev then curr
                  else String.lstrip ~drop curr
                in
                fmt_line line
                $ opt next (fun next ->
                      let spc =
                        match
                          String.lfindi next ~f:(fun _ c -> not (drop c))
                        with
                        | Some 0 -> ""
                        | Some i ->
                            escape_string c (String.sub next ~pos:0 ~len:i)
                        | None -> escape_string c next
                      in
                      fmt "\\n"
                      $ fmt_if_k
                          (not (String.is_empty next))
                          (str spc $ pre_break 0 "\\" 0) ) )
          $ str "\"" $ Option.call ~f:epi )
      in
      let s =
        match (c.conf.break_string_literals, c.conf.escape_strings) with
        | `Never, `Preserve -> Source.string_literal c.source `Preserve loc
        | (`Newlines | `Wrap), `Preserve ->
            Source.string_literal c.source `Normalize_nl loc
        | _ -> s
      in
      match c.conf.break_string_literals with
      | `Newlines | `Wrap -> fmt_lines (String.split ~on:'\n' s)
      | `Never -> str "\"" $ fmt_line s $ str "\""

let fmt_variance = function
  | Covariant -> fmt "+"
  | Contravariant -> fmt "-"
  | Invariant -> fmt ""

let doc_atrs atrs =
  let doc, rev_atrs =
    List.fold atrs ~init:(None, []) ~f:(fun (doc, rev_atrs) atr ->
        match (doc, atr) with
        | ( None
          , ( {txt= ("ocaml.doc" | "ocaml.text") as txt}
            , PStr
                [ { pstr_desc=
                      Pstr_eval
                        ( { pexp_desc=
                              Pexp_constant (Pconst_string (doc, None))
                          ; pexp_loc= loc
                          ; pexp_attributes= [] }
                        , [] ) } ] ) ) ->
            (Some ({txt= doc; loc}, String.equal "ocaml.text" txt), rev_atrs)
        | _ -> (doc, atr :: rev_atrs) )
  in
  (doc, List.rev rev_atrs)

let fmt_docstring c ?pro ?epi doc =
  opt doc (fun (({txt; loc} as doc), floating) ->
      let epi =
        match epi with
        | Some _ -> epi
        | None when floating -> Some (fmt "@,")
        | None -> None
      in
      fmt_if_k
        (not (Cmts.doc_is_dup c.cmts doc))
        ( Cmts.fmt c.cmts loc
        @@ vbox_if (Option.is_none pro) 0
             ( Option.call ~f:pro $ fmt "(**"
             $ (if c.conf.wrap_comments then fill_text else str) txt
             $ fmt "*)" $ Option.call ~f:epi ) ) )

let fmt_extension_suffix c ext =
  opt ext (fun {txt; loc} -> str "%" $ Cmts.fmt c.cmts loc (str txt))

let field_alias (li1: Longident.t) (li2: Longident.t) =
  match (li1, li2) with
  | Ldot (_, x), Lident y -> String.equal x y
  | _ -> Poly.equal li1 li2

let rec fmt_attribute c pre = function
  | ( {txt= ("ocaml.doc" | "ocaml.text") as txt}
    , PStr
        [ { pstr_desc=
              Pstr_eval
                ( { pexp_desc= Pexp_constant (Pconst_string (doc, None))
                  ; pexp_attributes= [] }
                , [] ) } ] ) ->
      fmt_or (String.equal txt "ocaml.text") "@ " " "
      $ fmt "(**" $ str doc $ fmt "*)"
  | {txt; loc}, pld ->
      let protect_token =
        match pld with PTyp t -> exposed_right_typ t | _ -> false
      in
      Cmts.fmt c.cmts loc
      @@ hvbox 2
           (wrap "[" "]"
              ( str pre $ str txt
              $ fmt_payload c (Pld pld) pld
              $ fmt_if protect_token " " ))

and fmt_extension c ctx key (({txt} as ext), pld) =
  match (pld, ctx) with
  | PStr [({pstr_desc= Pstr_value _; _} as si)], (Pld _ | Str _ | Top) ->
      fmt_structure_item c ~sep:"" ~last:true ~ext (sub_str ~ctx si)
  | _ ->
      let protect_token =
        match pld with PTyp t -> exposed_right_typ t | _ -> false
      in
      wrap "[" "]"
        ( str key $ str txt $ fmt_payload c ctx pld
        $ fmt_if protect_token " " )

and fmt_attributes c ?(pre= fmt "") ?(suf= fmt "") ?(box= true) ~key attrs =
  let num = List.length attrs in
  if num = 0 then fmt ""
  else
    let split = num > 1 in
    let box = split && box in
    pre
    $ hvbox_if box 0
        (list_fl attrs (fun ~first ~last atr ->
             fmt_or_k first (open_hvbox 0) (fmt "@;<1 0>")
             $ fmt_attribute c key atr
             $ fmt_if_k last (close_box $ suf) ))

and fmt_payload c ctx pld =
  protect (Pld pld)
  @@
  match pld with
  | PStr mex ->
      fmt_if (not (List.is_empty mex)) "@ " $ fmt_structure c ctx mex
  | PSig mty -> fmt "@ : " $ fmt_signature c ctx mty
  | PTyp typ -> fmt "@ : " $ fmt_core_type c (sub_typ ~ctx typ)
  | PPat (pat, exp) ->
      fmt "@ ? "
      $ fmt_pattern c (sub_pat ~ctx pat)
      $ opt exp (fun exp ->
            fmt " when " $ fmt_expression c (sub_exp ~ctx exp) )

and fmt_core_type c ?(box= true) ?pro ({ast= typ} as xtyp) =
  protect (Typ typ)
  @@
  let {ptyp_desc; ptyp_attributes; ptyp_loc} = typ in
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
        $ list xt1N "@;<1 0>-> " (fun (lI, xtI) ->
              hvbox 0 (arg_label lI $ fmt_core_type c xtI) ) )
  | Ptyp_constr ({txt; loc}, []) -> Cmts.fmt c.cmts loc @@ fmt_longident txt
  | Ptyp_constr ({txt; loc}, [t1]) ->
      Cmts.fmt c.cmts loc @@ fmt_core_type c (sub_typ ~ctx t1)
      $ fmt "@ " $ fmt_longident txt
  | Ptyp_constr ({txt; loc}, t1N) ->
      Cmts.fmt c.cmts loc
      @@ wrap_fits_breaks "(" ")"
           (list t1N "@,, " (sub_typ ~ctx >> fmt_core_type c))
      $ fmt "@ " $ fmt_longident txt
  | Ptyp_extension ext -> hvbox 2 (fmt_extension c ctx "%" ext)
  | Ptyp_package pty -> hvbox 0 (fmt "module@ " $ fmt_package_type c ctx pty)
  | Ptyp_poly ([], _) ->
      impossible "produced by the parser, handled elsewhere"
  | Ptyp_poly (a1N, t) ->
      hovbox_if box 0
        ( list a1N "@ " (fun {txt} -> fmt "'" $ str txt)
        $ fmt ".@ "
        $ fmt_core_type c ~box:false (sub_typ ~ctx t) )
  | Ptyp_tuple typs ->
      hvbox 0
        (wrap_fits_breaks_if parens "(" ")"
           (list typs "@ * " (sub_typ ~ctx >> fmt_core_type c)))
  | Ptyp_var s -> fmt "'" $ str s
  | Ptyp_variant (rfs, flag, lbls) ->
      let row_fields rfs =
        match rfs with
        | [] -> Cmts.fmt_within c.cmts ~pro:(fmt "") ptyp_loc
        | _ -> list rfs "@ | " (fmt_row_field c ctx)
      in
      let protect_token =
        match List.last rfs with
        | None -> false
        | Some (Rinherit _) -> false
        | Some (Rtag (_, _, _, l)) ->
          match List.last l with
          | None -> false
          | Some x -> exposed_right_typ x
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
                            ( Cmts.fmt c.cmts lab_loc.loc @@ str lab_loc.txt
                            $ fmt ":@ "
                            $ fmt_core_type c (sub_typ ~ctx typ) )
                        $ fmt_docstring c ~pro:(fmt "@;<2 0>") doc
                        $ fmt_attributes c ~pre:(fmt " ") ~key:"@" atrs )
               | Oinherit typ -> fmt_core_type c (sub_typ ~ctx typ) )
           $ fmt_if Poly.(closedness = Open) "@ ; .." ))
  | Ptyp_class ({txt; loc}, []) ->
      Cmts.fmt c.cmts loc @@ str "#" $ fmt_longident txt
  | Ptyp_class ({txt; loc}, [t1]) ->
      Cmts.fmt c.cmts loc @@ fmt_core_type c (sub_typ ~ctx t1)
      $ fmt "@ " $ str "#" $ fmt_longident txt
  | Ptyp_class ({txt; loc}, t1N) ->
      Cmts.fmt c.cmts loc
      @@ wrap_fits_breaks "(" ")"
           (list t1N "@,, " (sub_typ ~ctx >> fmt_core_type c))
      $ fmt "@ " $ str "#" $ fmt_longident txt )
  $ fmt_docstring c ~pro:(fmt "@ ") doc

and fmt_package_type c ctx ({txt}, cnstrs) =
  fmt_longident txt
  $ fmt_if (not (List.is_empty cnstrs)) "@;<1 2>"
  $ hvbox 0
      (list_fl cnstrs (fun ~first ~last:_ ({txt}, typ) ->
           fmt_or first "with type " "@;<1 1>and type "
           $ fmt_longident txt $ fmt " = "
           $ fmt_core_type c (sub_typ ~ctx typ) ))

and fmt_row_field c ctx = function
  | Rtag ({txt; loc}, atrs, const, typs) ->
      let doc, atrs = doc_atrs atrs in
      hvbox 0
        ( Cmts.fmt c.cmts loc @@ (fmt "`" $ str txt)
        $ fmt_if (not (const && List.is_empty typs)) " of "
        $ fmt_if (const && (not (List.is_empty typs))) " & "
        $ list typs "@ & " (sub_typ ~ctx >> fmt_core_type c)
        $ fmt_attributes c ~key:"@" atrs
        $ fmt_docstring c ~pro:(fmt "@;<2 0>") doc )
  | Rinherit typ -> fmt_core_type c (sub_typ ~ctx typ)

and fmt_pattern c ?pro ?parens ({ctx= ctx0; ast= pat} as xpat) =
  protect (Pat pat)
  @@
  let ctx = Pat pat in
  let {ppat_desc; ppat_attributes; ppat_loc} = pat in
  let parens = match parens with Some b -> b | None -> parenze_pat xpat in
  let spc = break_unless_newline 1 0 in
  ( match ppat_desc with
  | Ppat_or _ -> Fn.id
  | Ppat_construct ({txt; loc}, _) when Poly.(txt <> Longident.Lident "::") ->
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
  | Ppat_alias (pat, {txt}) ->
      let paren_pat =
        match pat.ppat_desc with
        | Ppat_or _ | Ppat_tuple _ -> Some true
        | _ -> None
      in
      hovbox 0
        (wrap_fits_breaks_if parens "(" ")"
           ( fmt_pattern c ?parens:paren_pat (sub_pat ~ctx pat)
           $ fmt "@ as@ "
           $ wrap_if (is_symbol_id txt) "( " " )" (str txt) ))
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
      hvbox 0
        (wrap_if_breaks "( " "@ )"
           (wrap_if_fits_and parens "(" ")"
              (list pats "@,, " (sub_pat ~ctx >> fmt_pattern c))))
  | Ppat_construct ({txt; loc}, None) -> (
    match txt with
    | Lident (("()" | "[]") as txt) ->
        let opn = txt.[0] and cls = txt.[1] in
        Cmts.fmt c.cmts loc
          (hvbox 0
             (wrap_k (char opn) (char cls)
                (Cmts.fmt_within c.cmts ~pro:(fmt " ") ~epi:(fmt " ")
                   ppat_loc)))
    | _ -> fmt_longident txt )
  | Ppat_construct
      ( {txt= Lident "::"}
      , Some {ppat_desc= Ppat_tuple pats; ppat_attributes= []} ) -> (
    match sugar_list_pat c pat with
    | Some (loc_xpats, nil_loc) ->
        hvbox 0
          (wrap_fits_breaks "[" "]"
             ( list loc_xpats "@,; " (fun (locs, xpat) ->
                   Cmts.fmt_list c.cmts locs @@ fmt_pattern c xpat )
             $ Cmts.fmt c.cmts ~pro:(fmt " ") ~epi:(fmt "") nil_loc
               @@ fmt "" ))
    | None ->
        hvbox 0
          (wrap_if parens "(" ")"
             (list pats "@ :: " (sub_pat ~ctx >> fmt_pattern c))) )
  | Ppat_construct ({txt}, Some pat) ->
      cbox 2
        (wrap_if parens "(" ")"
           (fmt_longident txt $ fmt "@ " $ fmt_pattern c (sub_pat ~ctx pat)))
  | Ppat_variant (lbl, None) -> fmt "`" $ str lbl
  | Ppat_variant (lbl, Some pat) ->
      cbox 2
        (wrap_if parens "(" ")"
           (fmt "`" $ str lbl $ fmt "@ " $ fmt_pattern c (sub_pat ~ctx pat)))
  | Ppat_record (flds, closed_flag) ->
      let fmt_field ({txt; loc}, pat) =
        let {ppat_desc; ppat_loc} = pat in
        hvbox 0
          ( Cmts.fmt c.cmts loc @@ Cmts.fmt c.cmts ppat_loc
          @@
          match ppat_desc with
          | Ppat_var {txt= txt'}
            when field_alias txt (Longident.parse txt')
                 && List.is_empty pat.ppat_attributes ->
              cbox 2 (fmt_longident txt)
          | Ppat_constraint ({ppat_desc= Ppat_var {txt= txt'; _}}, t)
            when field_alias txt (Longident.parse txt')
                 && List.is_empty pat.ppat_attributes ->
              cbox 2
                ( fmt_longident txt $ fmt " : "
                $ fmt_core_type c (sub_typ ~ctx:(Pat pat) t) )
          | _ ->
              cbox 2
                ( fmt_longident txt $ fmt "=@ "
                $ cbox 0 (fmt_pattern c (sub_pat ~ctx pat)) ) )
      in
      hvbox 0
        (wrap_fits_breaks "{" "}"
           ( list flds "@,; " fmt_field
           $ fmt_if Poly.(closed_flag = Open) "; _" ))
  | Ppat_array [] ->
      hvbox 0 (wrap_fits_breaks "[|" "|]" (Cmts.fmt_within c.cmts ppat_loc))
  | Ppat_array pats ->
      hvbox 0
        (wrap_fits_breaks "[|" "|]"
           (list pats "@;<0 1>; " (sub_pat ~ctx >> fmt_pattern c)))
  | Ppat_or _ ->
      let nested =
        match ctx0 with
        | Pat {ppat_desc= Ppat_or _} -> true
        | Exp {pexp_desc= Pexp_match _ | Pexp_function _} -> true
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
          when not c.conf.sparse ->
            or_newline "| " " |"
        | _ -> break_unless_newline 1 0 $ fmt "| "
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
                 c.conf.sparse
                 || (not (is_simple p1))
                 || (not (is_simple p2)) ))
            (fun ~first:first_grp ~last:_ xpat_grp ->
              list_fl xpat_grp (fun ~first ~last xpat ->
                  let pro =
                    if first_grp && first then pro0 $ open_hovbox (-2)
                    else if first then proI $ open_hovbox (-2)
                    else proI
                  in
                  fmt_pattern c ~pro xpat $ fmt_if_k last close_box ) )
        $ fits_breaks
            (if parens then ")" else "")
            (if nested then "" else "@;<1 2>)") )
  | Ppat_constraint
      ( {ppat_desc= Ppat_unpack {txt}; ppat_attributes= []}
      , ({ptyp_desc= Ptyp_package pty; ptyp_attributes= []} as typ) ) ->
      let ctx = Typ typ in
      wrap_if parens "(" ")"
        ( fmt "module " $ str txt $ fmt "@;<1 2>: "
        $ fmt_package_type c ctx pty )
  | Ppat_constraint (pat, typ) ->
      hvbox 2
        (wrap_if parens "(" ")"
           ( fmt_pattern c (sub_pat ~ctx pat)
           $ ( match ctx0 with
             | Exp {pexp_desc= Pexp_let _} -> fmt "@ : "
             | _ -> fmt ":@ " )
           $ fmt_core_type c (sub_typ ~ctx typ) ))
  | Ppat_type {txt} -> fmt "#" $ fmt_longident txt
  | Ppat_lazy pat ->
      cbox 2
        (wrap_if parens "(" ")"
           (fmt "lazy@ " $ fmt_pattern c (sub_pat ~ctx pat)))
  | Ppat_unpack {txt} ->
      wrap_fits_breaks_if parens "(" ")" (fmt "module@ " $ str txt)
  | Ppat_exception pat ->
      cbox 2
        (wrap_if parens "(" ")"
           (fmt "exception@ " $ fmt_pattern c (sub_pat ~ctx pat)))
  | Ppat_extension ext -> hvbox 2 (fmt_extension c ctx "%" ext)
  | Ppat_open ({txt}, pat) ->
      cbox 0
        ( fmt_longident txt $ fmt ".("
        $ fmt_pattern c (sub_pat ~ctx pat)
        $ fmt ")" )

and fmt_fun_args c ?(pro= fmt "") args =
  let fmt_fun_arg = function
    | Val (Nolabel, xpat, None) -> fmt_pattern c xpat
    | Val
        ( Labelled l
        , ( { ast=
                { ppat_desc=
                    ( Ppat_var {txt; loc}
                    | Ppat_constraint
                        ( { ppat_desc= Ppat_var {txt; loc}
                          ; ppat_attributes= [] }
                        , _ ) )
                ; ppat_attributes= [] } } as xpat )
        , None )
      when String.equal l txt ->
        Cmts.fmt c.cmts loc @@ cbox 0 (fmt "~" $ fmt_pattern c xpat)
    | Val (Labelled l, xpat, None) ->
        cbox 0 (fmt "~" $ str l $ fmt ":" $ fmt_pattern c xpat)
    | Val
        ( Optional l
        , ( { ast=
                { ppat_desc=
                    ( Ppat_var {txt; loc}
                    | Ppat_constraint
                        ( { ppat_desc= Ppat_var {txt; loc}
                          ; ppat_attributes= [] }
                        , _ ) )
                ; ppat_attributes= [] } } as xpat )
        , None )
      when String.equal l txt ->
        Cmts.fmt c.cmts loc @@ cbox 0 (fmt "?" $ fmt_pattern c xpat)
    | Val
        ( Optional l
        , {ast= {ppat_desc= Ppat_var {txt; loc}; ppat_attributes= []}}
        , None )
      when String.equal l txt ->
        Cmts.fmt c.cmts loc @@ cbox 0 (fmt "?" $ str l)
    | Val (Optional l, xpat, None) ->
        cbox 0 (fmt "?" $ str l $ fmt ":" $ fmt_pattern c xpat)
    | Val
        ( Optional l
        , {ast= {ppat_desc= Ppat_var {txt; loc}; ppat_attributes= []}}
        , Some xexp )
      when String.equal l txt ->
        Cmts.fmt c.cmts loc
        @@ cbox 0
             (fmt "?(" $ str l $ fmt "= " $ fmt_expression c xexp $ fmt ")")
    | Val (Optional l, xpat, Some xexp) ->
        cbox 0
          ( fmt "?" $ str l $ fmt ":(" $ fmt_pattern c xpat $ fmt " = "
          $ fmt_expression c xexp $ fmt ")" )
    | Val ((Labelled _ | Nolabel), _, Some _) ->
        impossible "not accepted by parser"
    | Newtypes [] -> impossible "not accepted by parser"
    | Newtypes names ->
        cbox 0
          (wrap "(" ")"
             ( fmt "type "
             $ list names "@ " (fun {txt; loc} ->
                   Cmts.fmt c.cmts loc @@ str txt ) ))
  in
  fmt_if_k
    (not (List.is_empty args))
    (pro $ list args "@;" (fun x -> hovbox 0 (fmt_fun_arg x)))

and fmt_body c ({ast= body} as xbody) =
  let ctx = Exp body in
  match body with
  | {pexp_desc= Pexp_function cs; pexp_attributes} ->
      fmt "@ function"
      $ fmt_attributes c ~key:"@" pexp_attributes
      $ close_box $ fmt "@ " $ fmt_cases c ctx cs
  | _ ->
      close_box $ fmt "@ " $ fmt_expression c ~eol:(fmt "@;<1000 0>") xbody

and fmt_index_op c ctx ~parens ?set (s, opn, cls) l i =
  wrap_if parens "(" ")"
    (hovbox 0
       ( fmt_expression c (sub_exp ~ctx l)
       $ str (Printf.sprintf "%s%c" s opn)
       $ fmt_expression c (sub_exp ~ctx i)
       $ str (Printf.sprintf "%c" cls)
       $
       match set with
       | None -> fmt ""
       | Some e -> fmt "@ <- " $ fmt_expression c (sub_exp ~ctx e) ))

and fmt_expression c ?(box= true) ?epi ?eol ?parens ?ext ({ast= exp} as xexp)
    =
  protect (Exp exp)
  @@
  let {pexp_desc; pexp_loc; pexp_attributes} = exp in
  let fmt_cmts = Cmts.fmt c.cmts ?eol pexp_loc in
  let fmt_atrs = fmt_attributes c ~pre:(fmt " ") ~key:"@" pexp_attributes in
  let parens = match parens with Some b -> b | None -> parenze_exp xexp in
  let width xe =
    String.length
      (Format.asprintf "%t" (Cmts.preserve (fun () -> fmt_expression c xe)))
  in
  let fmt_label lbl sep =
    match lbl with
    | Nolabel -> fmt ""
    | Labelled l -> fmt "~" $ str l $ fmt sep
    | Optional l -> fmt "?" $ str l $ fmt sep
  in
  let fmt_label_arg ?(box= box) ?epi ?parens (lbl, ({ast= arg} as xarg)) =
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
         |Pexp_letmodule _ | Pexp_match _ | Pexp_newtype _ | Pexp_open _
         |Pexp_sequence _ | Pexp_try _ ->
            true
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
               (if parens_or_forced then "( " else ""))
          $ fmt_cmts
          $ fmt_if_k first
              (open_hovbox (if first_grp && parens then -2 else 0))
          $ fmt_op_args ~first:very_first op_args ~last:very_last
          $ fmt_if_k last close_box
          $ fmt_or_k very_last
              (fits_breaks_if parens_or_nested ")"
                 (if parens_or_forced then "@ )" else ""))
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
    list_fl
      (List.group ((Nolabel, e0) :: a1N) ~break:(fun (_, a1) (_, a2) ->
           (not (is_simple c.conf width (sub_exp ~ctx a1)))
           || (not (is_simple c.conf width (sub_exp ~ctx a2))) ))
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
              ( {txt}
              , PStr
                  [ ( { pstr_desc=
                          Pstr_eval
                            (({pexp_desc= Pexp_fun _; _} as call_fun), []); _
                      } as pld ) ] ) }
      , e2 ) ->
      let xargs, xbody =
        sugar_fun c None (sub_exp ~ctx:(Str pld) call_fun)
      in
      hvbox 0
        (wrap_if parens "(" ")"
           ( hvbox 2
               (wrap "[" "]"
                  ( str "%"
                  $ hovbox 2
                      ( str txt $ fmt " "
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
      ( {pexp_desc= Pexp_ident {txt= Lident "|>"}; pexp_attributes= []}
      , [ (Nolabel, e0)
        ; ( Nolabel
          , { pexp_desc=
                Pexp_extension
                  ( {txt}
                  , PStr
                      [ ( { pstr_desc=
                              Pstr_eval
                                ( ({pexp_desc= Pexp_fun _; _} as retn_fun)
                                , [] ); _ } as pld ) ] ) } ) ] ) ->
      let xargs, xbody =
        sugar_fun c None (sub_exp ~ctx:(Str pld) retn_fun)
      in
      hvbox 0
        (wrap_fits_breaks_if parens "(" ")"
           ( fmt_expression c (sub_exp ~ctx e0)
           $ fmt "@\n|>@\n"
           $ hvbox 2
               (wrap "[" "]"
                  ( str "%"
                  $ hovbox 2
                      ( str txt $ fmt " "
                      $ ( fmt "fun "
                        $ fmt_attributes c ~suf:(fmt " ")
                            retn_fun.pexp_attributes ~key:"@"
                        $ fmt_fun_args c xargs $ fmt "@ ->" ) )
                  $ fmt "@ " $ fmt_expression c xbody )) ))
  | Pexp_apply
      ( { pexp_desc= Pexp_ident {txt= Ldot (Lident "Array", "get")}
        ; pexp_attributes= [] }
      , [(Nolabel, s); (Nolabel, i)] ) ->
      fmt_index_op c ctx ~parens index_op_array s i
  | Pexp_apply
      ( { pexp_desc= Pexp_ident {txt= Ldot (Lident "String", "get")}
        ; pexp_attributes= [] }
      , [(Nolabel, s); (Nolabel, i)] ) ->
      fmt_index_op c ctx ~parens index_op_string s i
  | Pexp_apply
      ( { pexp_desc= Pexp_ident {txt= Ldot (Lident "Array", "set")}
        ; pexp_attributes= [] }
      , [(Nolabel, s); (Nolabel, i); (Nolabel, e)] ) ->
      fmt_index_op c ctx ~parens index_op_array s i ~set:e
  | Pexp_apply
      ( { pexp_desc= Pexp_ident {txt= Ldot (Lident "String", "set")}
        ; pexp_attributes= [] }
      , [(Nolabel, s); (Nolabel, i); (Nolabel, e)] ) ->
      fmt_index_op c ctx ~parens index_op_string s i ~set:e
  | Pexp_apply
      ( {pexp_desc= Pexp_ident {txt= Lident ":="}; pexp_attributes= []}
      , [(Nolabel, r); (Nolabel, v)] )
    when is_simple c.conf width (sub_exp ~ctx r) ->
      wrap_if parens "(" ")"
        (hovbox 0
           ( fmt_expression c (sub_exp ~ctx r)
           $ fmt " :=@;<1 2>"
           $ hvbox 2 (fmt_expression c (sub_exp ~ctx v)) ))
  | Pexp_apply
      ( { pexp_desc= Pexp_ident {txt= Lident "~-"}
        ; pexp_loc
        ; pexp_attributes= [] }
      , [(Nolabel, e1)] ) ->
      let spc =
        match e1 with
        | {pexp_desc= Pexp_apply (op, _)} when is_prefix op -> fmt "@ "
        | _ -> fmt ""
      in
      wrap_if parens "(" ")"
        ( Cmts.fmt c.cmts pexp_loc
        @@ hvbox 2 (str "-" $ spc $ fmt_expression c (sub_exp ~ctx e1)) )
      $ fmt_atrs
  | Pexp_apply
      ( ( { pexp_desc= Pexp_ident {txt= Lident maybe_hash}
          ; pexp_attributes= [] } as op )
      , [(Nolabel, l); (Nolabel, ({pexp_desc= Pexp_ident _} as r))] )
    when String.is_prefix ~prefix:"#" maybe_hash ->
      wrap_if parens "(" ")"
        ( fmt_expression c (sub_exp ~ctx l)
        $ fmt_expression c (sub_exp ~ctx op)
        $ fmt_expression c (sub_exp ~ctx r) )
  | Pexp_apply
      ( ({pexp_desc= Pexp_ident {txt= Lident id}} as e0)
      , (Nolabel, _) :: (Nolabel, _) :: _ )
    when is_infix_id id && List.is_empty e0.pexp_attributes ->
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
      ( {pexp_desc= Pexp_ident {txt= Lident id}}
      , (Nolabel, s) :: (Nolabel, i) :: _ )
    when Option.is_some (index_op_get id) -> (
    match index_op_get id with
    | Some index_op -> fmt_index_op c ctx ~parens index_op s i
    | None -> impossible "previous match" )
  | Pexp_apply
      ( {pexp_desc= Pexp_ident {txt= Lident id}}
      , (Nolabel, s) :: (Nolabel, i) :: (Nolabel, e) :: _ )
    when Option.is_some (index_op_set id) -> (
    match index_op_set id with
    | Some index_op -> fmt_index_op c ctx ~parens index_op s i ~set:e
    | None -> impossible "previous match" )
  | Pexp_apply (e0, [(Nolabel, e1)]) when is_prefix e0 ->
      hvbox 2
        (wrap_fits_breaks_if parens "(" ")"
           ( fmt_expression c ~box (sub_exp ~ctx e0)
           $ fmt_expression c ~box (sub_exp ~ctx e1)
           $ fmt_atrs ))
  | Pexp_apply (e0, a1N)
    when is_infix e0 && List.is_empty e0.pexp_attributes ->
      hvbox 2
        ( wrap_fits_breaks_if parens "(" ")" (fmt_args_grouped e0 a1N)
        $ fmt_atrs )
  | Pexp_apply (e0, e1N1) -> (
    match List.rev e1N1 with
    | (lbl, ({pexp_desc= Pexp_fun _; pexp_loc} as eN1)) :: rev_e1N
      when List.for_all rev_e1N ~f:(fun (_, eI) ->
               is_simple c.conf (fun _ -> 0) (sub_exp ~ctx eI) ) ->
        let e1N = List.rev rev_e1N in
        (* side effects of Cmts.fmt c.cmts before sugar_fun is important *)
        let fmt_cmts = Cmts.fmt c.cmts pexp_loc in
        let xargs, xbody = sugar_fun c None (sub_exp ~ctx eN1) in
        hvbox 0
          ( wrap_if parens "(" ")"
              (hovbox 0
                 ( hovbox 2
                     ( hovbox 2
                         ( fmt_args_grouped e0 e1N $ fmt "@ "
                         $ fmt_label lbl ":@,"
                         $ fmt_cmts
                           @@ hvbox 0
                                ( fmt "(fun " $ fmt_fun_args c xargs
                                $ fmt "@ ->" ) )
                     $ fmt "@;<1 2>"
                     $ fmt_expression c
                         ?box:
                           ( match xbody.ast.pexp_desc with
                           | Pexp_fun _ | Pexp_function _ -> Some false
                           | _ -> None )
                         xbody )
                 $ fits_breaks ")" "@ )" ))
          $ fmt_atrs )
    | ( lbl
      , ({pexp_desc= Pexp_function [{pc_lhs; pc_guard= None; pc_rhs}]} as eN)
      )
      :: rev_e1N
      when List.for_all rev_e1N ~f:(fun (_, eI) ->
               is_simple c.conf (fun _ -> 0) (sub_exp ~ctx eI) ) ->
        let e1N = List.rev rev_e1N in
        let ctx = Exp eN in
        hvbox 2
          ( wrap_if parens "(" ")"
              (hovbox 4
                 ( fmt_args_grouped e0 e1N $ fmt "@ " $ fmt_label lbl ":@,"
                 $ fmt "(function"
                 $ fmt_attributes c ~pre:(fmt " ") ~key:"@"
                     eN.pexp_attributes
                 $ fmt "@ "
                 $ hvbox 0
                     ( fmt_pattern c ~pro:(if_newline "| ")
                         (sub_pat ~ctx pc_lhs)
                     $ fmt "@ ->" )
                 $ fmt "@ "
                 $ cbox 0 (fmt_expression c (sub_exp ~ctx pc_rhs))
                 $ fits_breaks ")" " )" ))
          $ fmt_atrs )
    | (lbl, ({pexp_desc= Pexp_function cs} as eN)) :: rev_e1N
      when List.for_all rev_e1N ~f:(fun (_, eI) ->
               is_simple c.conf (fun _ -> 0) (sub_exp ~ctx eI) ) ->
        let e1N = List.rev rev_e1N in
        let ctx'' = Exp eN in
        hvbox 2
          ( wrap_if parens "(" ")"
              ( hovbox 2
                  ( fmt_args_grouped e0 e1N $ fmt "@ " $ fmt_label lbl ":@,"
                  $ fmt "(function"
                  $ fmt_attributes c ~pre:(fmt " ") ~key:"@"
                      eN.pexp_attributes )
              $ fmt "@ " $ fmt_cases c ctx'' cs $ fits_breaks ")" " )" )
          $ fmt_atrs )
    | _ ->
        wrap_if parens "(" ")"
          (hvbox 2 (fmt_args_grouped e0 e1N1) $ fmt_atrs) )
  | Pexp_array [] ->
      hvbox 0
        ( wrap_fits_breaks "[|" "|]" (Cmts.fmt_within c.cmts pexp_loc)
        $ fmt_atrs )
  | Pexp_array e1N ->
      hvbox 0
        ( wrap_fits_breaks "[|" "|]"
            (list e1N "@;<0 1>; " (sub_exp ~ctx >> fmt_expression c))
        $ fmt_atrs )
  | Pexp_assert e0 ->
      let paren_body = parenze_exp (sub_exp ~ctx e0) in
      hovbox 0
        ( hovbox 0
            ( hvbox 2
                (wrap_if parens "(" ")"
                   ( fmt_or paren_body "assert (@," "assert@ "
                   $ fmt_expression c ~parens:false (sub_exp ~ctx e0) ))
            $ fmt "@," $ fmt_atrs )
        $ fits_breaks_if paren_body ")" "@ )" )
  | Pexp_constant const ->
      wrap_if
        (parens || (not (List.is_empty pexp_attributes)))
        "(" ")"
        (fmt_constant c ~loc:pexp_loc ?epi const $ fmt_atrs)
  | Pexp_constraint
      ( {pexp_desc= Pexp_pack me; pexp_attributes= []}
      , {ptyp_desc= Ptyp_package pty; ptyp_attributes= []} ) ->
      let {opn; pro; psp; bdy; cls; esp; epi} =
        fmt_module_expr c (sub_mod ~ctx me)
      in
      opn
      $ wrap_fits_breaks "(" ")"
          ( fmt "module " $ Option.call ~f:pro $ psp $ bdy $ cls $ esp
          $ Option.call ~f:epi $ fmt "@ : "
          $ fmt_package_type c ctx pty )
      $ fmt_atrs
  | Pexp_constraint (e, t) ->
      wrap_fits_breaks "(" ")"
        ( fmt_expression c (sub_exp ~ctx e)
        $ fmt "@ : "
        $ fmt_core_type c (sub_typ ~ctx t) )
      $ fmt_atrs
  | Pexp_construct ({txt; loc}, None) -> (
    match txt with
    | Lident (("()" | "[]") as txt) ->
        let opn = txt.[0] and cls = txt.[1] in
        Cmts.fmt c.cmts loc
        @@ hvbox 0
             (wrap_if
                (not (List.is_empty pexp_attributes))
                "(" ")"
                ( wrap_k (char opn) (char cls)
                    (Cmts.fmt_within c.cmts ~pro:(fmt " ") ~epi:(fmt " ")
                       pexp_loc)
                $ fmt_atrs ))
    | _ -> Cmts.fmt c.cmts loc @@ fmt_longident txt $ fmt_atrs )
  | Pexp_construct
      ( {txt= Lident "::"}
      , Some {pexp_desc= Pexp_tuple [_; _]; pexp_attributes= []} ) -> (
    match sugar_list_exp c exp with
    | Some (loc_xes, nil_loc) ->
        hvbox 0
          (wrap_if
             (not (List.is_empty pexp_attributes))
             "(" ")"
             ( wrap_fits_breaks "[" "]"
                 ( list loc_xes "@,; " (fun (locs, xexp) ->
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
  | Pexp_construct ({txt; loc}, Some arg) ->
      Cmts.fmt c.cmts loc
      @@ wrap_if parens "(" ")"
           ( hvbox 2
               ( fmt_longident txt $ fmt "@ "
               $ fmt_expression c (sub_exp ~ctx arg) )
           $ fmt_atrs )
  | Pexp_variant (s, arg) ->
      hvbox 2
        ( wrap_if parens "(" ")"
            ( fmt "`" $ str s
            $ opt arg (fmt "@ " >$ (sub_exp ~ctx >> fmt_expression c)) )
        $ fmt_atrs )
  | Pexp_field (exp, {txt}) ->
      hvbox 2
        ( wrap_if parens "(" ")"
            ( fmt_expression c (sub_exp ~ctx exp)
            $ fmt "@,." $ fmt_longident txt )
        $ fmt_atrs )
  | Pexp_newtype _ | Pexp_fun _ ->
      let xargs, xbody = sugar_fun c None xexp in
      hvbox_if box
        (if Option.is_none eol then 2 else 1)
        ( fmt_if parens "("
        $ ( open_hovbox 2
          $ ( hovbox 4
                ( fmt "fun "
                $ fmt_attributes c ~key:"@" pexp_attributes ~suf:(fmt " ")
                $ fmt_fun_args c xargs $ fmt "@ " )
            $ fmt "->" )
          $ fmt_body c xbody )
        $ fits_breaks_if parens ")" "@ )" )
  | Pexp_function cs ->
      wrap_if parens "(" ")"
        ( hvbox 2
            (fmt "function" $ fmt_attributes c ~key:"@" pexp_attributes)
        $ fmt "@ "
        $ hvbox 0 (fmt_cases c ctx cs) )
  | Pexp_ident {txt; loc} ->
      let wrap, wrap_ident =
        if is_symbol exp && (not (List.is_empty pexp_attributes)) then
          (wrap_if true "( " " )", true)
        else if is_symbol exp then (wrap_if parens "( " " )", false)
        else (wrap_if parens "(" ")", false)
      in
      Cmts.fmt c.cmts loc
      @@ wrap (wrap_if wrap_ident "(" ")" (fmt_longident txt) $ fmt_atrs)
  | Pexp_ifthenelse _ ->
      let cnd_exps = sugar_ite c xexp in
      hvbox 0
        (wrap_fits_breaks_if parens "(" ")"
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
                      $ fmt_if parens_bch " )" )
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
                        $ fmt_if parens_bch " )" )
                    $ fmt_if (not last) "@ " )))
  | Pexp_let (rec_flag, bindings, body) ->
      wrap_if
        (parens || (not (List.is_empty pexp_attributes)))
        "(" ")"
        (vbox 0
           ( hvbox 0
               (list_fl bindings (fun ~first ~last binding ->
                    fmt_value_binding c ~rec_flag ~first
                      ?ext:(if first then ext else None)
                      ctx binding
                      ~in_:(fun indent ->
                        fmt_if_k last (break 1 (-indent) $ fmt "in") )
                    $ fmt_if (not last) "@ " ))
           $ fmt "@;<1000 0>"
           $ hvbox 0 (fmt_expression c (sub_exp ~ctx body)) ))
      $ fmt_atrs
  | Pexp_letexception (ext_cstr, exp) ->
      hvbox 0
        ( wrap_if
            (parens || (not (List.is_empty pexp_attributes)))
            "(" ")"
            ( hvbox 0
                ( fmt_exception ~pre:(fmt "let exception@ ") c (fmt ": ")
                    ctx ext_cstr
                $ fmt "@ in" )
            $ fmt "@;<1000 0>"
            $ fmt_expression c (sub_exp ~ctx exp) )
        $ fmt_atrs )
  | Pexp_letmodule (name, pmod, exp) ->
      let {pmod_desc= _; pmod_attributes} = pmod in
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
            (parens || (not (List.is_empty pexp_attributes)))
            "(" ")"
            ( hvbox 2
                ( fmt_module c keyword name xargs (Some xbody) true xmty
                    (List.append pmod_attributes pmod.pmod_attributes)
                $ fmt " in" )
            $ fmt "@;<1000 0>"
            $ fmt_expression c (sub_exp ~ctx exp) )
        $ fmt_atrs )
  | Pexp_open (flag, {txt; loc}, e0) ->
      let override = Poly.(flag = Override) in
      let force_break_if =
        match e0.pexp_desc with
        | Pexp_let _ | Pexp_extension _ | Pexp_letexception _
         |Pexp_letmodule _ | Pexp_open _ ->
            true
        | _ -> override
      in
      let force_fit_if =
        match xexp.ctx with
        | Exp {pexp_desc= Pexp_apply _ | Pexp_construct _} ->
            not force_break_if
        | _ -> false
      in
      let fits_breaks = fits_breaks ~force_fit_if ~force_break_if
      and fits_breaks_if = fits_breaks_if ~force_fit_if ~force_break_if in
      let opn, cls =
        let can_skip_parens =
          match e0.pexp_desc with
          | Pexp_array _ | Pexp_constraint _ | Pexp_record _ -> true
          | Pexp_tuple _ -> Poly.(c.conf.parens_tuple = `Always)
          | _ ->
            match sugar_list_exp c e0 with Some _ -> true | None -> false
        in
        if can_skip_parens then (".", "") else (".(", ")")
      in
      hvbox 0
        ( Cmts.fmt c.cmts loc @@ fits_breaks_if parens "" "("
        $ fits_breaks "" (if override then "let open! " else "let open ")
        $ fmt_longident txt $ fits_breaks opn " in"
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
      match cs with
      | []
       |_ :: _ :: _
       |[ { pc_lhs=
              {ppat_desc= Ppat_or _ | Ppat_alias ({ppat_desc= Ppat_or _}, _)}
          } ] ->
          hvbox 0
            (wrap_fits_breaks_if parens "(" ")"
               ( hvbox 0
                   ( str keyword
                   $ fmt_extension_suffix c ext
                   $ fmt_attributes c ~key:"@" pexp_attributes
                   $ fmt "@;<1 2>"
                   $ fmt_expression c (sub_exp ~ctx e0)
                   $ fmt "@ with" )
               $ fmt "@ " $ fmt_cases c ctx cs ))
      | [{pc_lhs; pc_guard; pc_rhs}] ->
          wrap_fits_breaks_if parens "(" ")"
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
                   ( break_unless_newline 1 0 $ fmt "with@ "
                   $ hvbox 0
                       ( fmt_pattern c ~pro:(if_newline "| ")
                           (sub_pat ~ctx pc_lhs)
                       $ opt pc_guard (fun g ->
                             fmt "@ when "
                             $ fmt_expression c (sub_exp ~ctx g) ) )
                   $ fmt "@ ->" )
               $ fmt "@ "
               $ cbox 0 (fmt_expression c (sub_exp ~ctx pc_rhs)) )) )
  | Pexp_pack me ->
      let {opn; pro; psp; bdy; cls; esp; epi} =
        fmt_module_expr c (sub_mod ~ctx me)
      in
      opn
      $ wrap_fits_breaks "(" ")"
          ( fmt "module " $ Option.call ~f:pro $ psp $ bdy $ cls $ esp
          $ Option.call ~f:epi )
      $ fmt_atrs
  | Pexp_record (flds, default) ->
      let fmt_field ({txt; loc}, f) =
        Cmts.fmt c.cmts loc
        @@
        match f.pexp_desc with
        | Pexp_ident {txt= txt'; loc}
          when field_alias txt txt' && List.is_empty f.pexp_attributes ->
            Cmts.fmt c.cmts loc @@ cbox 2 (fmt_longident txt)
        | Pexp_constraint
            (({pexp_desc= Pexp_ident {txt= txt'; loc}} as e), t)
          when field_alias txt txt' && List.is_empty f.pexp_attributes ->
            Cmts.fmt c.cmts loc @@ fmt_expression c (sub_exp ~ctx:(Exp f) e)
            $ fmt " : "
            $ fmt_core_type c (sub_typ ~ctx:(Exp f) t)
        | _ ->
            cbox 2
              ( fmt_longident txt $ fmt "=@ "
              $ cbox 0 (fmt_expression c (sub_exp ~ctx f)) )
      in
      hvbox 0
        ( wrap_fits_breaks "{" "}"
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
           ( wrap_fits_breaks_if parens "(" ")"
               ( fmt_expression c ?parens:parens1 (sub_exp ~ctx e1)
               $ fmt " ;"
               $ fmt_extension_suffix c ext
               $ fmt "@ "
               $ fmt_expression c (sub_exp ~ctx e2) )
           $ fmt_atrs ))
  | Pexp_sequence _ ->
      hvbox 0
        (hvbox_if parens 2
           ( wrap_fits_breaks_if parens "(" ")"
               (list (sugar_sequence c width xexp) " ;@;<1000 0>"
                  (fun grp -> list grp " ;@ " (fmt_expression c) ))
           $ fmt_atrs ))
  | Pexp_setfield (e1, {txt}, e2) ->
      hvbox 0
        ( wrap_fits_breaks_if parens "(" ")"
            ( fmt_expression c (sub_exp ~ctx e1)
            $ fmt "." $ fmt_longident txt $ fmt "@ <- "
            $ fmt_expression c (sub_exp ~ctx e2) )
        $ fmt_atrs )
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
        if parens then wrap_fits_breaks "(" ")"
        else if no_parens_if_break then Fn.id
        else wrap_if_breaks "( " "@ )"
      in
      hvbox 0
        (wrap (list es "@,, " (sub_exp ~ctx >> fmt_expression c)) $ fmt_atrs)
  | Pexp_lazy e ->
      hvbox 2
        ( wrap_fits_breaks_if parens "(" ")"
            (fmt "lazy@ " $ fmt_expression c (sub_exp ~ctx e))
        $ fmt_atrs )
  | Pexp_extension
      ( ext
      , PStr
          [ ( { pstr_desc=
                  Pstr_eval
                    ( ( { pexp_desc=
                            ( Pexp_while _ | Pexp_for _ | Pexp_match _
                            | Pexp_try _ | Pexp_let _ | Pexp_ifthenelse _
                            | Pexp_sequence _ | Pexp_new _
                            | Pexp_letmodule _ | Pexp_object _ )
                        ; pexp_attributes= [] } as e1 )
                    , _ ) } as str ) ] ) ->
      hvbox 0
        ( fmt_expression c ~box ?eol ~parens ~ext (sub_exp ~ctx:(Str str) e1)
        $ fmt_atrs )
  | Pexp_extension ext -> hvbox 2 (fmt_extension c ctx "%" ext) $ fmt_atrs
  | Pexp_for (p1, e1, e2, dir, e3) ->
      hvbox 0
        (wrap_fits_breaks_if parens "(" ")"
           (hovbox 0
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
              $ fmt "@;<1000 0>done" )))
      $ fmt_atrs
  | Pexp_coerce (e1, t1, t2) ->
      hvbox 2
        ( wrap_fits_breaks "(" ")"
            ( fmt_expression c (sub_exp ~ctx e1)
            $ opt t1 (fmt "@ : " >$ (sub_typ ~ctx >> fmt_core_type c))
            $ fmt "@ :> "
            $ fmt_core_type c (sub_typ ~ctx t2) )
        $ fmt_atrs )
  | Pexp_while (e1, e2) ->
      hvbox 0
        ( wrap_fits_breaks_if parens "(" ")"
            (hovbox 0
               ( hvbox 2
                   ( hvbox 0
                       ( fmt "while"
                       $ fmt_extension_suffix c ext
                       $ fmt "@;<1 2>"
                       $ fmt_expression c (sub_exp ~ctx e1)
                       $ fmt "@;do" )
                   $ fmt "@;<1000 0>"
                   $ fmt_expression c (sub_exp ~ctx e2) )
               $ fmt "@;<1000 0>done" ))
        $ fmt_atrs )
  | Pexp_unreachable -> fmt "."
  | Pexp_send (exp, {txt; loc}) ->
      Cmts.fmt c.cmts loc
      @@ hvbox 2
           ( wrap_if parens "(" ")"
               (fmt_expression c (sub_exp ~ctx exp) $ fmt "@,#" $ str txt)
           $ fmt_atrs )
  | Pexp_new {txt; loc} ->
      Cmts.fmt c.cmts loc
      @@ hvbox 2
           ( wrap_if parens "(" ")"
               ( fmt "new"
               $ fmt_extension_suffix c ext
               $ fmt "@ " $ fmt_longident txt )
           $ fmt_atrs )
  | Pexp_object {pcstr_self; pcstr_fields} ->
      fmt_class_structure c ~ctx ~parens ?ext pcstr_self pcstr_fields
      $ fmt_atrs
  | Pexp_override l -> (
      let field_alias (x: string) (li: Longident.t) =
        match li with Lident y -> String.equal x y | _ -> false
      in
      let field ({txt; loc}, f) =
        match f.pexp_desc with
        | Pexp_ident {txt= txt'; loc} when field_alias txt txt' ->
            Cmts.fmt c.cmts ~eol:(fmt "") loc @@ fmt_longident txt'
        | Pexp_constraint
            (({pexp_desc= Pexp_ident {txt= txt'; loc}} as e), t)
          when field_alias txt txt' ->
            Cmts.fmt c.cmts ~eol:(fmt "") loc
            @@ fmt_expression c (sub_exp ~ctx:(Exp f) e)
            $ fmt " : "
            $ fmt_core_type c (sub_typ ~ctx:(Exp f) t)
        | _ ->
            Cmts.fmt c.cmts ~eol:(fmt "") loc @@ str txt
            $ fmt " = "
            $ fmt_expression c (sub_exp ~ctx f)
      in
      match l with
      | [] -> wrap "{<" ">}" (Cmts.fmt_within c.cmts pexp_loc)
      | _ ->
          hvbox 0
            ( wrap "{<" ">}"
                (list_fl l (fun ~first ~last:_ f ->
                     fmt_if_k (not first) (fmt "; ")
                     $ fits_breaks "" " "
                     $ hvbox 0 (field f)
                     $ fmt "@," ))
            $ fmt_atrs ) )
  | Pexp_setinstvar (name, expr) ->
      hvbox 0
        (wrap_fits_breaks_if parens "(" ")"
           ( str name.txt $ fmt " <-@;<1 2>"
           $ hvbox 2 (fmt_expression c (sub_exp ~ctx expr)) ))
  | Pexp_poly _ ->
      impossible "only used for methods, handled during method formatting"

and fmt_class_structure c ~ctx ~parens ?ext self_ fields =
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
  hvbox 0
    (wrap_if parens "(" ")"
       ( hvbox 2
           ( hvbox 0
               ( fmt "object"
               $ fmt_extension_suffix c ext
               $ opt self_ (fun self_ ->
                     fmt "@;"
                     $ wrap "(" ")" (fmt_pattern c (sub_pat ~ctx self_)) )
               )
           $ cmts_after_self
           $ fmt_if Poly.(fields <> []) "@;<1000 0>"
           $ hvbox 0
               (list fields "\n@\n" (fun cf -> fmt_class_field c ctx cf)) )
       $ fmt_or_k Poly.(fields <> []) (fmt "@\n") (fmt "@ ")
       $ fmt "end" ))

and fmt_class_signature c ~ctx ~parens ?ext self_ fields =
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
           $ fmt_if Poly.(fields <> []) "@;<1000 0>"
           $ hvbox 0
               (list fields "\n@\n" (fun cf -> fmt_class_type_field c ctx cf))
           )
       $ fmt_or_k Poly.(fields <> []) (fmt "@\n") (fmt "@ ")
       $ fmt "end" ))

and fmt_class_type c ?(box= true) ({ast= typ} as xtyp) =
  protect (Cty typ)
  @@
  let {pcty_desc; pcty_loc; pcty_attributes} = typ in
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
  | Pcty_constr ({txt; loc}, params) ->
      let params = List.map params ~f:(fun x -> (x, Invariant)) in
      Cmts.fmt c.cmts loc @@ fmt_class_params c ctx ~epi:(fmt "@ ") params
      $ fmt_longident txt
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
  | Pcty_open (flag, {txt}, cl) ->
      hvbox 0
        ( str "let open"
        $ fmt_if Poly.(flag = Override) "!"
        $ str " " $ fmt_longident txt $ fmt " in@;<1000 0>"
        $ fmt_class_type c (sub_cty ~ctx cl) ) )
  $ fmt_docstring c ~pro:(fmt "@ ") doc

and fmt_class_expr c ?eol ?(box= true) ({ast= exp} as xexp) =
  protect (Cl exp)
  @@
  let {pcl_desc; pcl_loc; pcl_attributes} = exp in
  let parens = parenze_cl xexp in
  let fmt_label lbl sep =
    match lbl with
    | Nolabel -> fmt ""
    | Labelled l -> fmt "~" $ str l $ fmt sep
    | Optional l -> fmt "?" $ str l $ fmt sep
  in
  let fmt_label_arg ?(box= box) ?epi ?parens (lbl, ({ast= arg} as xarg)) =
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
  let width xe =
    String.length
      (Format.asprintf "%t" (Cmts.preserve (fun () -> fmt_expression c xe)))
  in
  let fmt_args_grouped e0 a1N =
    (* TODO: consider [e0] when grouping *)
    fmt_class_expr c (sub_cl ~ctx e0)
    $ fmt "@ "
    $ list_fl
        (List.group a1N ~break:(fun (_, a1) (_, a2) ->
             (not (is_simple c.conf width (sub_exp ~ctx a1)))
             || (not (is_simple c.conf width (sub_exp ~ctx a2))) ))
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
  let fmt_cmts = Cmts.fmt c.cmts ?eol pcl_loc in
  let fmt_atrs = fmt_attributes c ~pre:(fmt " ") ~key:"@" pcl_attributes in
  ( hvbox_if box 0 @@ fmt_cmts
  @@
  match pcl_desc with
  | Pcl_constr ({txt; loc}, params) ->
      let params = List.map params ~f:(fun x -> (x, Invariant)) in
      Cmts.fmt c.cmts loc @@ fmt_class_params c ctx ~epi:(fmt "@ ") params
      $ fmt_longident txt
  | Pcl_structure {pcstr_fields; pcstr_self} ->
      fmt_class_structure c ~ctx ~parens ?ext:None pcstr_self pcstr_fields
  | Pcl_fun (_, _, p, _) ->
      let xargs, xbody = sugar_cl_fun c (Some p) xexp in
      hvbox_if box
        (if Option.is_none eol then 2 else 1)
        ( fmt_if parens "("
        $ ( open_hovbox 2
          $ ( hovbox 4 (fmt "fun " $ fmt_fun_args c xargs $ fmt "@ ")
            $ fmt "->" )
          $ close_box $ fmt "@ "
          $ fmt_class_expr c ~eol:(fmt "@;<1000 0>") xbody )
        $ fits_breaks_if parens ")" "@ )" )
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
                    $ fmt_if (not last) "@ " ))
           $ fmt "@;<1000 0>"
           $ hvbox 0 (fmt_class_expr c (sub_cl ~ctx body)) ))
      $ fmt_atrs
  | Pcl_constraint (e, t) ->
      wrap_fits_breaks "(" ")"
        ( fmt_class_expr c (sub_cl ~ctx e)
        $ fmt "@ : "
        $ fmt_class_type c (sub_cty ~ctx t) )
      $ fmt_atrs
  | Pcl_extension ext -> fmt_extension c ctx "%" ext $ fmt_atrs
  | Pcl_open (flag, {txt}, cl) ->
      hvbox 0
        ( str "let open"
        $ fmt_if Poly.(flag = Override) "!"
        $ str " " $ fmt_longident txt $ fmt " in@;<1000 0>"
        $ fmt_class_expr c (sub_cl ~ctx cl)
        $ fmt_atrs ) )
  $ fmt_atrs

and fmt_class_field c ctx (cf: class_field) =
  let {pcf_desc; pcf_loc; pcf_attributes} = cf in
  let fmt_cmts = Cmts.fmt c.cmts ?eol:None pcf_loc in
  let doc, atrs = doc_atrs pcf_attributes in
  let fmt_atrs = fmt_attributes c ~pre:(fmt " ") ~key:"@@" atrs in
  let fmt_kind = function
    | Cfk_virtual typ -> fmt "@ : " $ fmt_core_type c (sub_typ ~ctx typ)
    | Cfk_concrete
        ( _
        , { pexp_desc=
              Pexp_poly
                (e, Some ({ptyp_desc= Ptyp_poly (poly_args, _)} as poly)) }
        )
      -> (
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
            fmt "@ : " $ fmt "type "
            $ list args "@ " (fun {txt; loc} ->
                  Cmts.fmt c.cmts loc @@ str txt )
            $ fmt ". "
            $ fmt_core_type c (sub_typ ~ctx t)
            $ fmt " =@;"
            $ hvbox 2 (fmt_expression c (sub_exp ~ctx e))
        | None ->
            fmt "@ : "
            $ fmt_core_type c (sub_typ ~ctx poly)
            $ fmt " =@;"
            $ hvbox 2 (fmt_expression c (sub_exp ~ctx e)) )
    | Cfk_concrete (_, {pexp_desc= Pexp_poly (e, poly)}) ->
        let xargs, xbody =
          match poly with
          | None -> sugar_fun c None (sub_exp ~ctx e)
          | Some _ -> ([], sub_exp ~ctx e)
        in
        let ty, e =
          match (xbody.ast, poly) with
          | {pexp_desc= Pexp_constraint (e, t)}, None ->
              (Some t, sub_exp ~ctx e)
          | {pexp_desc= Pexp_constraint _}, Some _ -> (poly, xbody)
          | _, poly -> (poly, xbody)
        in
        fmt_fun_args ~pro:(fmt "@ ") c xargs
        $ opt ty (fun t -> fmt " : " $ fmt_core_type c (sub_typ ~ctx t))
        $ fmt "@ =@;"
        $ hvbox 2 (fmt_expression c e)
    | Cfk_concrete (_, e) ->
        let ty, e =
          match e with
          | {pexp_desc= Pexp_constraint (e, t)} -> (Some t, e)
          | _ -> (None, e)
        in
        opt ty (fun t -> fmt "@ : " $ fmt_core_type c (sub_typ ~ctx t))
        $ fmt "@ = "
        $ fmt_expression c (sub_exp ~ctx e)
  in
  let virtual_or_override = function
    | Cfk_virtual _ -> fmt "@ virtual"
    | Cfk_concrete (Override, _) -> fmt "!"
    | Cfk_concrete (Fresh, _) -> fmt ""
  in
  fmt_docstring c ~epi:(fmt "@\n") doc
  $ hvbox_if true 0
    @@ fmt_cmts
         ( match pcf_desc with
         | Pcf_inherit (override, cl, parent) ->
             fmt "inherit"
             $ fmt_if Poly.(override = Override) "!"
             $ fmt " "
             $ fmt_class_expr c (sub_cl ~ctx cl)
             $ opt parent (fun p -> fmt " as " $ str p.txt)
         | Pcf_method (name, priv, kind) ->
             hovbox 2
               ( fmt "method" $ virtual_or_override kind
               $ fmt_if Poly.(priv = Private) "@ private"
               $ fmt "@ " $ str name.txt $ fmt_kind kind )
         | Pcf_val (name, mut, kind) ->
             hovbox 2
               ( fmt "val" $ virtual_or_override kind
               $ fmt_if Poly.(mut = Mutable) "@ mutable"
               $ fmt "@ " $ str name.txt $ fmt_kind kind )
         | Pcf_constraint (t1, t2) ->
             fmt "constraint" $ fmt "@ "
             $ fmt_core_type c (sub_typ ~ctx t1)
             $ fmt " = "
             $ fmt_core_type c (sub_typ ~ctx t2)
         | Pcf_initializer e ->
             fmt "initializer" $ fmt "@ "
             $ fmt_expression c (sub_exp ~ctx e)
         | Pcf_attribute atr ->
             let doc, atrs = doc_atrs [atr] in
             fmt_docstring c ~epi:(fmt "") doc
             $ fmt_attributes c ~key:"@@@" atrs
         | Pcf_extension ext -> fmt_extension c ctx "%%" ext )
  $ fmt_atrs

and fmt_class_type_field c ctx (cf: class_type_field) =
  let {pctf_desc; pctf_loc; pctf_attributes} = cf in
  let fmt_cmts = Cmts.fmt c.cmts ?eol:None pctf_loc in
  let doc, atrs = doc_atrs pctf_attributes in
  let fmt_atrs = fmt_attributes c ~pre:(fmt " ") ~key:"@@" atrs in
  fmt_docstring c ~epi:(fmt "@\n") doc
  $ hvbox_if true 0
    @@ fmt_cmts
         ( match pctf_desc with
         | Pctf_inherit ct ->
             fmt "inherit " $ fmt_class_type c (sub_cty ~ctx ct)
         | Pctf_method (name, priv, virt, ty) ->
             hovbox 2
               ( fmt "method"
               $ fmt_if Poly.(virt = Virtual) "@ virtual"
               $ fmt_if Poly.(priv = Private) "@ private"
               $ fmt "@ " $ str name.txt $ fmt "@ :@ "
               $ fmt_core_type c (sub_typ ~ctx ty) )
         | Pctf_val (name, mut, virt, ty) ->
             hovbox 2
               ( fmt "val"
               $ fmt_if Poly.(virt = Virtual) "@ virtual"
               $ fmt_if Poly.(mut = Mutable) "@ mutable"
               $ fmt "@ " $ str name.txt $ fmt "@ :@ "
               $ fmt_core_type c (sub_typ ~ctx ty) )
         | Pctf_constraint (t1, t2) ->
             fmt "constraint" $ fmt "@ "
             $ fmt_core_type c (sub_typ ~ctx t1)
             $ fmt " = "
             $ fmt_core_type c (sub_typ ~ctx t2)
         | Pctf_attribute atr ->
             let doc, atrs = doc_atrs [atr] in
             fmt_docstring c ~epi:(fmt "") doc
             $ fmt_attributes c ~key:"@@@" atrs
         | Pctf_extension ext -> fmt_extension c ctx "%%" ext )
  $ fmt_atrs

and fmt_cases c ctx cs =
  list_fl cs (fun ~first ~last:_ {pc_lhs; pc_guard; pc_rhs} ->
      let xrhs = sub_exp ~ctx pc_rhs in
      let indent =
        match (ctx, pc_rhs.pexp_desc) with
        | ( Exp {pexp_desc= Pexp_function _ | Pexp_match _ | Pexp_try _}
          , (Pexp_match _ | Pexp_try _) ) ->
            2
        | _ -> 4
      in
      let paren_body = parenze_exp xrhs in
      let fmt_lhs =
        let xlhs = sub_pat ~ctx pc_lhs in
        let paren_lhs =
          match pc_lhs.ppat_desc with
          | Ppat_or _ when Option.is_some pc_guard -> true
          | _ -> parenze_pat xlhs
        in
        let fmt_arrow =
          fmt_or_k c.conf.sparse
            (fmt_or_k paren_body (fmt "@;<1 2>->") (fmt " ->@;<0 3>"))
            (fmt_or_k paren_body (fmt "@;<1 -2>-> (") (fmt " ->@;<0 -1>"))
        in
        hovbox 4
          ( hvbox 0
              ( fmt_pattern c
                  ~pro:(if first then if_newline "| " else fmt "| ")
                  ~parens:paren_lhs xlhs
              $ opt pc_guard (fun g ->
                    fmt "@;<1 2>when " $ fmt_expression c (sub_exp ~ctx g)
                ) )
          $ fmt_if_k (indent <= 2) fmt_arrow )
        $ fmt_if_k (indent > 2) fmt_arrow
      in
      fmt_if (not first) "@ "
      $ cbox_if (not c.conf.sparse) indent
          ( hvbox_if (not c.conf.sparse) indent fmt_lhs
          $ ( match (c.conf.sparse, indent > 2, paren_body) with
            | false, _, _ -> fmt "@ "
            | true, false, false -> fmt "@;<1 2>"
            | true, false, true -> fmt " (@;<1 2>"
            | true, true, false -> fmt " "
            | true, true, true -> fmt " (@;<1 4>" )
          $ hovbox 0
              ( hovbox 0 (fmt_expression c ~parens:false xrhs)
              $ fmt_if paren_body "@ )" ) ) )

and fmt_value_description c ctx vd =
  let {pval_name= {txt}; pval_type; pval_prim; pval_attributes} = vd in
  let pre = if List.is_empty pval_prim then "val" else "external" in
  let doc, atrs = doc_atrs pval_attributes in
  let doc_before =
    match c.conf.doc_comments with `Before -> true | `After -> false
  in
  hvbox 0
    ( fmt_if_k doc_before (fmt_docstring c ~epi:(fmt "@\n") doc)
    $ hvbox 2
        ( str pre $ fmt " "
        $ wrap_if (is_symbol_id txt) "( " " )" (str txt)
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
       ( wrap_fits_breaks_if
           (List.length params > 1)
           "(" ")"
           (list params "@,, " (fun (ty, vc) ->
                fmt_variance vc $ fmt_core_type c (sub_typ ~ctx ty) ))
       $ fmt " " ))

and fmt_class_params c ctx ~epi params =
  fmt_if_k
    (not (List.is_empty params))
    (hvbox 0
       ( wrap_fits_breaks "[" "]"
           (list_fl params (fun ~first ~last (ty, vc) ->
                fmt_if (first && exposed_left_typ ty) " "
                $ fmt_if_k (not first) (fmt "@,, ")
                $ fmt_variance vc
                $ fmt_core_type c (sub_typ ~ctx ty)
                $ fmt_if (last && exposed_right_typ ty) " " ))
       $ epi ))

and fmt_private_flag flag = fmt_if Poly.(flag = Private) "@ private"

and fmt_type_declaration c ?(pre= "") ?(suf= ("" : _ format)) ?(brk= suf)
    ctx ?fmt_name ?(eq= "=") decl =
  let fmt_manifest ~priv manifest =
    opt manifest (fun typ ->
        fmt " " $ str eq $ fmt_private_flag priv $ fmt "@ "
        $ fmt_core_type c (sub_typ ~ctx typ) )
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
            (wrap_fits_breaks "{" "}"
               (list_fl lbl_decls (fun ~first ~last x ->
                    fmt_if (not first) "@,; "
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
      $ hvbox 2
          (list cstrs "@ " (fun (t1, t2, _) ->
               fmt "constraint@ "
               $ fmt_core_type c (sub_typ ~ctx t1)
               $ fmt " =@ "
               $ fmt_core_type c (sub_typ ~ctx t2) )) )
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
  let doc, atrs = doc_atrs ptype_attributes in
  Cmts.fmt c.cmts loc @@ Cmts.fmt c.cmts ptype_loc
  @@ hvbox 0
       ( fmt_docstring c
           ~epi:
             ( match doc with
             | Some (_, true) -> fmt "\n@\n"
             | _ -> fmt "@\n" )
           doc
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
  let {pld_mutable; pld_name= {txt; loc}; pld_type; pld_loc; pld_attributes}
      =
    lbl_decl
  in
  let doc, atrs = doc_atrs pld_attributes in
  let fmt_cmts = Cmts.fmt c.cmts ~eol:(break_unless_newline 1 2) pld_loc in
  fmt_cmts
  @@ hvbox 4
       ( hvbox 2
           ( fmt_if Poly.(pld_mutable = Mutable) "mutable "
           $ Cmts.fmt c.cmts loc @@ str txt
           $ fmt ":@ "
           $ fmt_core_type c (sub_typ ~ctx pld_type) )
       $ fmt_attributes c ~pre:(fmt "@;<1 1>") ~box:false ~key:"@" atrs
       $ fmt_docstring c ~pro:(fmt "@;<2 0>") doc )

and fmt_constructor_declaration c ctx ~first ~last:_ cstr_decl =
  let {pcd_name= {txt; loc}; pcd_args; pcd_res; pcd_attributes; pcd_loc} =
    cstr_decl
  in
  let doc, atrs = doc_atrs pcd_attributes in
  fmt_if (not first) "@ "
  $ Cmts.fmt_before c.cmts pcd_loc
  $ Cmts.fmt_before c.cmts loc
  $ fmt_or_k first (if_newline "| ") (fmt "| ")
  $ hovbox 2
      ( hvbox 2
          ( wrap_if (is_symbol_id txt) "( " " )" (str txt)
          $ fmt_constructor_arguments_result c ctx pcd_args pcd_res )
      $ fmt_attributes c ~pre:(fmt "@;") ~key:"@" atrs
      $ fmt_docstring c ~pro:(fmt "@;<2 0>") doc )
  $ Cmts.fmt_after c.cmts ?pro:None ~epi:(fmt "@ ") loc
  $ Cmts.fmt_after c.cmts ?pro:None ~epi:(fmt "@ ") pcd_loc

and fmt_constructor_arguments c ctx pre args =
  match args with
  | Pcstr_tuple [] -> fmt ""
  | Pcstr_tuple typs ->
      fmt pre $ hvbox 0 (list typs "@ * " (sub_typ ~ctx >> fmt_core_type c))
  | Pcstr_record lds ->
      fmt pre
      $ wrap_fits_breaks "{" "}"
          (list lds "@,; " (fmt_label_declaration c ctx))

and fmt_constructor_arguments_result c ctx args res =
  let pre : _ format = if Option.is_none res then " of@ " else ":@ " in
  let before_type : _ format =
    match args with Pcstr_tuple [] -> ": " | _ -> "-> "
  in
  fmt_constructor_arguments c ctx pre args
  $ opt res (fun typ ->
        fmt "@ " $ fmt before_type $ fmt_core_type c (sub_typ ~ctx typ) )

and fmt_type_extension c ctx te =
  let { ptyext_params
      ; ptyext_path= {txt}
      ; ptyext_private
      ; ptyext_constructors
      ; ptyext_attributes } =
    te
  in
  let doc, atrs = doc_atrs ptyext_attributes in
  hvbox 2
    ( fmt_docstring c ~epi:(fmt "@,") doc
    $ hvbox 2
        ( fmt "type "
        $ fmt_tydcl_params c ctx ptyext_params
        $ fmt_longident txt $ fmt " +="
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
  let {pext_name= {txt}; pext_kind; pext_attributes} = ec in
  let doc, atrs = doc_atrs pext_attributes in
  hvbox 4
    ( hvbox 2
        ( str txt
        $
        match pext_kind with
        | Pext_decl ((Pcstr_tuple [] | Pcstr_record []), None) -> fmt ""
        | Pext_decl ((Pcstr_tuple [] | Pcstr_record []), Some res) ->
            sep $ fmt_core_type c (sub_typ ~ctx res)
        | Pext_decl (args, res) ->
            fmt_constructor_arguments_result c ctx args res
        | Pext_rebind {txt} -> fmt " = " $ fmt_longident txt )
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
  let parens = parenze_mty xmty in
  match pmty_desc with
  | Pmty_ident {txt} ->
      { empty with
        bdy= fmt_longident txt
      ; epi=
          Some (fmt_attributes c ~key:"@" pmty_attributes ~pre:(fmt "@ "))
      }
  | Pmty_signature s ->
      let empty = List.is_empty s in
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
            $ list xargs "@;<1 2>" (fun ({txt; _}, mt1) ->
                  let mt1 = Option.map ~f:(fmt_module_type c) mt1 in
                  wrap "(" ")"
                    (hovbox 0
                       ( str txt
                       $ opt mt1 (fun mt1 ->
                             let {opn; pro; psp; bdy; cls; esp; epi} =
                               mt1
                             in
                             opn $ fmt " :@," $ Option.call ~f:pro $ psp
                             $ fmt "@;<1 2>" $ bdy $ esp
                             $ Option.call ~f:epi $ cls ) )) )
            $ fmt "@;<1 2>-> " $ Option.call ~f:blk.pro )
      ; epi= Some (Option.call ~f:blk.epi $ Cmts.fmt_after c.cmts pmty_loc)
      }
  | Pmty_with (mt, wcs) ->
      let {opn; pro; psp; bdy; cls; esp; epi} =
        fmt_module_type c (sub_mty ~ctx mt)
      in
      { empty with
        bdy=
          hvbox 0
            (wrap_if parens "(" ")"
               ( opn $ Option.call ~f:pro $ psp $ bdy $ cls $ esp
               $ Option.call ~f:epi
               $ list_fl wcs (fun ~first ~last:_ wc ->
                     fmt_or first "@ with" "@;<1 1>and"
                     $ fmt_with_constraint c ctx wc ) ))
      ; epi=
          Some (fmt_attributes c ~key:"@" pmty_attributes ~pre:(fmt "@ "))
      }
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
  | Pmty_alias {txt} ->
      { empty with
        bdy= fmt_longident txt
      ; epi=
          Some (fmt_attributes c ~key:"@" pmty_attributes ~pre:(fmt "@ "))
      }

and fmt_signature c ctx itms =
  let grps =
    List.group itms ~break:(fun itmI itmJ ->
        let is_simple itm =
          match itm.psig_desc with
          | Psig_open _ -> true
          | Psig_module {pmd_type= {pmty_desc= Pmty_alias _}} -> true
          | _ -> false
        in
        (not (is_simple itmI)) || (not (is_simple itmJ)) )
  in
  let fmt_grp itms =
    list itms "@\n" (sub_sig ~ctx >> fmt_signature_item c)
  in
  hvbox 0 (list grps "\n@;<1000 0>" fmt_grp)

and fmt_signature_item c {ast= si} =
  protect (Sig si)
  @@ Cmts.fmt c.cmts ~epi:(fmt "\n@\n") ~eol:(fmt "\n@\n") si.psig_loc
  @@
  let ctx = Sig si in
  match si.psig_desc with
  | Psig_attribute atr ->
      let doc, atrs = doc_atrs [atr] in
      fmt_docstring c ~epi:(fmt "") doc $ fmt_attributes c ~key:"@@@" atrs
  | Psig_exception exc ->
      hvbox 2
        (fmt_exception ~pre:(fmt "exception@ ") c (fmt " of ") ctx exc)
  | Psig_extension (ext, atrs) ->
      hvbox 0
        ( fmt_extension c ctx "%%" ext
        $ fmt_attributes c ~pre:(fmt "@ ") ~key:"@@" atrs )
  | Psig_include {pincl_mod; pincl_attributes} ->
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
      hvbox 0
        (list_fl mds (fun ~first ~last decl ->
             fmt_module_declaration c ctx ~rec_flag:true ~first decl
             $ fmt_if (not last) "@,@\n" ))
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

and fmt_class_types c ctx ~pre ~sep (cls: class_type class_infos list) =
  list_fl cls (fun ~first ~last:_ cl ->
      let {pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes}
          =
        cl
      in
      let doc, atrs = doc_atrs pci_attributes in
      fmt_if (not first) "\n@\n"
      $ Cmts.fmt c.cmts pci_loc
        @@ fmt_docstring c
             ~epi:
               ( match doc with
               | Some (_, true) -> fmt "\n@\n"
               | _ -> fmt "@\n" )
             doc
      $ hovbox 2
          ( hvbox 2
              ( str (if first then pre else "and")
              $ fmt_if Poly.(pci_virt = Virtual) "@ virtual"
              $ fmt "@ "
              $ fmt_class_params c ctx ~epi:(fmt "@ ") pci_params
              $ str pci_name.txt $ fmt "@ " $ str sep )
          $ fmt "@;"
          $ fmt_class_type c (sub_cty ~ctx pci_expr)
          $ fmt_attributes c ~pre:(fmt "@;") ~key:"@@" atrs ) )

and fmt_class_exprs c ctx (cls: class_expr class_infos list) =
  list_fl cls (fun ~first ~last:_ cl ->
      let {pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes}
          =
        cl
      in
      let xargs, xbody =
        match pci_expr.pcl_attributes with
        | [] -> sugar_cl_fun c None (sub_cl ~ctx pci_expr)
        | _ -> ([], sub_cl ~ctx pci_expr)
      in
      let ty, e =
        match xbody.ast with
        | {pcl_desc= Pcl_constraint (e, t)} -> (Some t, sub_cl ~ctx e)
        | _ -> (None, xbody)
      in
      let doc, atrs = doc_atrs pci_attributes in
      fmt_if (not first) "\n@\n"
      $ Cmts.fmt c.cmts pci_loc
        @@ fmt_docstring c
             ~epi:
               ( match doc with
               | Some (_, true) -> fmt "\n@\n"
               | _ -> fmt "@\n" )
             doc
      $ hovbox 2
          ( hovbox 2
              ( str (if first then "class" else "and")
              $ fmt_if Poly.(pci_virt = Virtual) "@ virtual"
              $ fmt "@ "
              $ fmt_class_params c ctx ~epi:(fmt "@ ") pci_params
              $ str pci_name.txt
              $ ( fmt_fun_args c ~pro:(fmt "@ ") xargs
                $ opt ty (fun t ->
                      fmt "@ :@ " $ fmt_class_type c (sub_cty ~ctx t) )
                $ fmt "@ =" ) )
          $ fmt "@;" $ fmt_class_expr c e )
      $ fmt_attributes c ~pre:(fmt "@;") ~key:"@@" atrs )

and fmt_module c ?epi keyword name xargs xbody colon xmty attributes =
  let {txt= name; loc} = name in
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
    ( fmt_docstring c ~epi:(fmt "@,") doc
    $ opn_b
    $ (if Option.is_some epi_t then open_hovbox else open_hvbox) 0
    $ opn_t
    $ fmt_if_k (Option.is_some pro_t) (open_hvbox 0)
    $ ( match arg_blks with
      | (_, Some {opn; pro= Some _}) :: _ -> opn $ open_hvbox 0
      | _ -> fmt "" )
    $ hvbox 4
        ( keyword $ fmt " "
        $ Cmts.fmt c.cmts loc @@ str name
        $ list_pn arg_blks (fun ?prev:_ ({txt}, arg_mtyp) ?next ->
              ( match arg_mtyp with
              | Some {pro= None} -> fmt "@ @[<hv 2>("
              | _ -> fmt "@ (" )
              $ str txt
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
    $ Option.call ~f:epi )

and fmt_module_declaration c ctx ~rec_flag ~first pmd =
  let {pmd_name; pmd_type; pmd_attributes} = pmd in
  let keyword =
    if first then if rec_flag then str "module rec" else str "module"
    else str "and"
  in
  let xargs, xmty =
    sugar_functor_type c ~for_functor_kw:false (sub_mty ~ctx pmd_type)
  in
  let colon =
    match xmty.ast.pmty_desc with Pmty_alias _ -> false | _ -> true
  in
  fmt_module c keyword pmd_name xargs None colon (Some xmty) pmd_attributes

and fmt_module_type_declaration c ctx pmtd =
  let {pmtd_name; pmtd_type; pmtd_attributes} = pmtd in
  fmt_module c (fmt "module type") pmtd_name [] None false
    (Option.map pmtd_type ~f:(sub_mty ~ctx))
    pmtd_attributes

and fmt_open_description c {popen_lid; popen_override; popen_attributes} =
  let doc, atrs = doc_atrs popen_attributes in
  fmt_docstring c ~epi:(fmt "@,") doc
  $ fmt "open"
  $ fmt_if Poly.(popen_override = Override) "!"
  $ fmt " "
  $ fmt_longident popen_lid.txt
  $ fmt_attributes c ~pre:(fmt " ") ~key:"@@" atrs

and fmt_with_constraint c ctx = function
  | Pwith_type ({txt}, td) ->
      fmt " type "
      $ fmt_type_declaration c ctx ~fmt_name:(fmt_longident txt) td
  | Pwith_module ({txt= m1}, {txt= m2}) ->
      fmt " module " $ fmt_longident m1 $ fmt " = " $ fmt_longident m2
  | Pwith_typesubst ({txt}, td) ->
      fmt " type "
      $ fmt_type_declaration c ~eq:":=" ctx ~fmt_name:(fmt_longident txt) td
  | Pwith_modsubst ({txt= m1}, {txt= m2}) ->
      fmt " module " $ fmt_longident m1 $ fmt " := " $ fmt_longident m2

and maybe_generative c ~ctx m =
  match m with
  | {pmod_desc= Pmod_structure []; pmod_attributes= []} -> empty
  | _ -> fmt_module_expr c (sub_mod ~ctx m)

and fmt_module_expr c ({ast= m} as xmod) =
  let ctx = Mod m in
  let {pmod_desc; pmod_loc; pmod_attributes} = m in
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
          Some
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
          $ fits_breaks ")" " )"
      ; cls= close_box $ cls_e $ cls_t
      ; esp= fmt ""
      ; epi=
          Some
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
                    $ list xargs "@;<1 2>" (fun ({txt; _}, mt) ->
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
                               ( str txt
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
  | Pmod_ident {txt} ->
      let doc, atrs = doc_atrs pmod_attributes in
      { empty with
        opn= open_hvbox 2
      ; pro=
          Option.some_if
            (Cmts.has_before c.cmts pmod_loc || Option.is_some doc)
            ( Cmts.fmt_before c.cmts pmod_loc
            $ fmt_docstring c ~epi:(fmt "@,") doc )
      ; bdy= fmt_longident txt
      ; cls= close_box
      ; epi=
          Some
            ( Cmts.fmt_after c.cmts pmod_loc
            $ fmt_attributes c ~pre:(fmt " ") ~key:"@" atrs ) }
  | Pmod_structure sis ->
      let empty = List.is_empty sis in
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
      ; psp= fmt_if (not empty) "@;<1000 2>"
      ; bdy= within $ fmt_structure c ~sep:";; " ctx sis
      ; cls= close_box
      ; esp= fmt_if (not empty) "@;<1000 0>"
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
      { empty with
        pro=
          Some
            ( Cmts.fmt_before c.cmts pmod_loc
            $ fmt_docstring c ~epi:(fmt "@,") doc )
      ; bdy=
          Cmts.fmt c.cmts pmod_loc
          @@ hvbox 2
               (wrap_fits_breaks "(" ")"
                  ( fmt "val "
                  $ fmt_expression c (sub_exp ~ctx e1)
                  $ fmt "@;<1 2>: "
                  $ fmt_package_type c ctx pty ))
      ; epi=
          Some
            ( Cmts.fmt_after c.cmts pmod_loc
            $ fmt_attributes c ~pre:(fmt " ") ~key:"@" atrs ) }
  | Pmod_unpack e1 ->
      let doc, atrs = doc_atrs pmod_attributes in
      { empty with
        pro=
          Option.some_if
            (Cmts.has_before c.cmts pmod_loc || Option.is_some doc)
            ( Cmts.fmt_before c.cmts pmod_loc
            $ fmt_docstring c ~epi:(fmt "@,") doc )
      ; bdy=
          Cmts.fmt c.cmts pmod_loc
          @@ hvbox 2
               (wrap_fits_breaks "(" ")"
                  (fmt "val " $ fmt_expression c (sub_exp ~ctx e1)))
      ; epi=
          Some
            ( Cmts.fmt_after c.cmts pmod_loc
            $ fmt_attributes c ~pre:(fmt " ") ~key:"@" atrs ) }
  | Pmod_extension x1 ->
      let doc, atrs = doc_atrs pmod_attributes in
      { empty with
        pro=
          Option.some_if
            (Cmts.has_before c.cmts pmod_loc || Option.is_some doc)
            ( Cmts.fmt_before c.cmts pmod_loc
            $ fmt_docstring c ~epi:(fmt "@,") doc )
      ; bdy= Cmts.fmt c.cmts pmod_loc @@ fmt_extension c ctx "%" x1
      ; epi=
          Some
            ( Cmts.fmt_after c.cmts pmod_loc
            $ fmt_attributes c ~pre:(fmt " ") ~key:"@" atrs ) }

and fmt_use_file c ctx itms =
  list itms ";;\n@\n" (fun item -> fmt_toplevel_phrase c ctx item)

and fmt_toplevel_phrase c ctx = function
  | Ptop_def structure -> fmt_structure c ctx ~use_file:true structure
  | Ptop_dir (dir, directive_argument) ->
      str "#" $ str dir
      $
      match directive_argument with
      | Pdir_none -> fmt ""
      | Pdir_string s -> fmt " " $ str (Printf.sprintf "%S" s)
      | Pdir_int (lit, Some m) ->
          fmt " " $ str (Printf.sprintf "%s%c" lit m)
      | Pdir_int (lit, None) -> fmt " " $ str lit
      | Pdir_ident longident -> fmt " " $ fmt_longident longident
      | Pdir_bool bool -> fmt " " $ str (Bool.to_string bool)

and fmt_structure c ?(sep= "") ?use_file ctx itms =
  let grps =
    List.group itms ~break:(fun itmI itmJ ->
        let has_doc itm =
          match itm.pstr_desc with
          | Pstr_attribute atr -> Option.is_some (fst (doc_atrs [atr]))
          | Pstr_eval (_, atrs)
           |Pstr_value (_, {pvb_attributes= atrs} :: _)
           |Pstr_primitive {pval_attributes= atrs}
           |Pstr_type (_, {ptype_attributes= atrs} :: _)
           |Pstr_typext {ptyext_attributes= atrs}
           |Pstr_exception {pext_attributes= atrs}
           |Pstr_recmodule ({pmb_expr= {pmod_attributes= atrs}} :: _)
           |Pstr_modtype {pmtd_attributes= atrs}
           |Pstr_open {popen_attributes= atrs}
           |Pstr_extension (_, atrs)
           |Pstr_class_type ({pci_attributes= atrs} :: _)
           |Pstr_class ({pci_attributes= atrs} :: _) ->
              Option.is_some (fst (doc_atrs atrs))
          | Pstr_include
              {pincl_mod= {pmod_attributes= atrs1}; pincl_attributes= atrs2}
           |Pstr_module
              {pmb_attributes= atrs1; pmb_expr= {pmod_attributes= atrs2}} ->
              Option.is_some (fst (doc_atrs (List.append atrs1 atrs2)))
          | Pstr_value (_, [])
           |Pstr_type (_, [])
           |Pstr_recmodule []
           |Pstr_class_type []
           |Pstr_class [] ->
              false
        in
        let rec is_simple_mod me =
          match me.pmod_desc with
          | Pmod_apply (me1, me2) -> is_simple_mod me1 && is_simple_mod me2
          | Pmod_functor (_, _, me) -> is_simple_mod me
          | Pmod_ident _ -> true
          | _ -> false
        in
        let is_simple itm =
          match itm.pstr_desc with
          | Pstr_include {pincl_mod= me} | Pstr_module {pmb_expr= me} ->
              is_simple_mod me
          | Pstr_open _ -> true
          | _ -> false
        in
        has_doc itmI || has_doc itmJ
        || (not (is_simple itmI))
        || (not (is_simple itmJ)) )
  in
  let fmt_grp ~last:last_grp itms =
    list_fl itms (fun ~first ~last itm ->
        fmt_if (not first) "@\n"
        $ fmt_structure_item c ~sep ~last:(last && last_grp) ?use_file
            (sub_str ~ctx itm) )
  in
  hvbox 0
    (list_fl grps (fun ~first ~last grp ->
         fmt_if (not first) "\n@\n" $ fmt_grp ~last grp ))

and fmt_structure_item c ~sep ~last:last_item ?ext ?(use_file= false)
    {ctx; ast= si} =
  protect (Str si)
  @@
  let at_top =
    match ctx with
    | Top | Str {pstr_desc= Pstr_extension ((_, PStr (_ :: _ :: _)), _)} ->
        true
    | _ -> false
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
      fmt_docstring c ~epi:(fmt "") doc $ fmt_attributes c ~key:"@@@" atrs
  | Pstr_eval (exp, atrs) ->
      let doc, atrs = doc_atrs atrs in
      str sep $ fmt_docstring c doc
      $ cbox 0
          ( fmt_if (at_top && (not use_file)) ";; "
          $ fmt_expression c (sub_exp ~ctx exp) )
      $ fmt_attributes c ~pre:(fmt " ") ~key:"@@" atrs
  | Pstr_exception extn_constr ->
      hvbox 2
        (fmt_exception ~pre:(fmt "exception@ ") c (fmt ": ") ctx extn_constr)
  | Pstr_include {pincl_mod; pincl_attributes} ->
      let {opn; pro; psp; bdy; cls; esp; epi} =
        fmt_module_expr c (sub_mod ~ctx pincl_mod)
      in
      opn
      $ ( hvbox 2 (fmt "include " $ Option.call ~f:pro)
        $ psp $ bdy $ cls $ esp $ Option.call ~f:epi
        $ fmt_attributes c ~pre:(fmt " ") ~key:"@@" pincl_attributes )
  | Pstr_module binding ->
      fmt_module_binding c ctx ~rec_flag:false ~first:true binding
  | Pstr_open open_descr -> fmt_open_description c open_descr
  | Pstr_primitive vd -> fmt_value_description c ctx vd
  | Pstr_recmodule bindings ->
      hvbox 0
        (list_fl bindings (fun ~first ~last binding ->
             fmt_module_binding c ctx ~rec_flag:true ~first binding
             $ fmt_if (not last) "@,@\n" ))
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
      hvbox 0
        (list_fl bindings (fun ~first ~last binding ->
             fmt_value_binding c ~rec_flag ~first
               ?ext:(if first then ext else None)
               ctx binding
               ?epi:
                 (Option.some_if c.conf.sparse
                    (fits_breaks ~force_fit_if:last_item "" "\n"))
             $ fmt_if (not last) "\n@\n" ))
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
  let doc, atrs = doc_atrs pvb_attributes in
  let keyword =
    if first then if Poly.(rec_flag = Recursive) then "let rec" else "let"
    else "and"
  in
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
          sub_pat ~ctx:(Pat pvb_pat) pat
      | _ -> sub_pat ~ctx pvb_pat
    in
    let ({ast= body} as xbody) = sub_exp ~ctx pvb_expr in
    if not (List.is_empty xbody.ast.pexp_attributes) then
      (xpat, [], None, xbody)
    else
      let sugar_polynewtype pat body =
        let ctx = Pat pat in
        match pat.ppat_desc with
        | Ppat_constraint (pat, {ptyp_desc= Ptyp_poly (pvars, _)}) ->
            let rec sugar_polynewtype_ xpat pvars0 pvars body =
              let ctx = Exp body in
              match (pvars, body.pexp_desc) with
              | [], Pexp_constraint (exp, typ) ->
                  Some (xpat, pvars0, sub_typ ~ctx typ, sub_exp ~ctx exp)
              | {txt= pvar} :: pvars, Pexp_newtype ({txt= nvar}, exp)
                when String.equal pvar nvar ->
                  sugar_polynewtype_ xpat pvars0 pvars exp
              | _ -> None
            in
            sugar_polynewtype_ (sub_pat ~ctx pat) pvars pvars body
        | _ -> None
      in
      match sugar_polynewtype pat body with
      (* format
       *    let f: 'r 's. 'r 's t = fun (type r) -> fun (type s) -> (e : r s t)
       * as let f: type r s. r s t = e *)
      | Some (xpat, pvars, xtyp, xbody) ->
          let fmt_cstr =
            fmt "@ : type "
            $ list pvars " " (fun {txt} -> str txt)
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
            sugar_fun c (Some pat) xbody
          in
          let fmt_cstr, xbody =
            let ctx = Exp body in
            match body.pexp_desc with
            | Pexp_constraint
                ({pexp_desc= Pexp_pack _}, {ptyp_desc= Ptyp_package _}) ->
                (None, xbody)
            | Pexp_constraint (exp, typ) ->
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
  fmt_docstring c
    ~epi:(match doc with Some (_, true) -> fmt "\n@\n" | _ -> fmt "@\n")
    doc
  $ Cmts.fmt_before c.cmts pvb_loc
  $ hvbox indent
      ( open_hovbox 2
      $ ( hovbox 4
            ( str keyword
            $ fmt_extension_suffix c ext
            $ fmt_if_k (Option.is_some in_) (fmt_attributes c ~key:"@" atrs)
            $ fmt " " $ fmt_pattern c xpat
            $ fmt_fun_args c ~pro:(fmt "@ ") xargs
            $ Option.call ~f:fmt_cstr )
        $ fmt "@;<1 2>=" )
      $ fmt_body c xbody
      $ fmt_if_k (Option.is_none in_) (fmt_attributes c ~key:"@@" atrs)
      $ Cmts.fmt_after c.cmts pvb_loc
      $ (match in_ with Some in_ -> in_ indent | None -> Fn.const ())
      $ Option.call ~f:epi )

and fmt_module_binding c ?epi ~rec_flag ~first ctx pmb =
  let {pmb_name; pmb_expr; pmb_attributes} = pmb in
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
  fmt_module c ?epi keyword pmb_name xargs (Some xbody) true xmty
    pmb_attributes

(** Entry points *)

let fmt_signature s cmts c =
  let c = {source= s; cmts; conf= c} in
  fmt_signature c Top

let fmt_structure s cmts c =
  let c = {source= s; cmts; conf= c} in
  fmt_structure c Top

let fmt_use_file s cmts c =
  let c = {source= s; cmts; conf= c} in
  fmt_use_file c Top
