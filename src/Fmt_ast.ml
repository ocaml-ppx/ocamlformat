(**********************************************************************)
(*                                                                    *)
(*                            OCamlFormat                             *)
(*                                                                    *)
(*  Copyright (c) 2017-present, Facebook, Inc.  All rights reserved.  *)
(*                                                                    *)
(*  This source code is licensed under the MIT license found in the   *)
(*  LICENSE file in the root directory of this source tree.           *)
(*                                                                    *)
(**********************************************************************)

module Format = Format_

(** Format OCaml Ast *)

open Migrate_ast
open Asttypes
open Parsetree
open Ast
open Fmt

(* Debug: catch and report failures at nearest enclosing Ast.t *)

let protect =
  let first = ref true in
  fun ast pp fs ->
    try pp fs with exc ->
      ( if !first then
          let bt = Caml.Printexc.get_backtrace () in
          Format.pp_print_flush fs () ;
          Caml.Format.eprintf "@\nFAIL@\n%a@\n%s@." Ast.dump ast bt ;
          first := false ) ;
      raise exc


let rec sugar_arrow_typ ({ast= typ} as xtyp) =
  let ctx = Typ typ in
  let {ptyp_desc; ptyp_loc} = typ in
  match ptyp_desc with
  | Ptyp_arrow (l, t1, t2) ->
      Cmts.relocate ~src:ptyp_loc ~before:t1.ptyp_loc ~after:t2.ptyp_loc ;
      (l, sub_typ ~ctx t1) :: sugar_arrow_typ (sub_typ ~ctx t2)
  | _ -> [(Nolabel, xtyp)]


let rec sugar_or_pat ({ast= pat} as xpat) =
  let ctx = Pat pat in
  match pat with
  | {ppat_desc= Ppat_or (pat1, pat2); ppat_loc; ppat_attributes= []} ->
      Cmts.relocate ~src:ppat_loc ~before:pat1.ppat_loc ~after:pat2.ppat_loc ;
      sugar_or_pat (sub_pat ~ctx pat1) @ sugar_or_pat (sub_pat ~ctx pat2)
  | _ -> [xpat]


let sugar_fun pat xexp =
  let rec sugar_fun_ ({ast= exp} as xexp) =
    let ctx = Exp exp in
    let {pexp_desc; pexp_loc} = exp in
    match pexp_desc with
    | Pexp_fun (label, default, pattern, body) ->
        Cmts.relocate ~src:pexp_loc ~before:pattern.ppat_loc
          ~after:body.pexp_loc ;
        let xargs, xbody = sugar_fun_ (sub_exp ~ctx body) in
        ( (label, sub_pat ~ctx pattern, Option.map default ~f:(sub_exp ~ctx))
          :: xargs
        , xbody )
    | _ -> ([], xexp)
  in
  match pat with
  | Some {ppat_desc= Ppat_constraint _} -> ([], xexp)
  | Some {ppat_attributes} when not (List.is_empty ppat_attributes) ->
      ([], xexp)
  | _ -> sugar_fun_ xexp


let sugar_infix prec xexp =
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
            Cmts.relocate ~src ~before ~after
        | _ -> Cmts.relocate ~src ~before:e0.pexp_loc ~after ) ;
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
            | Some {ast} -> Cmts.relocate ~src ~before:ast.pexp_loc ~after
            | None -> Cmts.relocate ~src ~before:e1.pexp_loc ~after )
          | None ->
            match xop with
            | Some {ast} -> Cmts.relocate ~src ~before:ast.pexp_loc ~after
            | None -> Cmts.relocate ~src ~before:e1.pexp_loc ~after )
        | _ ->
          match xop with
          | Some {ast} -> Cmts.relocate ~src ~before:ast.pexp_loc ~after
          | None -> Cmts.relocate ~src ~before:e1.pexp_loc ~after ) ;
        (xop, [(l1, sub_exp ~ctx e1)]) :: op_args2
    | _ -> [(xop, [xexp])]
  in
  sugar_infix_ None (Nolabel, xexp)


let rec sugar_list_pat pat =
  let ctx = Pat pat in
  let {ppat_desc; ppat_loc= src} = pat in
  match ppat_desc with
  | Ppat_construct ({txt= Lident "[]"; loc}, None) -> Some ([], [src; loc])
  | Ppat_construct
      ( {txt= Lident "::"; loc}
      , Some {ppat_desc= Ppat_tuple [hd; tl]; ppat_loc; ppat_attributes= []}
      ) -> (
    match sugar_list_pat tl with
    | Some (xtl, nil_locs) ->
        Some (([src; loc; ppat_loc], sub_pat ~ctx hd) :: xtl, nil_locs)
    | None -> None )
  | _ -> None


let sugar_list_exp exp =
  let rec sugar_list_exp_ exp =
    let ctx = Exp exp in
    let {pexp_desc; pexp_loc= src} = exp in
    match pexp_desc with
    | Pexp_construct ({txt= Lident "[]"; loc}, None) -> Some ([], [src; loc])
    | Pexp_construct
        ( {txt= Lident "::"; loc}
        , Some
            {pexp_desc= Pexp_tuple [hd; tl]; pexp_loc; pexp_attributes= []} )
      -> (
      match sugar_list_exp_ tl with
      | Some (xtl, nil_locs) ->
          Some (([src; loc; pexp_loc], sub_exp ~ctx hd) :: xtl, nil_locs)
      | None -> None )
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
      match sugar_infix_cons_ (sub_exp ~ctx tl) with xtl ->
        ([l1; l2; l3], sub_exp ~ctx hd) :: xtl )
    | _ -> [([], xexp)]
  in
  sugar_infix_cons_ xexp


let rec sugar_ite ({ast= exp} as xexp) =
  let ctx = Exp exp in
  let {pexp_desc; pexp_loc} = exp in
  match pexp_desc with
  | Pexp_ifthenelse (cnd, thn, Some els) ->
      Cmts.relocate ~src:pexp_loc ~before:cnd.pexp_loc ~after:els.pexp_loc ;
      (Some (sub_exp ~ctx cnd), sub_exp ~ctx thn)
      :: sugar_ite (sub_exp ~ctx els)
  | Pexp_ifthenelse (cnd, thn, None) ->
      Cmts.relocate ~src:pexp_loc ~before:cnd.pexp_loc ~after:thn.pexp_loc ;
      [(Some (sub_exp ~ctx cnd), sub_exp ~ctx thn)]
  | _ -> [(None, xexp)]


let sugar_sequence c width xexp =
  let rec sugar_sequence_ ({ast= exp} as xexp) =
    let ctx = Exp exp in
    let {pexp_desc; pexp_loc} = exp in
    match pexp_desc with
    | Pexp_sequence (e1, e2) ->
        Cmts.relocate ~src:pexp_loc ~before:e1.pexp_loc ~after:e2.pexp_loc ;
        if Ast.exposed Ast.Let_match e1 then
          [sub_exp ~ctx e1; sub_exp ~ctx e2]
        else
          List.append
            (sugar_sequence_ (sub_exp ~ctx e1))
            (sugar_sequence_ (sub_exp ~ctx e2))
    | _ -> [xexp]
  in
  List.group (sugar_sequence_ xexp) ~break:(fun xexp1 xexp2 ->
      not (is_simple c width xexp1) || not (is_simple c width xexp2) )


let rec sugar_functor_type ({ast= mty} as xmty) =
  let ctx = Mty mty in
  match mty with
  | { pmty_desc= Pmty_functor (arg, arg_mty, body)
    ; pmty_loc
    ; pmty_attributes= [] } ->
      Cmts.relocate ~src:pmty_loc ~before:arg.loc ~after:body.pmty_loc ;
      let xargs, xbody = sugar_functor_type (sub_mty ~ctx body) in
      ((arg, Option.map arg_mty ~f:(sub_mty ~ctx)) :: xargs, xbody)
  | _ -> ([], xmty)


let rec sugar_functor ?mt ({ast= me} as xme) =
  let ctx = Mod me in
  match (me, mt) with
  | ( { pmod_desc= Pmod_functor (arg, arg_mt, body)
      ; pmod_loc
      ; pmod_attributes= [] }
    , None )
    -> (
      Cmts.relocate ~src:pmod_loc ~before:arg.loc ~after:body.pmod_loc ;
      let xarg_mt = Option.map arg_mt ~f:(sub_mty ~ctx) in
      let ctx = Mod body in
      match body with
      | { pmod_desc= Pmod_constraint (body_me, body_mt)
        ; pmod_loc
        ; pmod_attributes= [] } ->
          Cmts.relocate ~src:pmod_loc ~before:body_me.pmod_loc
            ~after:body_mt.pmty_loc ;
          let xbody_mt0 = sub_mty ~ctx body_mt in
          let xargs, xbody_me, xbody_mt1 =
            sugar_functor ~mt:xbody_mt0 (sub_mod ~ctx body_me)
          in
          ((arg, xarg_mt) :: xargs, xbody_me, xbody_mt1)
      | _ ->
          let xargs, xbody_me, xbody_mt =
            sugar_functor (sub_mod ~ctx body)
          in
          ((arg, xarg_mt) :: xargs, xbody_me, xbody_mt) )
  | _ -> ([], xme, mt)


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


let fmt_constant const =
  match const with
  | Pconst_integer (lit, suf) | Pconst_float (lit, suf) ->
      str lit $ opt suf char
  | Pconst_char c -> wrap "'" "'" @@ str (Char.escaped c)
  | Pconst_string (s, delim) ->
      let pre, mid, suf =
        match delim with
        | None -> ("\"", String.escaped s, "\"")
        | Some delim -> ("{" ^ delim ^ "|", s, "|" ^ delim ^ "}")
      in
      str pre $ str mid $ str suf


let fmt_variance = function
  | Covariant -> fmt "+"
  | Contravariant -> fmt "-"
  | Invariant -> fmt ""


let doc_atrs atrs =
  let doc, rev_atrs =
    List.fold atrs ~init:(None, []) ~f:(fun (doc, rev_atrs) atr ->
        match (doc, atr) with
        | ( None
          , ( {txt= "ocaml.doc" | "ocaml.text" as txt}
            , PStr
                [ { pstr_desc=
                      Pstr_eval
                        ( { pexp_desc=
                              Pexp_constant Pconst_string (doc, None)
                          ; pexp_loc= loc
                          ; pexp_attributes= [] }
                        , [] ) } ] ) ) ->
            (Some ({txt= doc; loc}, String.equal "ocaml.text" txt), rev_atrs)
        | _ -> (doc, atr :: rev_atrs) )
  in
  (doc, List.rev rev_atrs)


let fmt_docstring ?pro ?epi doc =
  opt doc (fun (({txt; loc} as doc), floating) ->
      let epi =
        match epi with
        | Some _ -> epi
        | None when floating -> Some (fmt "@,")
        | None -> None
      in
      fmt_if_k
        (not (Cmts.doc_is_dup doc))
        ( Cmts.fmt loc
        @@ vbox_if (Option.is_none pro) 0
             ( Option.call ~f:pro $ fmt "(**" $ str txt $ fmt "*)"
             $ Option.call ~f:epi ) ) )


let rec fmt_attribute c pre = function
  | ( {txt= "ocaml.doc" | "ocaml.text" as txt}
    , PStr
        [ { pstr_desc=
              Pstr_eval
                ( { pexp_desc= Pexp_constant Pconst_string (doc, None)
                  ; pexp_attributes= [] }
                , [] ) } ] ) ->
      fmt_or (String.equal txt "ocaml.text") "@ " " " $ fmt "(**" $ str doc
      $ fmt "*)"
  | {txt; loc}, pld ->
      let nullary = match pld with PStr [] -> true | _ -> false in
      Cmts.fmt loc
      @@ hvbox 2
           (wrap "[" "]"
              ( str pre $ str txt $ fmt_if (not nullary) "@ "
              $ fmt_payload c (Pld pld) pld ))


and fmt_extension c ctx key ({txt}, pld) =
  wrap "[" "]" (str key $ str txt $ fmt "@ " $ fmt_payload c ctx pld)


and fmt_attributes c pre ~key attrs suf =
  list_fl attrs (fun ~first ~last atr ->
      fmt_or_k first pre (fmt "@ ") $ fmt_attribute c key atr
      $ fmt_if_k last suf )


and fmt_payload c ctx pld =
  protect (Pld pld)
  @@
  match pld with
  | PStr mex -> fmt_structure c ctx mex
  | PSig mty -> fmt ": " $ fmt_signature c ctx mty
  | PTyp typ -> fmt ": " $ fmt_core_type c (sub_typ ~ctx typ)
  | PPat (pat, exp) ->
      fmt "? " $ fmt_pattern c (sub_pat ~ctx pat)
      $ opt exp (fun exp ->
            fmt " when " $ fmt_expression c (sub_exp ~ctx exp) )


and fmt_core_type c ?(box= true) ({ast= typ} as xtyp) =
  protect (Typ typ)
  @@
  let {ptyp_desc; ptyp_loc} = typ in
  Cmts.fmt ptyp_loc
  @@
  let parens = parenze_typ xtyp in
  hvbox_if box 0
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
      let xt1N = sugar_arrow_typ xtyp in
      hovbox_if box 0
        (list xt1N "@ -> " (fun (lI, xtI) ->
             hvbox 0 (arg_label lI $ fmt_core_type c xtI) ))
  | Ptyp_constr ({txt; loc}, []) -> Cmts.fmt loc @@ fmt_longident txt
  | Ptyp_constr ({txt; loc}, [t1]) ->
      Cmts.fmt loc @@ fmt_core_type c (sub_typ ~ctx t1) $ fmt "@ "
      $ fmt_longident txt
  | Ptyp_constr ({txt; loc}, t1N) ->
      Cmts.fmt loc
      @@ wrap_fits_breaks "(" ")"
           (list t1N "@,, " (sub_typ ~ctx >> fmt_core_type c))
      $ fmt "@ " $ fmt_longident txt
  | Ptyp_extension ext -> hvbox 2 (fmt_extension c ctx "%" ext)
  | Ptyp_package pty -> hvbox 0 (fmt "module@ " $ fmt_package_type c ctx pty)
  | Ptyp_poly (a1N, t) ->
      hovbox_if box 0
        ( list a1N "@ " (fun {txt} -> fmt "'" $ str txt) $ fmt ".@ "
        $ fmt_core_type c ~box:false (sub_typ ~ctx t) )
  | Ptyp_tuple typs ->
      hvbox 0
        (wrap_fits_breaks_if parens "(" ")"
           (list typs "@ * " (sub_typ ~ctx >> fmt_core_type c)))
  | Ptyp_var s -> fmt "'" $ str s
  | Ptyp_variant (rfs, flag, lbls) ->
      let row_fields rfs = list rfs "@ | " (fmt_row_field c ctx) in
      hvbox 0
        ( fits_breaks "[" "["
        $ ( match (flag, lbls) with
          | Closed, None -> fits_breaks "" " " $ row_fields rfs
          | Open, None -> fmt "> " $ row_fields rfs
          | Closed, Some [] -> fmt "< " $ row_fields rfs
          | Closed, Some ls ->
              fmt "< " $ row_fields rfs $ fmt " > "
              $ list ls "@ " (fmt "`" >$ str)
          | Open, Some _ -> impossible "not produced by parser" )
        $ fits_breaks "]" "@ ]" )
  | Ptyp_object _ | Ptyp_class _ ->
      internal_error "classes not implemented" []


and fmt_package_type c ctx ({txt}, cnstrs) =
  hvbox 0
    ( fmt_longident txt
    $ list_fl cnstrs (fun ~first ~last:_ ({txt}, typ) ->
          fmt_or first " with type " " and " $ fmt_longident txt $ fmt " = "
          $ fmt_core_type c (sub_typ ~ctx typ) ) )


and fmt_row_field c ctx = function
  | Rtag ({txt; loc}, atrs, const, typs) ->
      let doc, atrs = doc_atrs atrs in
      hvbox 0
        ( Cmts.fmt loc @@ (fmt "`" $ str txt)
        $ fmt_attributes c (fmt " ") ~key:"@" atrs (fmt "")
        $ fmt_if (not (const && List.is_empty typs)) " of "
        $ fmt_if (const && not (List.is_empty typs)) " & "
        $ list typs "@ & " (sub_typ ~ctx >> fmt_core_type c)
        $ fmt_docstring ~pro:(fmt "@;<2 0>") doc )
  | Rinherit typ -> fmt_core_type c (sub_typ ~ctx typ)


and fmt_pattern (c: Conf.t) ?pro ?parens ({ctx= ctx0; ast= pat} as xpat) =
  protect (Pat pat)
  @@
  let ctx = Pat pat in
  let {ppat_desc; ppat_loc} = pat in
  let parens = match parens with Some b -> b | None -> parenze_pat xpat in
  ( match ppat_desc with
  | Ppat_or _ -> Fn.id
  | Ppat_construct ({txt; loc}, _) when Poly.(txt <> Longident.Lident "::") ->
      fun k -> Cmts.fmt ppat_loc @@ Cmts.fmt loc @@ (Option.call ~f:pro $ k)
  | _ -> fun k -> Cmts.fmt ppat_loc @@ (Option.call ~f:pro $ k) )
  @@
  match ppat_desc with
  | Ppat_any -> fmt "_"
  | Ppat_var {txt; loc} ->
      Cmts.fmt loc @@ wrap_if (is_symbol_id txt) "( " " )" (str txt)
  | Ppat_alias (pat, {txt}) ->
      hovbox 0
        (wrap_fits_breaks_if parens "(" ")"
           (fmt_pattern c (sub_pat ~ctx pat) $ fmt "@ as@ " $ str txt))
  | Ppat_constant c -> fmt_constant c
  | Ppat_interval (l, u) -> fmt_constant l $ fmt ".." $ fmt_constant u
  | Ppat_tuple pats ->
      hvbox 0
        (wrap_if_breaks "( " "@ )"
           (wrap_if_fits_and parens "(" ")"
              (list pats "@,, " (sub_pat ~ctx >> fmt_pattern c))))
  | Ppat_construct ({txt}, None) -> fmt_longident txt
  | Ppat_construct
      ( {txt= Lident "::"}
      , Some {ppat_desc= Ppat_tuple pats; ppat_attributes= []} ) -> (
    match sugar_list_pat pat with
    | Some (loc_xpats, nil_locs) ->
        hvbox 0
          (wrap_fits_breaks "[" "]"
             ( Cmts.fmt_list nil_locs
             @@ list loc_xpats "@,; " (fun (locs, xpat) ->
                    Cmts.fmt_list locs @@ fmt_pattern c xpat ) ))
    | None ->
        hvbox 0
          (wrap_if_fits_and parens "(" ")"
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
      let fmt_field ({txt}, pat) =
        let {ppat_desc; ppat_loc} = pat in
        Cmts.fmt ppat_loc
        @@
        match ppat_desc with
        | Ppat_var {txt= txt'} when String.equal (Longident.last txt) txt' ->
            cbox 2 (fmt_longident txt)
        | _ ->
            cbox 2
              ( fmt_longident txt $ fmt "=@ "
              $ cbox 0 (fmt_pattern c (sub_pat ~ctx pat)) )
      in
      hvbox 0
        (wrap_fits_breaks "{" "}"
           ( list flds "@,; " fmt_field
           $ fmt_if Poly.(closed_flag = Open) "; _" ))
  | Ppat_array pats ->
      hvbox 0
        (wrap_fits_breaks "[|" "|]"
           (list pats "@,; " (sub_pat ~ctx >> fmt_pattern c)))
  | Ppat_or _ ->
      let nested =
        match ctx0 with
        | Pat {ppat_desc= Ppat_or _} -> true
        | Pat _ -> false
        | _ -> true
      in
      let xpats = sugar_or_pat xpat in
      let pro0 =
        Option.call ~f:pro
        $ fits_breaks
            (if parens then "(" else "")
            (if nested then "" else "( ")
      in
      let proI =
        match ctx0 with
        | Exp {pexp_desc= Pexp_function _ | Pexp_match _ | Pexp_try _}
          when not c.sparse ->
            fits_breaks "| " " |"
        | _ -> fmt "| "
      in
      hovbox_if (not nested) 0
        ( list_fl xpats (fun ~first ~last xpat ->
              fmt_pattern c ~pro:(if first then pro0 else proI) xpat
              $ fmt_if (not last) "@ " )
        $ fits_breaks
            (if parens then ")" else "")
            (if nested then "" else "@;<1 2>)") )
  | Ppat_constraint
      ( {ppat_desc= Ppat_unpack {txt}; ppat_attributes= []}
      , {ptyp_desc= Ptyp_package pty; ptyp_attributes= []} ) ->
      wrap_if parens "(" ")"
        (fmt "module " $ str txt $ fmt ":@ " $ fmt_package_type c ctx pty)
  | Ppat_constraint (pat, typ) ->
      hvbox 2
        (wrap_if parens "(" ")"
           ( fmt_pattern c (sub_pat ~ctx pat) $ fmt ":@ "
           $ fmt_core_type c (sub_typ ~ctx typ) ))
  | Ppat_type {txt} -> fmt "#" $ fmt_longident txt
  | Ppat_lazy pat ->
      cbox 2
        (wrap_if parens "(" ")"
           (fmt "lazy@ " $ fmt_pattern c (sub_pat ~ctx pat)))
  | Ppat_unpack {txt} -> fmt "module@ " $ str txt
  | Ppat_exception pat ->
      cbox 2
        (wrap_if parens "(" ")"
           (fmt "exception@ " $ fmt_pattern c (sub_pat ~ctx pat)))
  | Ppat_extension ext -> hvbox 2 (fmt_extension c ctx "%" ext)
  | Ppat_open ({txt}, pat) ->
      cbox 0
        ( fmt_longident txt $ fmt ".(" $ fmt_pattern c (sub_pat ~ctx pat)
        $ fmt ")" )


and fmt_fun_args c args =
  let fmt_fun_arg = function
    | Nolabel, xpat, None -> fmt_pattern c xpat
    | ( Labelled l
      , ( { ast=
              { ppat_desc=
                  ( Ppat_var {txt; loc}
                  | Ppat_constraint
                      ( {ppat_desc= Ppat_var {txt; loc}; ppat_attributes= []}
                      , _ ) )
              ; ppat_attributes= [] } } as xpat )
      , None )
      when String.equal l txt ->
        Cmts.fmt loc @@ cbox 0 (fmt "~" $ fmt_pattern c xpat)
    | Labelled l, xpat, None ->
        cbox 0 (fmt "~" $ str l $ fmt ":" $ fmt_pattern c xpat)
    | ( Optional l
      , {ast= {ppat_desc= Ppat_var {txt; loc}; ppat_attributes= []}}
      , None )
      when String.equal l txt ->
        Cmts.fmt loc @@ cbox 0 (fmt "?" $ str l)
    | Optional l, xpat, None ->
        cbox 0 (fmt "?" $ str l $ fmt ":" $ fmt_pattern c xpat)
    | ( Optional l
      , {ast= {ppat_desc= Ppat_var {txt; loc}; ppat_attributes= []}}
      , Some xexp )
      when String.equal l txt ->
        Cmts.fmt loc
        @@ cbox 0
             (fmt "?(" $ str l $ fmt "= " $ fmt_expression c xexp $ fmt ")")
    | Optional l, xpat, Some xexp ->
        cbox 0
          ( fmt "?" $ str l $ fmt ":(" $ fmt_pattern c xpat $ fmt " = "
          $ fmt_expression c xexp $ fmt ")" )
    | (Labelled _ | Nolabel), _, Some _ ->
        impossible "not accepted by parser"
  in
  fmt_if_k (not (List.is_empty args)) (list args "@ " fmt_fun_arg $ fmt "@ ")


and fmt_expression c ?(box= true) ?eol ?parens ({ast= exp} as xexp) =
  protect (Exp exp)
  @@
  let {pexp_desc; pexp_loc; pexp_attributes} = exp in
  let parens = match parens with Some b -> b | None -> parenze_exp xexp in
  let fmt_label lbl sep =
    match lbl with
    | Nolabel -> fmt ""
    | Labelled l -> fmt "~" $ str l $ fmt sep
    | Optional l -> fmt "?" $ str l $ fmt sep
  in
  let fmt_label_arg ?(box= box) ?eol ?parens (lbl, ({ast= arg} as xarg)) =
    match (lbl, arg.pexp_desc) with
    | (Labelled l | Optional l), Pexp_ident {txt= Lident i; loc}
      when String.equal l i ->
        Cmts.fmt loc @@ Cmts.fmt ?eol arg.pexp_loc @@ fmt_label lbl ""
    | _ ->
        hvbox_if box 2
          (fmt_label lbl ":@," $ fmt_expression c ~box ?eol ?parens xarg)
  in
  let ctx = Exp exp in
  let width xe =
    String.length
      (Format.asprintf "%t" (Cmts.preserve (fun () -> fmt_expression c xe)))
  in
  let fmt_args_grouped e0 a1N =
    list_fl
      (List.group ((Nolabel, e0) :: a1N) ~break:(fun (_, a1) (_, a2) ->
           not (is_simple c width (sub_exp ~ctx a1))
           || not (is_simple c width (sub_exp ~ctx a2)) ))
      (fun ~first:first_grp ~last:last_grp args ->
        list_pn args (fun ?prev (lbl, arg) ?next ->
            let {ast} as xarg = sub_exp ~ctx arg in
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
              consecutive_prefix_ops
              || (not (is_prefix ast) || Option.is_none next && not last_grp)
                 && (not last_grp || Option.is_some next)
            in
            openbox
            $ hovbox 2
                (fmt_label_arg
                   ?box:
                     ( match ast.pexp_desc with
                     | Pexp_fun _ | Pexp_function _ -> Some false
                     | _ -> None )
                   (lbl, xarg))
            $ fmt_if_k (Option.is_none next) close_box $ fmt_if spc "@ " ) )
  in
  let fmt_cmts = Cmts.fmt ?eol pexp_loc in
  let fmt_atrs =
    fmt_attributes c (fmt " ") ~key:"@" pexp_attributes (fmt "")
  in
  hvbox_if box 0 @@ fmt_cmts
  @@
  match pexp_desc with
  | Pexp_apply (_, []) -> impossible "not produced by parser"
  | Pexp_apply
      ( { pexp_desc= Pexp_ident {txt= Ldot (Lident "Array", "get")}
        ; pexp_attributes= [] }
      , [(Nolabel, s); (Nolabel, i)] ) ->
      fmt_expression c (sub_exp ~ctx s) $ fmt ".("
      $ fmt_expression c (sub_exp ~ctx i) $ fmt ")"
  | Pexp_apply
      ( { pexp_desc= Pexp_ident {txt= Ldot (Lident "String", "get")}
        ; pexp_attributes= [] }
      , [(Nolabel, s); (Nolabel, i)] ) ->
      fmt_expression c (sub_exp ~ctx s) $ fmt ".["
      $ fmt_expression c (sub_exp ~ctx i) $ fmt "]"
  | Pexp_apply
      ( { pexp_desc= Pexp_ident {txt= Ldot (Lident "Array", "set")}
        ; pexp_attributes= [] }
      , [(Nolabel, s); (Nolabel, i); (Nolabel, e)] ) ->
      fmt_expression c (sub_exp ~ctx s) $ fmt ".("
      $ fmt_expression c (sub_exp ~ctx i) $ fmt ")@ <- "
      $ fmt_expression c (sub_exp ~ctx e)
  | Pexp_apply
      ( { pexp_desc= Pexp_ident {txt= Ldot (Lident "String", "set")}
        ; pexp_attributes= [] }
      , [(Nolabel, s); (Nolabel, i); (Nolabel, e)] ) ->
      fmt_expression c (sub_exp ~ctx s) $ fmt ".["
      $ fmt_expression c (sub_exp ~ctx i) $ fmt "]@ <- "
      $ fmt_expression c (sub_exp ~ctx e)
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
        ( Cmts.fmt pexp_loc
        @@ hvbox 2 (str "-" $ spc $ fmt_expression c (sub_exp ~ctx e1)) )
      $ fmt_atrs
  | Pexp_apply
      ( {pexp_desc= Pexp_ident {txt= Lident id}}
      , (Nolabel, _) :: (Nolabel, _) :: _ )
    when is_infix_id id ->
      let op_args = sugar_infix (prec_ast (Exp exp)) xexp in
      let fmt_arg ~last_op ~first:_ ~last lbl_xarg =
        let _, ({ast= arg} as xarg) = lbl_xarg in
        let parens =
          not last_op && exposed Ast.Non_apply arg || parenze_exp xarg
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
      let fmt_op_args ~first ~last (xop, xargs) =
        let fmt_xop = function
          | Some op -> fmt_expression c op
          | None -> fmt ""
        in
        let is_not_indented exp =
          match exp.pexp_desc with
          | Pexp_ifthenelse _
           |Pexp_let _
           |Pexp_letexception _
           |Pexp_letmodule _
           |Pexp_match _
           |Pexp_newtype _
           |Pexp_open _
           |Pexp_sequence _
           |Pexp_try _ ->
              true
          | _ -> false
        in
        let final_break =
          match xargs with
          | (_, {ast= a0}) :: _ -> last && is_not_indented a0
          | _ -> false
        in
        (* side effects of Cmts.fmt_before first is important *)
        let fmt_xop_cmts =
          match xop with
          | Some {ast= {pexp_loc}} -> Cmts.fmt_before pexp_loc
          | None -> fmt ""
        in
        fmt_xop_cmts
        $ hvbox 0
            ( fmt_xop xop
            $ (if final_break then fmt "@ " else fmt_if (not first) " ")
            $ hovbox_if (not last) 2 (fmt_args ~last_op:last xargs)
            $ fmt_if_k (not last)
                (break 0 (if parens && first then -2 else 0)) )
        $ fmt_if_k (not last) (break_unless_newline 1 0)
      in
      hovbox_if (box || parens) 0
        ( wrap_fits_breaks_if parens "(" ")" (list_fl op_args fmt_op_args)
        $ fmt_atrs )
  | Pexp_apply (e0, a1N) when is_infix e0 ->
      hvbox 2
        ( wrap_fits_breaks_if parens "(" ")" (fmt_args_grouped e0 a1N)
        $ fmt_atrs )
  | Pexp_apply (e0, e1N1) -> (
    match List.rev e1N1 with
    | (lbl, ({pexp_desc= Pexp_fun _; pexp_loc} as eN1)) :: rev_e1N ->
        let e1N = List.rev rev_e1N in
        (* side effects of Cmts.fmt before sugar_fun is important *)
        let fmt_cmts = Cmts.fmt pexp_loc in
        let xargs, xbody = sugar_fun None (sub_exp ~ctx eN1) in
        hvbox 0
          ( wrap_if parens "(" ")"
              (hovbox 0
                 ( hovbox 2
                     ( fmt_args_grouped e0 e1N $ fmt "@ "
                     $ fmt_label lbl ":@,"
                     $ fmt_cmts
                       @@ hvbox 0
                            (fmt "(fun " $ fmt_fun_args c xargs $ fmt "->")
                     )
                 $ fmt "@;<1 4>"
                 $ fmt_expression c
                     ?box:
                       ( match xbody.ast.pexp_desc with
                       | Pexp_fun _ | Pexp_function _ -> Some false
                       | _ -> None )
                     xbody
                 $ fits_breaks ")" "@ )" ))
          $ fmt_atrs )
    | ( lbl
      , ({pexp_desc= Pexp_function [{pc_lhs; pc_guard= None; pc_rhs}]} as eN)
      )
      :: rev_e1N ->
        let e1N = List.rev rev_e1N in
        let ctx = Exp eN in
        hvbox 2
          ( wrap_if parens "(" ")"
              (hovbox 4
                 ( fmt_args_grouped e0 e1N $ fmt "@ " $ fmt_label lbl ":@,"
                 $ fmt "(function"
                 $ fmt_attributes c (fmt " ") ~key:"@" eN.pexp_attributes
                     (fmt "")
                 $ fmt "@ "
                 $ hvbox 0
                     ( fmt_pattern c ~pro:(if_newline "| ")
                         (sub_pat ~ctx pc_lhs)
                     $ fmt "@ ->" )
                 $ fmt "@ "
                 $ cbox 0 (fmt_expression c (sub_exp ~ctx pc_rhs))
                 $ fits_breaks ")" " )" ))
          $ fmt_atrs )
    | (lbl, ({pexp_desc= Pexp_function cs} as eN)) :: rev_e1N ->
        let e1N = List.rev rev_e1N in
        let ctx'' = Exp eN in
        hvbox 2
          ( wrap_if parens "(" ")"
              ( hovbox 2
                  ( fmt_args_grouped e0 e1N $ fmt "@ " $ fmt_label lbl ":@,"
                  $ fmt "(function"
                  $ fmt_attributes c (fmt " ") ~key:"@" eN.pexp_attributes
                      (fmt "") )
              $ fmt "@ " $ fmt_cases c ctx'' cs $ fits_breaks ")" " )" )
          $ fmt_atrs )
    | _ ->
        wrap_if parens "(" ")"
          (hvbox 2 (fmt_args_grouped e0 e1N1) $ fmt_atrs) )
  | Pexp_array e1N ->
      hvbox 0
        ( wrap_fits_breaks "[|" "|]"
            (list e1N "@,; " (sub_exp ~ctx >> fmt_expression c))
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
      wrap_if parens "(" ")" (fmt_constant const) $ fmt_atrs
  | Pexp_constraint
      ( {pexp_desc= Pexp_pack me; pexp_attributes= []}
      , {ptyp_desc= Ptyp_package pty; ptyp_attributes= []} ) ->
      let {opn; pro; psp; bdy; cls; esp; epi} =
        fmt_module_expr c (sub_mod ~ctx me)
      in
      opn
      $ wrap_fits_breaks "(" ")"
          ( fmt "module " $ Option.call ~f:pro $ psp $ bdy $ cls $ esp
          $ Option.call ~f:epi $ fmt "@ : " $ fmt_package_type c ctx pty )
      $ fmt_atrs
  | Pexp_constraint (e, t) ->
      wrap_fits_breaks "(" ")"
        ( fmt_expression c (sub_exp ~ctx e) $ fmt "@ : "
        $ fmt_core_type c (sub_typ ~ctx t) )
      $ fmt_atrs
  | Pexp_construct
      ( {txt= Lident "::"}
      , Some {pexp_desc= Pexp_tuple [_; _]; pexp_attributes= []} ) -> (
    match sugar_list_exp exp with
    | Some (loc_xes, nil_locs) ->
        hvbox 0
          (wrap_fits_breaks "[" "]"
             ( Cmts.fmt_list nil_locs
             @@ list loc_xes "@,; " (fun (locs, xexp) ->
                    Cmts.fmt_list ~eol:(fmt "@;<2 2>") locs
                    @@ fmt_expression c xexp ) ))
    | None ->
        let loc_args = sugar_infix_cons xexp in
        let fmt_arg ~last_op ({ast= arg} as xarg) =
          let parens =
            not last_op && exposed Ast.Non_apply arg || parenze_exp xarg
          in
          fmt_label_arg
            ?box:
              ( match arg.pexp_desc with
              | Pexp_fun _ | Pexp_function _ -> Some false
              | _ -> None )
            ~parens (Nolabel, xarg)
        in
        let fmt_loc_args ~first ~last (locs, xarg) =
          let is_not_indented exp =
            match exp.pexp_desc with
            | Pexp_ifthenelse _
             |Pexp_let _
             |Pexp_letexception _
             |Pexp_letmodule _
             |Pexp_match _
             |Pexp_newtype _
             |Pexp_open _
             |Pexp_sequence _
             |Pexp_try _ ->
                true
            | _ -> false
          in
          let final_break = last && is_not_indented xarg.ast in
          list locs "" Cmts.fmt_before
          $ hvbox 0
              ( fmt_if (not first) "::"
              $ (if final_break then fmt "@ " else fmt_if (not first) " ")
              $ hovbox_if (not last) 2 (fmt_arg ~last_op:last xarg)
              $ fmt_if_k (not last)
                  (break 0 (if parens && first then -2 else 0)) )
          $ fmt_if_k (not last) (break_unless_newline 1 0)
        in
        hvbox 0
          ( wrap_fits_breaks_if parens "(" ")"
              (list_fl loc_args fmt_loc_args)
          $ fmt_atrs ) )
  | Pexp_construct ({txt; loc}, args) ->
      Cmts.fmt loc
      @@ wrap_if parens "(" ")"
           (hvbox 2
              ( fmt_longident txt
              $ opt args (fun arg ->
                    fmt "@ " $ fmt_expression c (sub_exp ~ctx arg) ) ))
      $ fmt_atrs
  | Pexp_variant (s, arg) ->
      hvbox 2
        ( wrap_if parens "(" ")"
            ( fmt "`" $ str s
            $ opt arg (fmt "@ " >$ (sub_exp ~ctx >> fmt_expression c)) )
        $ fmt_atrs )
  | Pexp_field (exp, {txt}) ->
      hvbox 2
        ( wrap_if parens "(" ")"
            ( fmt_expression c (sub_exp ~ctx exp) $ fmt "@,."
            $ fmt_longident txt )
        $ fmt_atrs )
  | Pexp_fun _ ->
      let xargs, xbody = sugar_fun None xexp in
      hvbox_if box 2
        ( wrap_if parens "(" ")"
            ( hovbox 2
                (hovbox 4 (fmt "fun " $ fmt_fun_args c xargs) $ fmt "->")
            $ fmt "@ " $ fmt_expression c xbody )
        $ fmt_atrs )
  | Pexp_function cs ->
      wrap_if parens "(" ")"
        ( hvbox 2
            ( fmt "function"
            $ fmt_attributes c (fmt "") ~key:"@" pexp_attributes (fmt "") )
        $ fmt "@;<1 2>" $ hvbox 0 (fmt_cases c ctx cs) )
  | Pexp_ident {txt; loc} ->
      let wrap =
        if is_symbol exp then wrap_if parens "( " " )"
        else wrap_if parens "(" ")"
      in
      Cmts.fmt loc @@ wrap (fmt_longident txt $ fmt_atrs)
  | Pexp_ifthenelse _ ->
      let cnd_exps = sugar_ite xexp in
      hvbox 0
        ( wrap_fits_breaks_if parens "(" ")"
            (list_fl cnd_exps (fun ~first ~last (xcnd, xbch) ->
                 let parens = parenze_exp xbch in
                 hovbox 0
                   ( hovbox 2
                       ( ( match xcnd with
                         | Some xcnd ->
                             hvbox 0
                               ( hvbox 2
                                   ( fmt_if (not first) "else " $ fmt "if "
                                   $ fmt_expression c xcnd )
                               $ fmt "@ then" )
                         | None -> fmt "else" )
                       $ fmt_if parens " (" $ fmt "@ "
                       $ fmt_expression c ~box:false ~parens:false xbch )
                   $ fmt_if parens " )" )
                 $ fmt_if (not last) "@ " ))
        $ fmt_atrs )
  | Pexp_let (rec_flag, bindings, body) ->
      wrap_if
        (parens || not (List.is_empty pexp_attributes))
        "(" ")"
        (vbox 0
           ( hvbox 0
               (list_fl bindings (fun ~first ~last binding ->
                    fmt_value_binding c ~rec_flag ~first ctx binding
                      ~in_:(fmt_if last "@;<1 -2>in")
                    $ fmt_if (not last) "@ " ))
           $ fmt "@;<1000 0>"
           $ hvbox 0 (fmt_expression c (sub_exp ~ctx body)) ))
      $ fmt_atrs
  | Pexp_letexception (ext_cstr, exp) ->
      hvbox 0
        ( hvbox 0
            ( fmt_extension_constructor ~pre:(fmt "let exception@ ") c ": "
                ctx ext_cstr
            $ fmt "@ in" )
        $ fmt "@ " $ fmt_expression c (sub_exp ~ctx exp) $ fmt_atrs )
  | Pexp_letmodule ({txt; loc}, pmod, exp) ->
      let {pmod_desc; pmod_attributes} = pmod in
      let keyword = "let module" in
      let me, mt =
        match pmod_desc with
        | Pmod_constraint (me, mt) -> (me, Some (sub_mty ~ctx mt))
        | _ -> (pmod, None)
      in
      let xargs, xbody, xmty = sugar_functor ?mt (sub_mod ~ctx me) in
      Cmts.fmt loc
      @@ hvbox 0
           ( hvbox 2
               ( fmt_module c keyword txt xargs (Some xbody) true xmty
                   (List.append pmod_attributes me.pmod_attributes)
               $ fmt " in" )
           $ fmt "@ " $ fmt_expression c (sub_exp ~ctx exp) $ fmt_atrs )
  | Pexp_open (flag, {txt}, e0) ->
      let override = Poly.(flag = Override) in
      let force_fit_if =
        match xexp.ctx with
        | Exp {pexp_desc= Pexp_apply _ | Pexp_construct _} -> not override
        | _ -> false
      in
      let fits_breaks = fits_breaks ~force_fit_if ~force_break_if:override
      and fits_breaks_if =
        fits_breaks_if ~force_fit_if ~force_break_if:override
      in
      let opn, cls =
        match e0.pexp_desc with
        | Pexp_array _ | Pexp_constraint _ | Pexp_record _ | Pexp_tuple _ ->
            (".", "")
        | _ -> (".(", ")")
      in
      hvbox 0
        ( fits_breaks_if parens "" "("
        $ fits_breaks "" (if override then "let open! " else "let open ")
        $ fmt_longident txt $ fits_breaks opn " in"
        $ fmt_or_k force_fit_if (fmt "@;<0 2>") (fits_breaks "" "@ ")
        $ fmt_expression c (sub_exp ~ctx e0) $ fits_breaks cls ""
        $ fits_breaks_if parens "" ")" $ fmt_atrs )
  | Pexp_match (e0, cs) | Pexp_try (e0, cs) -> (
      let keyword =
        match exp.pexp_desc with
        | Pexp_match _ -> "match"
        | Pexp_try _ -> "try"
        | _ -> impossible "previous match"
      in
      match cs with
      | [] | _ :: _ :: _ | [{pc_lhs= {ppat_desc= Ppat_or _}}] ->
          hvbox 0
            (wrap_fits_breaks_if parens "(" ")"
               ( hvbox 0
                   ( str keyword
                   $ fmt_attributes c (fmt "") ~key:"@" pexp_attributes
                       (fmt "")
                   $ fmt "@;<1 2>" $ fmt_expression c (sub_exp ~ctx e0)
                   $ fmt "@ with" )
               $ fmt "@ " $ fmt_cases c ctx cs ))
      | [{pc_lhs; pc_guard; pc_rhs}] ->
          wrap_fits_breaks_if parens "(" ")"
            (hovbox 2
               ( hvbox 0
                   ( str keyword
                   $ fmt_attributes c (fmt "") ~key:"@" pexp_attributes
                       (fmt "")
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
               $ fmt "@ " $ cbox 0 (fmt_expression c (sub_exp ~ctx pc_rhs))
               )) )
  | Pexp_newtype ({txt}, exp) ->
      hvbox 0
        ( fmt "fun (type " $ str txt $ fmt ") ->@ "
        $ fmt_expression c (sub_exp ~ctx exp) $ fmt_atrs )
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
      let field_alias (li1: Longident.t) (li2: Longident.t) =
        match (li1, li2) with
        | Ldot (_, x), Lident y -> String.equal x y
        | _ -> Poly.equal li1 li2
      in
      let fmt_field ({txt; loc}, f) =
        Cmts.fmt loc
        @@
        match f.pexp_desc with
        | Pexp_ident {txt= txt'; loc} when field_alias txt txt' ->
            Cmts.fmt loc @@ cbox 2 (fmt_longident txt)
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
  | Pexp_sequence _ ->
      hvbox 0
        ( wrap_if parens "(" ")"
            (list (sugar_sequence c width xexp) " ;@;<1000 0>" (fun grp ->
                 list grp " ;@ " (fmt_expression c) ))
        $ fmt_atrs )
  | Pexp_setfield (e1, {txt}, e2) ->
      hvbox 0
        ( wrap_fits_breaks_if parens "(" ")"
            ( fmt_expression c (sub_exp ~ctx e1) $ fmt "."
            $ fmt_longident txt $ fmt "@ <- "
            $ fmt_expression c (sub_exp ~ctx e2) )
        $ fmt_atrs )
  | Pexp_tuple es ->
      hvbox 0
        ( wrap_fits_breaks "(" ")"
            (list es "@,, " (sub_exp ~ctx >> fmt_expression c))
        $ fmt_atrs )
  | Pexp_lazy e ->
      hvbox 2
        ( wrap_fits_breaks_if parens "(" ")"
            (fmt "lazy@ " $ fmt_expression c (sub_exp ~ctx e))
        $ fmt_atrs )
  | Pexp_extension ext -> hvbox 2 (fmt_extension c ctx "%" ext) $ fmt_atrs
  | Pexp_for (p1, e1, e2, dir, e3) ->
      hvbox 0
        ( hvbox 2
            ( hvbox 0
                ( fmt "for@ " $ fmt_pattern c (sub_pat ~ctx p1) $ fmt "@ = "
                $ fmt_expression c (sub_exp ~ctx e1)
                $ fmt (if Poly.(dir = Upto) then "@ to " else "@ downto ")
                $ fmt_expression c (sub_exp ~ctx e2) $ fmt "@ do" )
            $ fmt "@ " $ fmt_expression c (sub_exp ~ctx e3) )
        $ fmt "@ done" )
      $ fmt_atrs
  | Pexp_coerce (e1, t1, t2) ->
      hvbox 2
        ( wrap_fits_breaks "(" ")"
            ( fmt_expression c (sub_exp ~ctx e1)
            $ opt t1 (fmt "@ : " >$ (sub_typ ~ctx >> fmt_core_type c))
            $ fmt "@ :> " $ fmt_core_type c (sub_typ ~ctx t2) )
        $ fmt_atrs )
  | Pexp_while (e1, e2) ->
      hvbox 0
        ( hvbox 2
            ( hvbox 0
                ( fmt "while@ " $ fmt_expression c (sub_exp ~ctx e1)
                $ fmt "@ do" )
            $ fmt "@ " $ fmt_expression c (sub_exp ~ctx e2) )
        $ fmt "@ done" $ fmt_atrs )
  | Pexp_unreachable -> fmt "."
  | Pexp_new _
   |Pexp_object _
   |Pexp_override _
   |Pexp_poly _
   |Pexp_send _
   |Pexp_setinstvar _ ->
      internal_error "classes not implemented" []


and fmt_cases (c: Conf.t) ctx cs =
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
          fmt_or_k c.sparse
            (fmt_or_k paren_body (fmt "@;<1 2>->") (fmt " ->@;<0 3>"))
            (fmt_or_k paren_body (fmt "@;<1 -2>-> (") (fmt " ->@;<0 -1>"))
        in
        hovbox 4
          ( hvbox 0
              ( fmt_pattern c
                  ~pro:(if first then if_newline "| " else fmt "| ")
                  ~parens:paren_lhs xlhs
              $ opt pc_guard (fun g ->
                    fmt "@;<1 2>when " $ fmt_expression c (sub_exp ~ctx g) )
              )
          $ fmt_if_k (indent <= 2) fmt_arrow )
        $ fmt_if_k (indent > 2) fmt_arrow
      in
      fmt_if (not first) "@ "
      $ cbox_if (not c.sparse) indent
          ( hvbox_if (not c.sparse) indent fmt_lhs
          $ ( match (c.sparse, indent > 2, paren_body) with
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
  hvbox 0
    ( hvbox 2
        ( str pre $ fmt " " $ wrap_if (is_symbol_id txt) "( " " )" (str txt)
        $ fmt " :@ " $ fmt_core_type c (sub_typ ~ctx pval_type)
        $ list_fl pval_prim (fun ~first ~last:_ s ->
              fmt_if first "@ =" $ fmt " \"" $ str s $ fmt "\"" ) )
    $ fmt_attributes c (fmt "@;<2 2>") ~key:"@@" atrs (fmt "")
    $ fmt_docstring ~pro:(fmt "@\n") doc )


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
    | Ptype_variant ctor_decls ->
        hvbox 2
          (fmt_manifest ~priv:Public mfst $ fmt " =" $ fmt_private_flag priv)
        $ fmt "@ " $ list_fl ctor_decls (fmt_constructor_declaration c ctx)
    | Ptype_record lbl_decls ->
        hvbox 2
          (fmt_manifest ~priv:Public mfst $ fmt " =" $ fmt_private_flag priv)
        $ fmt "@ "
        $ hvbox 0
            (wrap_fits_breaks "{" "}"
               (list lbl_decls "@,; " (fmt_label_declaration c ctx)))
    | Ptype_open -> fmt " = .."
  in
  let fmt_cstrs cstrs =
    fmt_if_k
      (not (List.is_empty cstrs))
      ( fmt "@;"
      $ hvbox 2
          (list cstrs "@ " (fun (t1, t2, _) ->
               fmt "constraint@ " $ fmt_core_type c (sub_typ ~ctx t1)
               $ fmt " =@ " $ fmt_core_type c (sub_typ ~ctx t2) )) )
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
  Cmts.fmt loc @@ Cmts.fmt ptype_loc
  @@ hvbox 0
       ( fmt_docstring ~epi:(fmt "@,") doc
       $ hvbox 2
           ( str pre $ fmt_tydcl_params c ctx ptype_params
           $ (match fmt_name with Some pp -> pp | None -> str txt)
           $ fmt_manifest_kind ptype_manifest ptype_private ptype_kind
           $ fmt_cstrs ptype_cstrs
           $ fmt_attributes c (fmt "@ ") ~key:"@@" atrs (fmt "") ) )
  $ fmt brk


and fmt_label_declaration c ctx lbl_decl =
  let {pld_mutable; pld_name= {txt; loc}; pld_type; pld_loc; pld_attributes} =
    lbl_decl
  in
  let doc, atrs = doc_atrs pld_attributes in
  let fmt_cmts = Cmts.fmt pld_loc in
  fmt_cmts
  @@ hvbox 4
       ( hvbox 2
           ( fmt_if Poly.(pld_mutable = Mutable) "mutable "
           $ Cmts.fmt loc @@ str txt $ fmt ":@ "
           $ fmt_core_type c (sub_typ ~ctx pld_type) )
       $ fmt_docstring ~pro:(fmt "@;<2 0>") doc
       $ fmt_attributes c (fmt " ") ~key:"@" atrs (fmt "") )


and fmt_constructor_declaration c ctx ~first ~last:_ cstr_decl =
  let {pcd_name= {txt; loc}; pcd_args; pcd_res; pcd_attributes; pcd_loc} =
    cstr_decl
  in
  let doc, atrs = doc_atrs pcd_attributes in
  fmt_if (not first) "@ " $ Cmts.fmt_before pcd_loc $ Cmts.fmt_before loc
  $ fmt_or_k first (if_newline "| ") (fmt "| ")
  $ hovbox 2
      ( hvbox 2
          (str txt $ fmt_constructor_arguments_result c ctx pcd_args pcd_res)
      $ fmt_if (Option.is_some doc) "@;<2 0>" $ fmt_docstring doc
      $ fmt_attributes c (fmt " ") ~key:"@" atrs (fmt "") )
  $ Cmts.fmt_after ?pro:None ~epi:(fmt "@ ") loc
  $ Cmts.fmt_after ?pro:None ~epi:(fmt "@ ") pcd_loc


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
    ( fmt_docstring ~epi:(fmt "@,") doc
    $ hvbox 2
        ( fmt "type " $ fmt_tydcl_params c ctx ptyext_params
        $ fmt_longident txt $ fmt " +=" $ fmt_private_flag ptyext_private
        $ fmt "@ "
        $ hvbox 0
            ( if_newline "| "
            $ list ptyext_constructors "@ | " (fun ctor ->
                  hvbox 0 (fmt_extension_constructor c " of@ " ctx ctor) ) )
        )
    $ fmt_attributes c (fmt "") ~key:"@@" atrs (fmt "") )


and fmt_extension_constructor ?pre c sep ctx ec =
  let {pext_name= {txt}; pext_kind; pext_attributes} = ec in
  let doc, atrs = doc_atrs pext_attributes in
  hvbox 4
    ( hvbox 2
        ( hvbox 2
            ( Option.call ~f:pre $ str txt
            $ fmt_attributes c (fmt " ") ~key:"@" atrs
                ( match pext_kind with
                | Pext_decl ((Pcstr_tuple [] | Pcstr_record []), None)
                 |Pext_decl (_, None)
                 |Pext_rebind _ ->
                    fmt ""
                | Pext_decl ((Pcstr_tuple [] | Pcstr_record []), Some _)
                 |Pext_decl (_, Some _) ->
                    fmt " " ) )
        $
        match pext_kind with
        | Pext_decl ((Pcstr_tuple [] | Pcstr_record []), None) -> fmt ""
        | Pext_decl ((Pcstr_tuple [] | Pcstr_record []), Some res) ->
            str sep $ fmt_core_type c (sub_typ ~ctx res)
        | Pext_decl (args, res) ->
            fmt_constructor_arguments_result c ctx args res
        | Pext_rebind {txt} -> fmt " = " $ fmt_longident txt )
    $ fmt_docstring ~pro:(fmt "@;<2 0>") doc )


and fmt_module_type c {ast= mt} =
  let ctx = Mty mt in
  let {pmty_desc} = mt in
  match pmty_desc with
  | Pmty_ident {txt} -> {empty with bdy= fmt_longident txt}
  | Pmty_signature s ->
      { opn= open_hvbox 0
      ; pro= Some (fmt "sig")
      ; psp= fmt "@;<1000 2>"
      ; bdy= fmt_signature c ctx s
      ; cls= close_box
      ; esp= fmt "@;<1000 0>"
      ; epi= Some (fmt "end") }
  | Pmty_functor ({txt}, mt1, mt2) ->
      let blk = fmt_module_type c (sub_mty ~ctx mt2) in
      { blk with
        pro=
          Some
            ( fmt "functor (" $ str txt
            $ opt mt1 (fun mt1 ->
                  let {opn; pro; psp; bdy; cls; esp; epi} =
                    fmt_module_type c (sub_mty ~ctx mt1)
                  in
                  fmt " :" $ opn $ Option.call ~f:pro $ psp $ fmt "@;<1 2>"
                  $ bdy $ cls $ esp $ Option.call ~f:epi )
            $ fmt ") -> " $ Option.call ~f:blk.pro ) }
  | Pmty_with (mt, wcs) ->
      let {opn; pro; psp; bdy; cls; esp; epi} =
        fmt_module_type c (sub_mty ~ctx mt)
      in
      { empty with
        bdy=
          hvbox 0
            ( opn $ Option.call ~f:pro $ psp $ bdy $ cls $ esp
            $ Option.call ~f:epi
            $ list_fl wcs (fun ~first ~last:_ wc ->
                  fmt_or first "@ with" "@;<1 1>and"
                  $ fmt_with_constraint c ctx wc ) ) }
  | Pmty_typeof me -> (
      let blk = fmt_module_expr c (sub_mod ~ctx me) in
      match blk.pro with
      | Some pro -> {blk with pro= Some (fmt "module type of " $ pro)}
      | _ -> {blk with bdy= hvbox 2 (fmt "module type of@ " $ blk.bdy)} )
  | Pmty_extension ext -> {empty with bdy= fmt_extension c ctx "%" ext}
  | Pmty_alias {txt} -> {empty with bdy= fmt_longident txt}


and fmt_signature c ctx itms =
  let grps =
    List.group itms ~break:(fun itmI itmJ ->
        let is_simple itm =
          match itm.psig_desc with
          | Psig_open _ -> true
          | Psig_module {pmd_type= {pmty_desc= Pmty_alias _}} -> true
          | _ -> false
        in
        not (is_simple itmI) || not (is_simple itmJ) )
  in
  let fmt_grp itms =
    list itms "@\n" (sub_sig ~ctx >> fmt_signature_item c)
  in
  hvbox 0 (list grps "\n@;<1000 0>" fmt_grp)


and fmt_signature_item c {ast= si} =
  protect (Sig si)
  @@ Cmts.fmt ~epi:(fmt "\n@\n") ~eol:(fmt "\n@\n") si.psig_loc
  @@
  let ctx = Sig si in
  match si.psig_desc with
  | Psig_attribute atr ->
      let doc, atrs = doc_atrs [atr] in
      fmt_docstring ~epi:(fmt "") doc
      $ fmt_attributes c (fmt "") ~key:"@@@" atrs (fmt "")
  | Psig_exception exc ->
      hvbox 2
        (fmt_extension_constructor ~pre:(fmt "exception@ ") c " of " ctx exc)
  | Psig_extension (ext, atrs) ->
      hvbox 0
        ( fmt_extension c ctx "%%" ext $ fmt "@ "
        $ fmt_attributes c (fmt "") ~key:"@@" atrs (fmt "") )
  | Psig_include {pincl_mod; pincl_attributes} ->
      let doc, atrs = doc_atrs pincl_attributes in
      let keyword, {opn; pro; psp; bdy; cls; esp; epi} =
        match pincl_mod with
        | {pmty_desc= Pmty_typeof me} ->
            let blk = fmt_module_expr c (sub_mod ~ctx me) in
            ( fmt "include module type of"
              $ fmt_or (Option.is_some blk.pro) " " "@ "
            , blk )
        | _ -> (fmt "include ", fmt_module_type c (sub_mty ~ctx pincl_mod))
      in
      hvbox 0
        ( fmt_docstring ~epi:(fmt "@,") doc $ opn
        $ hvbox 2 (keyword $ Option.call ~f:pro $ psp $ bdy) $ cls $ esp
        $ Option.call ~f:epi
        $ fmt_attributes c (fmt "") ~key:"@@" atrs (fmt "") )
  | Psig_modtype mtd -> fmt_module_type_declaration c ctx mtd
  | Psig_module md ->
      hvbox 0 (fmt_module_declaration c ctx ~rec_flag:false md)
  | Psig_open od -> fmt_open_description c od
  | Psig_recmodule mds ->
      hvbox 0
        (list mds "\n@\nand " (fmt_module_declaration c ctx ~rec_flag:true))
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
  | Psig_class _ | Psig_class_type _ ->
      internal_error "classes not implemented" []


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
              ( fmt_or colon " :" " =" $ fmt_if (Option.is_some blk.pro) " "
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
    ( fmt_docstring ~epi:(fmt "@,") doc $ opn_b
    $ (if Option.is_some epi_t then open_hovbox else open_hvbox) 0 $ opn_t
    $ fmt_if_k (Option.is_some pro_t) (open_hvbox 0)
    $ ( match arg_blks with
      | (_, Some {opn; pro= Some _}) :: _ -> opn $ open_hvbox 0
      | _ -> fmt "" )
    $ hvbox 4
        ( str keyword $ fmt " " $ str name
        $ list_pn arg_blks (fun ?prev:_ ({txt}, arg_mtyp) ?next ->
              ( match arg_mtyp with
              | Some {pro= None} -> fmt "@ @[<hv 2>("
              | _ -> fmt "@ (" )
              $ str txt
              $ opt arg_mtyp (fun {pro; psp; bdy; cls; esp; epi} ->
                    fmt " : " $ Option.call ~f:pro
                    $ fmt_if_k (Option.is_some pro) close_box $ psp $ bdy
                    $ fmt_if_k (Option.is_some pro) cls $ esp
                    $ ( match next with
                      | Some (_, Some {opn; pro= Some _}) ->
                          opn $ open_hvbox 0
                      | _ -> fmt "" )
                    $ Option.call ~f:epi )
              $
              match arg_mtyp with
              | Some {pro= None} -> fmt ")@]"
              | _ -> fmt ")" ) )
    $ Option.call ~f:pro_t $ fmt_if_k (Option.is_some pro_t) close_box
    $ psp_t $ bdy_t $ cls_t $ esp_t $ Option.call ~f:epi_t
    $ fmt_if (Option.is_some xbody) " ="
    $ fmt_if (Option.is_some pro_b) "@ " $ Option.call ~f:pro_b $ close_box
    $ psp_b $ fmt_if (Option.is_none pro_b && Option.is_some xbody) "@ "
    $ bdy_b $ cls_b $ esp_b $ Option.call ~f:epi_b
    $ fmt_attributes c (fmt "@ ") ~key:"@@" atrs (fmt "")
    $ Option.call ~f:epi )


and fmt_module_declaration c ctx ~rec_flag pmd =
  let {pmd_name= {txt}; pmd_type; pmd_attributes} = pmd in
  let keyword = if rec_flag then "module rec" else "module" in
  let xargs, xmty = sugar_functor_type (sub_mty ~ctx pmd_type) in
  let colon =
    match xmty.ast.pmty_desc with Pmty_alias _ -> false | _ -> true
  in
  fmt_module c keyword txt xargs None colon (Some xmty) pmd_attributes


and fmt_module_type_declaration c ctx pmtd =
  let {pmtd_name= {txt}; pmtd_type; pmtd_attributes} = pmtd in
  fmt_module c "module type" txt [] None false
    (Option.map pmtd_type ~f:(sub_mty ~ctx))
    pmtd_attributes


and fmt_open_description c {popen_lid; popen_override; popen_attributes} =
  let doc, atrs = doc_atrs popen_attributes in
  fmt_docstring ~epi:(fmt "@,") doc $ fmt "open"
  $ fmt_if Poly.(popen_override = Override) "!" $ fmt " "
  $ fmt_longident popen_lid.txt
  $ fmt_attributes c (fmt " ") ~key:"@@" atrs (fmt "")


and fmt_with_constraint c ctx = function
  | Pwith_type ({txt}, td) ->
      fmt " type "
      $ fmt_type_declaration c ctx ~fmt_name:(fmt_longident txt) td
  | Pwith_module ({txt= m1}, {txt= m2}) ->
      fmt " module " $ fmt_longident m1 $ fmt " = " $ fmt_longident m2
  | Pwith_typesubst (_, td) ->
      fmt " type " $ fmt_type_declaration c ~eq:":=" ctx td
  | Pwith_modsubst ({txt= m1}, {txt= m2}) ->
      fmt " module " $ fmt_longident m1 $ fmt " := " $ fmt_longident m2


and fmt_module_expr c {ast= m} =
  let ctx = Mod m in
  let {pmod_desc; pmod_loc; pmod_attributes} = m in
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
      let {opn= opn_a; pro= pro_a; bdy= bdy_a; cls= cls_a; epi= epi_a} as
          blk_a =
        fmt_module_expr c (sub_mod ~ctx me_a)
      in
      let fmt_rator =
        fmt_docstring ~epi:(fmt "@,") doc $ opn_f $ psp_f
        $ Option.call ~f:pro_f $ bdy_f $ cls_f $ esp_f
        $ Option.call ~f:epi_f $ fmt "@ " $ fmt "("
      in
      if Option.is_some pro_a then
        { blk_a with
          opn= opn_a
        ; pro=
            Some
              ( Cmts.fmt_before pmod_loc $ open_hvbox 2 $ fmt_rator
              $ close_box $ Option.call ~f:pro_a )
        ; cls= cls_a
        ; epi=
            Some
              ( Option.call ~f:epi_a $ fmt ")"
              $ fmt_attributes c (fmt " ") ~key:"@@" atrs (fmt "")
              $ Cmts.fmt_after pmod_loc ) }
      else
        { blk_a with
          opn= open_hvbox 2 $ opn_a
        ; bdy= Cmts.fmt_before pmod_loc $ open_hvbox 2 $ fmt_rator $ bdy_a
        ; cls= close_box $ cls_a $ close_box
        ; epi=
            Some
              ( Option.call ~f:epi_a $ fmt ")"
              $ fmt_attributes c (fmt " ") ~key:"@@" atrs (fmt "")
              $ Cmts.fmt_after pmod_loc ) }
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
        fmt_module_expr c (sub_mod ~ctx me_a)
      in
      { empty with
        opn= opn_a $ opn_f $ open_hvbox 2
      ; bdy=
          hvbox 2
            ( Cmts.fmt_before pmod_loc $ fmt_docstring ~epi:(fmt "@,") doc
            $ Option.call ~f:pro_f $ psp_f $ bdy_f $ esp_f
            $ Option.call ~f:epi_f $ fmt "@ " $ fmt "("
            $ Option.call ~f:pro_a $ psp_a $ bdy_a $ esp_a
            $ Option.call ~f:epi_a $ fmt ")" )
      ; cls= close_box $ cls_f $ cls_a
      ; epi=
          Some
            ( Cmts.fmt_after pmod_loc
            $ fmt_attributes c (fmt " ") ~key:"@@" atrs (fmt "") ) }
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
            ( Cmts.fmt_before pmod_loc $ fmt_docstring ~epi:(fmt "@,") doc
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
            ( Cmts.fmt_after pmod_loc
            $ fmt_attributes c (fmt " ") ~key:"@@" atrs (fmt "") ) }
  | Pmod_functor ({txt}, mt, me) ->
      let { opn= opn_t
          ; pro= pro_t
          ; psp= psp_t
          ; bdy= bdy_t
          ; cls= cls_t
          ; esp= esp_t
          ; epi= epi_t } =
        Option.value_map mt ~default:empty
          ~f:(sub_mty ~ctx >> fmt_module_type c)
      in
      let { opn= opn_e
          ; pro= pro_e
          ; psp= psp_e
          ; bdy= bdy_e
          ; cls= cls_e
          ; esp= esp_e
          ; epi= epi_e } =
        fmt_module_expr c (sub_mod ~ctx me)
      in
      { opn= opn_e $ opn_t
      ; pro= None
      ; psp= fmt ""
      ; bdy=
          Cmts.fmt pmod_loc
          @@ hvbox 0
               ( fmt "functor@ "
               $ wrap "(" ")"
                   ( str txt
                   $ opt mt (fun _ ->
                         fmt "@ : " $ Option.call ~f:pro_t $ psp_t
                         $ fmt "@;<1 2>" $ bdy_t $ esp_t
                         $ Option.call ~f:epi_t ) )
               $ fmt " ->@ " $ Option.call ~f:pro_e $ psp_e $ bdy_e $ esp_e
               $ Option.call ~f:epi_e )
      ; cls= cls_t $ cls_e
      ; esp= fmt ""
      ; epi= None }
  | Pmod_ident {txt} ->
      {empty with bdy= Cmts.fmt pmod_loc @@ fmt_longident txt}
  | Pmod_structure sis ->
      { opn= open_hvbox 0
      ; pro= Some (Cmts.fmt_before pmod_loc $ fmt "struct")
      ; psp= fmt "@;<1000 2>"
      ; bdy= fmt_structure c ~sep:";; " ctx sis
      ; cls= close_box
      ; esp= fmt "@ "
      ; epi= Some (fmt "end" $ Cmts.fmt_after pmod_loc) }
  | Pmod_unpack e1 ->
      { empty with
        bdy=
          Cmts.fmt pmod_loc
          @@ wrap_fits_breaks "(" ")"
               (fmt "val " $ fmt_expression c (sub_exp ~ctx e1)) }
  | Pmod_extension x1 ->
      {empty with bdy= Cmts.fmt pmod_loc @@ fmt_extension c ctx "%" x1}


and fmt_structure c ?(sep= "") ctx itms =
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
           |Pstr_include {pincl_mod= {pmod_attributes= atrs}}
           |Pstr_extension (_, atrs) ->
              Option.is_some (fst (doc_atrs atrs))
          | Pstr_module {pmb_attributes; pmb_expr= {pmod_attributes}} ->
              Option.is_some
                (fst (doc_atrs (List.append pmb_attributes pmod_attributes)))
          | Pstr_value (_, []) | Pstr_type (_, []) | Pstr_recmodule [] ->
              false
          | Pstr_class _ | Pstr_class_type _ ->
              internal_error "classes not implemented" []
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
        has_doc itmJ || not (is_simple itmI) || not (is_simple itmJ) )
  in
  let fmt_grp ~last:last_grp itms =
    list_fl itms (fun ~first ~last itm ->
        fmt_if (not first) "@\n"
        $ fmt_structure_item c ~sep ~last:(last && last_grp)
            (sub_str ~ctx itm) )
  in
  hvbox 0
    (list_fl grps (fun ~first ~last grp ->
         fmt_if (not first) "\n@\n" $ fmt_grp ~last grp ))


and fmt_structure_item c ~sep ~last:last_item {ctx; ast= si} =
  protect (Str si)
  @@
  let at_top = Poly.(ctx = Top) in
  let ctx = Str si in
  let fmt_cmts_before =
    Cmts.fmt_before ~epi:(fmt "\n@\n") ~eol:(fmt "\n@\n") ~adj:(fmt "@\n")
      si.pstr_loc
  and fmt_cmts_after = Cmts.fmt_after ~pro:(fmt "\n@\n") si.pstr_loc in
  wrap_k fmt_cmts_before fmt_cmts_after
  @@
  match si.pstr_desc with
  | Pstr_attribute atr ->
      let doc, atrs = doc_atrs [atr] in
      fmt_docstring ~epi:(fmt "") doc
      $ fmt_attributes c (fmt "") ~key:"@@@" atrs (fmt "")
  | Pstr_eval (exp, atrs) ->
      let doc, atrs = doc_atrs atrs in
      str sep $ fmt_docstring doc
      $ cbox 0 (fmt_if at_top ";; " $ fmt_expression c (sub_exp ~ctx exp))
      $ fmt_attributes c (fmt " ") ~key:"@@" atrs (fmt "")
  | Pstr_exception extn_constr ->
      hvbox 2
        (fmt_extension_constructor ~pre:(fmt "exception@ ") c ": " ctx
           extn_constr)
  | Pstr_include {pincl_mod; pincl_attributes} ->
      let {opn; pro; psp; bdy; cls; esp; epi} =
        fmt_module_expr c (sub_mod ~ctx pincl_mod)
      in
      opn
      $ ( hvbox 2 (fmt "include " $ Option.call ~f:pro) $ psp $ bdy $ cls
        $ esp $ Option.call ~f:epi
        $ fmt_attributes c (fmt " ") ~key:"@@" pincl_attributes (fmt "") )
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
  | Pstr_typext te -> fmt_type_extension c ctx te
  | Pstr_value (rec_flag, bindings) ->
      hvbox 0
        (list_fl bindings (fun ~first ~last binding ->
             fmt_value_binding c ~rec_flag ~first ctx binding
               ~epi:(fits_breaks ~force_fit_if:last_item "" "\n")
             $ fmt_if (not last) "\n@\n" ))
  | Pstr_modtype mtd -> fmt_module_type_declaration c ctx mtd
  | Pstr_extension (ext, atrs) ->
      let doc, atrs = doc_atrs atrs in
      fmt_docstring doc $ fmt_extension c ctx "%%" ext
      $ fmt_attributes c (fmt " ") ~key:"@@" atrs (fmt "")
  | Pstr_class _ | Pstr_class_type _ ->
      internal_error "classes not implemented" []


and fmt_value_binding c ~rec_flag ~first ?in_ ?epi ctx binding =
  let {pvb_pat; pvb_expr; pvb_attributes; pvb_loc} = binding in
  let doc, atrs = doc_atrs pvb_attributes in
  let keyword =
    if first then if Poly.(rec_flag = Recursive) then "let rec" else "let"
    else "and"
  in
  let xpat, xpcstr, (xargs, ({ast= body} as xbody)) =
    let xbody = sub_exp ~ctx pvb_expr in
    match (pvb_pat.ppat_desc, pvb_expr.pexp_desc) with
    (* recognize and undo the pattern of code introduced by
       ocaml/ocaml@fd0dc6a0fbf73323c37a73ea7e8ffc150059d6ff to fix
       https://caml.inria.fr/mantis/view.php?id=7344 *)
    | ( Ppat_constraint (pat, {ptyp_desc= Ptyp_poly ([], typ1)})
      , Pexp_constraint (_, typ2) )
      when Poly.equal typ1 typ2 ->
        let ctx = Pat pvb_pat in
        (sub_pat ~ctx pat, None, sugar_fun None xbody)
    | Ppat_constraint (pat, typ), _ ->
        let ctx = Pat pvb_pat in
        (sub_pat ~ctx pat, Some (sub_typ ~ctx typ), ([], xbody))
    | _ -> (sub_pat ~ctx pvb_pat, None, sugar_fun None xbody)
  in
  let xecstr, xbody =
    let ctx = Exp body in
    match body.pexp_desc with
    | Pexp_constraint (exp, typ) ->
        (Some (sub_typ ~ctx typ), sub_exp ~ctx exp)
    | _ -> (None, xbody)
  in
  let fmt_body ({ast= body} as xbody) =
    match body with
    | {pexp_desc= Pexp_function cs; pexp_attributes} ->
        let ctx = Exp body in
        fmt "@ function"
        $ fmt_attributes c (fmt "") ~key:"@" pexp_attributes (fmt "")
        $ close_box $ fmt "@ " $ fmt_cases c ctx cs
    | _ ->
        close_box
        $ (fmt "@ " $ fmt_expression c ~eol:(fmt "@;<1000 0>") xbody)
  in
  fmt_docstring ~epi:(fmt "@,") doc $ Cmts.fmt_before pvb_loc
  $ hvbox 2
      ( open_hovbox 2
      $ ( hovbox 4
            ( str keyword $ fmt_attributes c (fmt "") ~key:"@" atrs (fmt "")
            $ fmt " " $ fmt_pattern c xpat
            $ opt xpcstr (fmt "@ : " >$ fmt_core_type c) $ fmt "@ "
            $ fmt_fun_args c xargs
            $ opt xecstr (fun xtyp ->
                  fmt ": " $ fmt_core_type c xtyp $ fmt "@ " ) )
        $ fmt "=" )
      $ fmt_body xbody $ Cmts.fmt_after pvb_loc $ Option.call ~f:in_
      $ Option.call ~f:epi )


and fmt_module_binding c ?epi ~rec_flag ~first ctx pmb =
  let {pmb_name= {txt}; pmb_expr; pmb_attributes} = pmb in
  let keyword =
    if first then if rec_flag then "module rec" else "module" else "and"
  in
  let me, mt =
    match pmb_expr.pmod_desc with
    | Pmod_constraint (me, mt) -> (me, Some (sub_mty ~ctx mt))
    | _ -> (pmb_expr, None)
  in
  let xargs, xbody, xmty = sugar_functor ?mt (sub_mod ~ctx me) in
  fmt_module c ?epi keyword txt xargs (Some xbody) true xmty
    (List.append pmb_attributes me.pmod_attributes)


(** Entry points *)

let fmt_signature c = fmt_signature c Top

let fmt_structure c = fmt_structure c Top
