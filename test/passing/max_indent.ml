let () =
  fooooo
  |> List.iter (fun x ->
       let x = x $ y in
       fooooooooooo x )

let () =
  fooooo
  |> List.iter
       (fun some_really_really_really_long_name_that_doesn't_fit_on_the_line
            ->
       let x =
         some_really_really_really_long_name_that_doesn't_fit_on_the_line $ y
       in
       fooooooooooo x )

let foooooooooo =
  foooooooooooooooooooooo
  |> Option.bind ~f:(function
       | Pform.Expansion.Var (Values l) -> Some (static l)
       | Macro (Ocaml_config, s) ->
         Some (static (expand_ocaml_config (Lazy.force ocaml_config) var s))
       | Macro (Env, s) -> Option.map ~f:static (expand_env t var s) )

let fooooooooooooo =
  match lbls with
  | (_, {lbl_all}, _) :: _ ->
    let t =
      Array.map
        (fun lbl -> (mknoloc (Longident.Lident "?temp?"), lbl, omega))
        lbl_all
    in
    fooooooo

let foooooooooo =
  match fooooooooooooo with
  | Pexp_construct
      ({txt= Lident "::"; _}, Some {pexp_desc= Pexp_tuple [_; e2]; _}) ->
    if is_sugared_list e2 then Some (Semi, Non)
    else Some (ColonColon, if exp == e2 then Right else Left)

let foooooooooooooooooooooooooo =
  match foooooooooooooooooooooo with
  | Pexp_apply
      ( { pexp_desc=
            Pexp_ident
              {txt= Lident (("~-" | "~-." | "~+" | "~+.") as op); loc}
        ; pexp_loc
        ; pexp_attributes= []
        ; _ }
      , [(Nolabel, e1)] ) ->
    fooooooooooooooooooooooooooooooooooooo

let fooooooooooooooooooooooooooooooooooo =
  match foooooooooooooooooooooo with
  | ( Ppat_constraint
        ( ({ppat_desc= Ppat_var _; _} as p0)
        , {ptyp_desc= Ptyp_poly ([], t0); _} )
    , Pexp_constraint (e0, t1) )
    when Poly.(t0 = t1) ->
    m.value_binding m

let foooooooooooooooooooooooooooooooo =
  match foooooooooooooooooooooooooooo with
  | Tpat_variant (lab, Some omega, _) -> (
    fun q rem ->
      match q.pat_desc with
      | Tpat_variant (lab', Some arg, _) when lab = lab' -> (p, arg :: rem)
      | Tpat_any -> (p, omega :: rem)
      | _ -> raise NoMatch )
