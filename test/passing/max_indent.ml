let () =
  fooooo
  |> List.iter (fun x ->
    let x = x $ y in
    fooooooooooo x)

let () =
  fooooo
  |> List.iter
    (fun some_really_really_really_long_name_that_doesn't_fit_on_the_line
      ->
        let x =
          some_really_really_really_long_name_that_doesn't_fit_on_the_line
          $ y
        in
        fooooooooooo x)

let foooooooooo =
  foooooooooooooooooooooo
  |> Option.bind ~f:(function
    | Pform.Expansion.Var (Values l) -> Some (static l)
    | Macro (Ocaml_config, s) ->
      Some
        (static (expand_ocaml_config (Lazy.force ocaml_config) var s))
    | Macro (Env, s) -> Option.map ~f:static (expand_env t var s))

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
