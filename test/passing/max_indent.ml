let () =
  fooooo
  |> List.iter (fun x ->
    let x = x $ y in
    fooooooooooo x)

let () =
  fooooo
  |> List.iter
    (fun some_really_really_really_long_name_that_doesn't_fit_on_the_line ->
      let x =
        some_really_really_really_long_name_that_doesn't_fit_on_the_line $ y
      in
      fooooooooooo x)

let foooooooooo =
  foooooooooooooooooooooo
  |> Option.bind ~f:(function
    | Pform.Expansion.Var (Values l) -> Some (static l)
    | Macro (Ocaml_config, s) ->
      Some (static (expand_ocaml_config (Lazy.force ocaml_config) var s))
    | Macro (Env, s) -> Option.map ~f:static (expand_env t var s))
