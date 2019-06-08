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
