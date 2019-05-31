[@@@ocamlformat "disambiguate-non-breaking-match"]

let () = r := (fun () -> f () ; g ())

let () =
  r :=
    fun () ->
      f () ; g () ; g () ; g () ; g () ; g () ; g () ; g () ; g () ; g ()

let () = r := function () -> f () ; g ()

let () =
  r :=
    function
    | () ->
        f () ; g () ; g () ; g () ; g () ; g () ; g () ; g () ; g () ; g ()

let () = r := match () with () -> f () ; g ()

let () =
  r :=
    match () with
    | () ->
        f () ; g () ; g () ; g () ; g () ; g () ; g () ; g () ; g () ; g ()

let () = r := try () with () -> f () ; g ()

let () =
  r :=
    try ()
    with () ->
      f () ; g () ; g () ; g () ; g () ; g () ; g () ; g () ; g () ; g ()
