[@@@ocamlformat "disambiguate-non-breaking-functions"]

let () = r := (fun () -> f () ; g ())

let () =
  r :=
    fun () ->
      f () ; g () ; g () ; g () ; g () ; g () ; g () ; g () ; g () ; g ()
