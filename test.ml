[@@@ocamlformat "profile=janestreet"]

let _ =
  foo
  |> List.map ~f:(fun x ->
    do_something ();
    do_something ();
    do_something ();
    do_something ();
    do_something_else ())
;;
