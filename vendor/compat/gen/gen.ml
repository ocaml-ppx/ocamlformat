let () =
  let ocaml_version_str = Sys.argv.(1) in
  let ocaml_version =
    Scanf.sscanf ocaml_version_str "%u.%u" (fun a b -> (a, b))
  in
  print_string (if ocaml_version < (4, 08) then "lt_408.ml" else "ge_408.ml")
