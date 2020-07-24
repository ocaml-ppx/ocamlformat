let () =
  let ocaml_version =
    Scanf.sscanf Sys.ocaml_version "%u.%u" (fun a b -> (a, b))
  in
  print_string (if ocaml_version < (4, 08) then "lt_408.ml" else "ge_408.ml")
