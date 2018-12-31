let[@ocamlformat "break-cases=all"] _ =
  try () with
  | End_of_file
  | Not_found -> ()
