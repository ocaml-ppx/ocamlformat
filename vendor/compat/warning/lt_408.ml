let with_warning_filter ~filter ~f =
  let warning_printer = !Location.warning_printer in
  (Location.warning_printer :=
     fun loc fmt warn ->
       if filter loc warn then warning_printer loc fmt warn else ()) ;
  let reset () = Location.warning_printer := warning_printer in
  try
    let x = f () in
    reset () ; x
  with e -> reset () ; raise e

let print_warning l w =
  !Location.warning_printer l Caml.Format.err_formatter w

let is_unexpected_docstring = function
  | Warnings.Bad_docstring _ -> true
  | _ -> false
