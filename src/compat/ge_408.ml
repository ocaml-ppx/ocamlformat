let _ = Clflags.error_style := Some Misc.Error_style.Short

let setup_warning_filter f =
  let warning_reporter = !Location.warning_reporter in
  (Location.warning_reporter :=
     fun loc warn ->
     if f loc warn
     then Location.default_warning_reporter loc warn
     else None);
  `Reset (fun () -> Location.warning_reporter := warning_reporter)

let print_warning l w =
  Location.default_warning_reporter l w
  |> Option.iter ~f:(Location.print_report Caml.Format.err_formatter)
