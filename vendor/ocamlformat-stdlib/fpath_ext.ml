include Fpath

let cwd () = Stdlib.Sys.getcwd () |> v

let exists p = to_string p |> Stdlib.Sys.file_exists

let to_absolute file = if is_rel file then append (cwd ()) file else file

let to_string ?(relativize = false) p =
  if relativize then
    Base.Option.value_map
      (Fpath.relativize ~root:(cwd ()) p)
      ~default:(to_string p) ~f:to_string
  else to_string p

let pp fmt p = Format.fprintf fmt "%s" (to_string ~relativize:true p)
