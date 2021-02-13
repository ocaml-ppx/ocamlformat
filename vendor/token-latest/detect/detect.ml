let dir_name version =
  if version >= (4, 11) then
    match Sys.os_type with
    | "Unix" -> begin  
        match Sys.command "which metaocaml > /dev/null 2>&1" with
        | 0 -> "411ber"
        | _ -> "411"
      end
    | _ -> "411"
  else "408"

let () =
  Scanf.sscanf Sys.ocaml_version "%d.%d" (fun a b -> (a, b))
  |> dir_name |> print_string
