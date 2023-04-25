let pf = Printf.printf

let () =
  match Array.to_list Sys.argv with
  | _ :: exe :: args ->
      pf "{0 Manpage: %s}\n\n{v\n%!" (Filename.basename exe) ;
      let s =
        Sys.command
          (String.concat " " (List.map Filename.quote (exe :: args)))
      in
      if s <> 0 then exit s ;
      pf "v}\n%!"
  | [] | [_] ->
      Printf.eprintf "Not enough argument" ;
      exit 2
