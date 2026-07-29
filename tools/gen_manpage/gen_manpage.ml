(** See [doc/dune] for usage. *)

let pf = Printf.printf

let () =
  match Array.to_list Sys.argv with
  | _ :: prog_name :: cmd ->
      pf "{0 Manpage: %s}\n\n{v\n%!" prog_name ;
      let s =
        Sys.command (String.concat " " (List.map Filename.quote cmd))
      in
      if s <> 0 then exit s ;
      pf "v}\n%!"
  | [] | [_] ->
      Printf.eprintf "Not enough argument" ;
      exit 2
