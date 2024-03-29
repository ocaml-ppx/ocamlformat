open Bos

let to_dashes v = String.map v ~f:(function '.' -> '-' | c -> c)

let latest = to_dashes Ocamlformat_lib.Version.current

let run cmd =
  let li = Cmd.to_list cmd in
  let e = List.hd_exn li in
  let argv = li |> Array.of_list in
  Unix.execvp e argv

let () =
  Ocamlformat_lib.Conf.enable_warnings false ;
  if Result.is_error (Bin_conf.action ()) then Stdlib.exit 1 ;
  let required_version =
    Ocamlformat_lib.Conf_t.Elt.v
      !Bin_conf.global_conf.lib_conf.opr_opts.required_version
  in
  let ocamlformat_cmd version =
    Cmd.(
      v ("ocamlformat-" ^ version)
      %% (() |> Sys.get_argv |> Array.to_list |> List.tl_exn |> of_list) )
  in
  let open Bos in
  match required_version with
  | None -> run (ocamlformat_cmd latest)
  | Some v ->
      let v_dashes = to_dashes v in
      let exec_name = "ocamlformat-" ^ v_dashes in
      let cmd = ocamlformat_cmd v_dashes in
      if Stdlib.Result.get_ok @@ OS.Cmd.exists cmd then run cmd
      else if
        Ocamlformat_lib.Conf_t.Elt.v
          !Bin_conf.global_conf.lib_conf.opr_opts.version_check
      then (
        Stdlib.Printf.eprintf
          "Ocamlformat version %s not installed\nHint: `opam install %s`\n%!"
          v exec_name ;
        Stdlib.exit 1 )
      else run (ocamlformat_cmd latest)
