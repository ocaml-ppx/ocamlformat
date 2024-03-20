open Bos

let to_dashes v = String.map v ~f:(function '.' -> '-' | c -> c)

let latest = to_dashes Ocamlformat_lib.Version.current

let run cmd =
  match OS.Cmd.run cmd with
  | Ok () -> Stdlib.exit 0
  | Error _e -> Stdlib.exit 1

let redirect_stdout () =
  let newstdout = Stdlib.open_out "/dev/null" in
  Unix.dup2 (Unix.descr_of_out_channel newstdout) Unix.stdout ;
  Unix.dup2 (Unix.descr_of_out_channel newstdout) Unix.stderr

let () =
  let oldstderr = Unix.dup Unix.stderr in
  let oldstdout = Unix.dup Unix.stdout in
  let devnull = Stdlib.open_out "/dev/null" in
  Unix.dup2 (Unix.descr_of_out_channel devnull) Unix.stdout ;
  Unix.dup2 (Unix.descr_of_out_channel devnull) Unix.stderr ;
  (match Bin_conf.action () with Ok _ -> () | Error _e -> ()) ;
  Unix.dup2 oldstdout Unix.stdout ;
  Unix.dup2 oldstderr Unix.stderr ;
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
        Stdlib.exit 2 )
      else run (ocamlformat_cmd latest)
