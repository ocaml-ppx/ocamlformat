(* This executable reads the conf to know the required version. Then it tries
   to run [ocamlformat-%version%]. If it is not found, then it prints
   instructions to install it. *)

open Bos

let to_unders v = String.map v ~f:(function '.' -> '_' | c -> c)

let latest = to_unders Ocamlformat_lib.Current_version.v

let execvp =
  if Sys.unix then Unix.execvp
  else fun f args ->
    let pid =
      Unix.create_process f args Unix.stdin Unix.stdout Unix.stderr
    in
    let _pid, r = Unix.waitpid [] pid in
    match r with
    | Unix.WEXITED code -> Stdlib.exit code
    | Unix.WSIGNALED code | Unix.WSTOPPED code ->
        Unix.kill (Unix.getpid ()) code

let run cmd =
  let li = Cmd.to_list cmd in
  let e = List.hd_exn li in
  let argv = li |> Array.of_list in
  execvp e argv

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
  | None ->
      let cmd = ocamlformat_cmd latest in
      if Stdlib.Result.get_ok @@ OS.Cmd.exists cmd then run cmd
      else (
        Stdlib.Printf.eprintf
          "Latest ocamlformat version \"ocamlformat-%s\" could not be found.\n\
           This should never happen, please report this to maintainers.%!"
          latest ;
        Stdlib.exit 23 )
  | Some v ->
      let v_dashes = to_unders v in
      let exec_name = "ocamlformat-" ^ v_dashes in
      let cmd = ocamlformat_cmd v_dashes in
      if Stdlib.Result.get_ok @@ OS.Cmd.exists cmd then run cmd
      else if
        Ocamlformat_lib.Conf_t.Elt.v
          !Bin_conf.global_conf.lib_conf.opr_opts.version_check
      then (
        Stdlib.Printf.eprintf
          "Ocamlformat version %s not installed.\n\
           You may be able to get it with: `opam install %s`\n\
           If this package does not exists, try `opam update`.\n\
           If it still does not exist, there might be a typo in your \
           config, or the version is very old and has been not been yet \
           repackaged.\n\
           %!"
          v exec_name ;
        Stdlib.exit 1 )
      else run (ocamlformat_cmd latest)
