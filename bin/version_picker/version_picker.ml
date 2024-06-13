(* This executable reads the conf to know the required version. Then it tries
   to run [ocamlformat-%version%]. If it is not found, then it prints
   instructions to install it. *)

module Sys = Stdlib.Sys

let to_unders v = String.map v ~f:(function '.' -> '_' | c -> c)

let latest = to_unders Ocamlformat_lib.Current_version.v

(* Tool existence and search *)

let find_exec_in_dir dirname exec_name =
  if Sys.file_exists dirname && Sys.is_directory dirname then
    let dir = Sys.readdir dirname in
    dir
    |> Array.find ~f:(String.( = ) exec_name)
    |> Option.map ~f:(Filename.concat dirname)
  else None

let path_sep = if Sys.win32 then ';' else ':'

let find_in_path exec =
  Unix.getenv "PATH" |> String.split ~on:path_sep
  |> List.find_map ~f:(function
       | "" -> None
       | dir -> find_exec_in_dir dir exec )

let exec_of_version ?(ensure_dot_exe = true) version =
  "ocamlformat-" ^ to_unders version
  ^ if ensure_dot_exe && Sys.win32 then ".exe" else ""

let find_version version =
  let exec_name = exec_of_version version in
  let dirname = Filename.dirname Sys.executable_name in
  find_exec_in_dir dirname exec_name
  |> (function None -> find_in_path exec_name | a -> a)
  |> function None -> Error version | Some a -> Ok a

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

let error_exec_not_found version =
  Stdlib.Printf.eprintf
    "Ocamlformat version %s not installed.\n\
     You may be able to get it with: `opam install %s`\n\
     If this package does not exists, try `opam update`.\n\
     If it still does not exist, there might be a typo in your config, or \
     the version is very old and has been not been yet repackaged.\n\
     %!"
    version
    (exec_of_version ~ensure_dot_exe:false version) ;
  Stdlib.exit 1

let run exec = execvp exec Sys.argv

let () =
  Ocamlformat_lib.Conf.enable_warnings false ;
  if Result.is_error (Bin_conf.action ()) then Stdlib.exit 1 ;
  let required_version =
    Ocamlformat_lib.Conf_t.Elt.v
      !Bin_conf.global_conf.lib_conf.opr_opts.required_version
  in
  let version =
    match required_version with None -> latest | Some version -> version
  in
  let exec =
    match find_version version with
    | Error _
      when not
           @@ Ocamlformat_lib.Conf_t.Elt.v
                !Bin_conf.global_conf.lib_conf.opr_opts.version_check ->
        find_version latest
    | exec -> exec
  in
  match exec with
  | Error version -> error_exec_not_found version
  | Ok exec -> run exec
