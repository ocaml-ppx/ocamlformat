module Result = struct
  module Infix = struct
    let ( >>= ) r f = match r with Ok x -> f x | Error _ as e -> e

    let ( >>| ) r f = match r with Ok x -> Ok (f x) | Error _ as e -> e
  end

  let map_error ~f = function Ok x -> Ok x | Error x -> Error (f x)
end

open Result.Infix
module Ocf = Ocamlformat_rpc_lib

let log = Format.printf

let supported_versions = ["v1"]

type state = Uninitialized | Running of Ocf.V1.Client.t | Errored

let state : state ref = ref Uninitialized

let start () =
  let prog = Sys.argv.(1) in
  let argv = [|"ocamlformat-rpc"|] in
  ( match
      let input, output = Unix.open_process_args prog argv in
      let pid = Unix.process_pid (input, output) in
      let client = Ocf.V1.Client.mk ~pid input output in
      log "[ocf] client V1 selected\n%!" ;
      state := Running client ;
      client
    with
  | exception _ ->
      Error
        (`Msg
          "OCamlFormat-RPC did not respond. Check that a compatible version \
           of the OCamlFormat RPC server (ocamlformat-rpc >= 0.18.0) is \
           installed." )
  | x -> Ok x )
  |> Result.map_error ~f:(fun (`Msg msg) ->
         state := Errored ;
         log
           "An error occured while initializing and configuring ocamlformat:\n\
            %s\n\
            %!"
           msg ;
         `No_process )

let get_client () =
  match !state with
  | Uninitialized -> start ()
  | Running cl ->
      let i, _ = Unix.waitpid [WNOHANG] (Ocf.V1.Client.pid cl) in
      if i = 0 then Ok cl else start ()
  | Errored -> Error `No_process

let config c =
  get_client ()
  >>= fun cl -> log "[ocf] Config\n%!" ; Ocf.V1.Client.config c cl

let format x =
  get_client ()
  >>= fun cl ->
  log "[ocf] Format '%s'\n%!" x ;
  Ocf.V1.Client.format x cl

let halt () =
  get_client ()
  >>= fun cl ->
  log "[ocf] Halt\n%!" ;
  Ocf.V1.Client.halt cl >>| fun () -> state := Uninitialized

let protect_unit x =
  match x with
  | Ok () -> ()
  | Error (`Msg e) -> log "Error: %s\n%!" e
  | Error `No_process -> log "No process\n%!"

let protect_string x =
  match x with
  | Ok s -> log "@[<hv>Output:@;%s@]\n%!" s
  | Error (`Msg e) -> log "Error: %s\n%!" e
  | Error `No_process -> log "No process\n%!"

let () =
  log "Starting then doing nothing\n%!" ;
  protect_unit @@ halt ()

let () =
  log "Sending requests\n%!" ;
  protect_unit @@ config [("profile", "janestreet")] ;
  protect_string @@ format "char -> string" ;
  protect_unit @@ halt ()
