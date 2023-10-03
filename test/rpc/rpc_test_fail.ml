module Result = struct
  module Infix = struct
    let ( >>= ) r f = match r with Ok x -> f x | Error _ as e -> e

    let ( >>| ) r f = match r with Ok x -> Ok (f x) | Error _ as e -> e
  end

  let map_error ~f = function Ok x -> Ok x | Error x -> Error (f x)
end

module IO = struct
  type 'a t = 'a

  type ic = in_channel

  type oc = out_channel

  let ( >>= ) x f = f x

  let return x = x

  let read ic =
    match Csexp.input ic with
    | Ok x -> return (Some x)
    | Error _ -> return None

  let write oc lx =
    List.iter (Csexp.to_channel oc) lx ;
    Stdlib.flush oc ;
    return ()
end

open Result.Infix
open Ocamlformat_rpc_lib
module Ocf = Make (IO)

let log = Format.printf

(* latest first *)
let supported_versions = List.map Version.to_string [V2; V1]

type close = unit -> unit

type state = Uninitialized | Running of Ocf.client * close | Errored

let state : state ref = ref Uninitialized

let start () =
  let prog = Sys.argv.(1) in
  let argv = [|"ocamlformat-rpc"|] in
  ( match
      let input, output = Unix.open_process_args prog argv in
      let pid = Unix.process_pid (input, output) in
      Ocf.pick_client ~pid input output supported_versions
      >>| fun client ->
      let close =
        match client with
        | `V1 _ ->
            log "[ocf] client V1 selected\n%!" ;
            fun () -> close_out output ; close_in input
        | `V2 _ ->
            log "[ocf] client V2 selected\n%!" ;
            fun () -> close_out output ; close_in input
      in
      state := Running (client, close) ;
      client
    with
  | exception _ ->
      Error
        (`Msg
           "OCamlFormat-RPC did not respond. Check that a compatible \
            version of the OCamlFormat RPC server (ocamlformat-rpc >= \
            0.18.0) is installed." )
  | x -> x )
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
  | Running (cl, _) ->
      let i, _ = Unix.waitpid [WNOHANG] (Ocf.pid cl) in
      if i = 0 then Ok cl else start ()
  | Errored -> Error `No_process

let close_client () =
  match !state with
  | Uninitialized -> ()
  | Running (cl, close) ->
      let i, _ = Unix.waitpid [WNOHANG] (Ocf.pid cl) in
      if i = 0 then close () else ()
  | Errored -> ()

let config c =
  get_client () >>= fun cl -> log "[ocf] Config\n%!" ; Ocf.config c cl

let format x =
  get_client ()
  >>= fun cl ->
  log "[ocf] Format '%s'\n%!" x ;
  Ocf.format x cl

let halt () =
  get_client ()
  >>= fun cl ->
  log "[ocf] Halt\n%!" ;
  Ocf.halt cl
  >>| fun () ->
  close_client () ;
  state := Uninitialized

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
