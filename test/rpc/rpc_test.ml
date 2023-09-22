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

type close = unit -> unit

type state = Uninitialized | Running of Ocf.client * close | Errored

let state : state ref = ref Uninitialized

let start ?versions () =
  let prog = Sys.argv.(1) in
  let argv = [|"ocamlformat-rpc"|] in
  ( match
      let input, output = Unix.open_process_args prog argv in
      let pid = Unix.process_pid (input, output) in
      let versions =
        match versions with
        | Some v -> List.map Version.to_string v
        | None -> List.map Version.to_string [V2; V1]
      in
      Ocf.pick_client ~pid input output versions
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

let get_client ?versions () =
  match !state with
  | Uninitialized -> start ?versions ()
  | Running (cl, _) ->
      let i, _ = Unix.waitpid [WNOHANG] (Ocf.pid cl) in
      if i = 0 then Ok cl else start ?versions ()
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

let format ?(format_args = empty_args) ?versions x =
  get_client ?versions ()
  >>= fun cl ->
  log "[ocf] Format '%s'\n%!" x ;
  Ocf.format ~format_args x cl

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
  log "Testing v1\n%!" ;
  protect_string @@ format ~versions:[V1] "char -> string" ;
  protect_string @@ format "int -> int" ;
  protect_string @@ format " int    (* foo *) \n\n ->     int  (* bar *)" ;
  protect_unit @@ config [("foo", "bar")] ;
  protect_unit @@ config [("margin", "10")] ;
  protect_string @@ format "aaa -> bbb -> ccc -> ddd -> eee -> fff -> ggg" ;
  protect_unit @@ config [("margin", "80")] ;
  protect_string @@ format "aaa -> bbb -> ccc -> ddd -> eee -> fff -> ggg" ;
  protect_string @@ format "val x :\n \nint" ;
  protect_string @@ format "val write : 'a    t/1 -> 'a ->unit M/2.t" ;
  protect_string @@ format "x + y * z" ;
  protect_string @@ format "let x = 4 in x" ;
  protect_string @@ format "sig end" ;
  protect_string
  @@ format
       "sig\n\n\
       \ val x : foo -> bar\n\
       \  (** this does something *)\n\n\
       \ val f : a -> b -> c ->\n\n\
       \ d     end" ;
  let some_function =
    {|
let ssmap
    :  (module MapT
          with type key = string
           and type data = string
           and type map = SSMap.map )
    -> unit
  =
  ()
|}
  in
  protect_string @@ format some_function ;
  protect_unit @@ config [("profile", "janestreet")] ;
  protect_string @@ format some_function ;
  protect_unit @@ halt ()

let () =
  log "Testing v2\n%!" ;
  (* testing format args *)
  protect_string
  @@ format ~versions:[V2]
       ~format_args:{empty_args with path= Some "small_margin/foo.ml"}
       "aaa -> bbb -> ccc -> ddd -> eee -> fff -> ggg" ;
  protect_string @@ format "aaa -> bbb -> ccc -> ddd -> eee -> fff -> ggg" ;
  protect_string
  @@ format
       ~format_args:{empty_args with config= Some [("margin", "10")]}
       "aaa -> bbb -> ccc -> ddd -> eee -> fff -> ggg" ;
  protect_string @@ format "aaa -> bbb -> ccc -> ddd -> eee -> fff -> ggg" ;
  protect_string
  @@ format
       ~format_args:
         {config= Some [("margin", "80")]; path= Some "small_margin/foo.ml"}
       "aaa -> bbb -> ccc -> ddd -> eee -> fff -> ggg" ;
  protect_string @@ format "aaa -> bbb -> ccc -> ddd -> eee -> fff -> ggg" ;
  protect_unit @@ halt ()
