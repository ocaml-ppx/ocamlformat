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

type state = Uninitialized | Running of Ocf.client | Errored

let state : state ref = ref Uninitialized

let start () =
  let prog = Sys.argv.(1) in
  let argv = [|"ocamlformat-rpc"|] in
  let input, output = Unix.open_process_args prog argv in
  let pid = Unix.process_pid (input, output) in
  log "[ocf] proposed versions: @[<hv>%a@]\n%!"
    (Format.pp_print_list
       ~pp_sep:(fun fs () -> Format.fprintf fs ",@ ")
       Format.pp_print_string )
    supported_versions ;
  Ocf.pick_client ~pid input output supported_versions
  >>| (fun client ->
        (match client with `V1 _ -> log "[ocf] client V1 selected\n%!") ;
        state := Running client ;
        client )
  |> Result.map_error ~f:(fun (`Msg msg) ->
         state := Errored ;
         log
           "An error occured while initializing and configuring ocamlformat:\n\
            %s"
           msg ;
         `No_process )

let get_client () =
  match !state with
  | Uninitialized -> start ()
  | Running cl ->
      let i, _ = Unix.waitpid [WNOHANG] (Ocf.pid cl) in
      if i = 0 then Ok cl else start ()
  | Errored -> Error `No_process

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
  Ocf.halt cl >>| fun () -> state := Uninitialized

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
  protect_string @@ format "char -> string" ;
  protect_string @@ format "int -> int" ;
  protect_string @@ format " int    (* foo *) \n\n ->     int  (* bar *)" ;
  protect_unit @@ config [("foo", "bar")] ;
  protect_unit @@ config [("margin", "10")] ;
  protect_string @@ format "aaa -> bbb -> ccc -> ddd -> eee -> fff -> ggg" ;
  protect_unit @@ config [("margin", "80")] ;
  protect_string @@ format "aaa -> bbb -> ccc -> ddd -> eee -> fff -> ggg" ;
  protect_string @@ format "val x :\n \nint" ;
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
