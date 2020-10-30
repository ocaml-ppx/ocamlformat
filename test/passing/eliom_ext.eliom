let%server log str = Lwt_io.write_line Lwt_io.stdout str

let%client log = ~%(Eliom_client.server_function [%derive.json: string] log)

let%client () =
  Eliom_client.onload
    (* NB The service underlying the server_function isn't available on the
       client before loading the page. *)
    (fun () ->
      Lwt.async (fun () -> log "Hello from the client to the server!") )

[%%shared
type some_type = int * string list [@@deriving json]

type another_type = A of some_type | B of another_type [@@deriving json]]

let%server ( (s : int Eliom_shared.React.S.t)
           , (f : (?step:React.step -> int -> unit) Eliom_shared.Value.t) ) =
  Eliom_shared.React.S.create 0

let%client incr_s () =
  let v = Eliom_shared.React.S.value ~%s in
  ~%f (v + 1)

let%shared msg_of_int i = Printf.sprintf "value is %d" i

let s_as_string () : string Eliom_shared.React.S.t =
  Eliom_shared.React.S.map [%shared msg_of_int] s

let%shared () =
  Eliom_registration.Html.register s (fun () () ->
      Lwt.return
        (Eliom_tools.F.html ~title:"hybrid"
           Html.F.(body [h1 [txt "Salut !"]]) ) )
