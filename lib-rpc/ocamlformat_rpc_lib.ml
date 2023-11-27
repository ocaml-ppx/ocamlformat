(**************************************************************************)
(*                                                                        *)
(*                              OCamlFormat                               *)
(*                                                                        *)
(*            Copyright (c) Facebook, Inc. and its affiliates.            *)
(*                                                                        *)
(*      This source code is licensed under the MIT license found in       *)
(*      the LICENSE file in the root directory of this source tree.       *)
(*                                                                        *)
(**************************************************************************)

type format_args = Protocol.format_args =
  {path: string option; config: (string * string) list option}

let empty_args = Protocol.empty_args

module Version = Protocol.Version

module type IO = IO.S

module Protocol = Protocol

module Make (IO : IO) = struct
  module Protocol = Protocol.Make (IO)

  module V1 = struct
    module Client = struct
      type t = {pid: int; input: IO.ic; output: IO.oc}

      let pid t = t.pid

      let mk ~pid input output = {pid; input; output}

      let query command t =
        let open IO in
        Protocol.V1.output t.output command
        >>= fun () -> Protocol.V1.read_input t.input

      let halt t =
        let open IO in
        match Protocol.V1.output t.output `Halt with
        | exception _ ->
            return (Error (`Msg "failing to close connection to server"))
        | (_ : unit IO.t) -> return (Ok ())

      let config c t =
        let open IO in
        query (`Config c) t
        >>= function
        | `Config _ -> return (Ok ())
        | `Error msg -> return (Error (`Msg msg))
        | _ ->
            return
              (Error (`Msg "failing to set configuration: unknown error"))

      let format x t =
        let open IO in
        query (`Format x) t
        >>= function
        | `Format x -> return (Ok x)
        | `Error msg -> return (Error (`Msg msg))
        | _ -> return (Error (`Msg "failing to format input: unknown error"))
    end
  end

  module V2 = struct
    module Client = struct
      type t = {pid: int; input: IO.ic; output: IO.oc}

      let pid t = t.pid

      let mk ~pid input output = {pid; input; output}

      let query command t =
        let open IO in
        Protocol.V2.output t.output command
        >>= fun () -> Protocol.V2.read_input t.input

      let halt t =
        let open IO in
        match Protocol.V2.output t.output `Halt with
        | exception _ ->
            return (Error (`Msg "failing to close connection to server"))
        | (_ : unit IO.t) -> return (Ok ())

      let format ~format_args x t =
        let open IO in
        query (`Format (x, format_args)) t
        >>= function
        | `Format (x, _args) -> return (Ok x)
        | `Error msg -> return (Error (`Msg msg))
        | _ -> return (Error (`Msg "failing to format input: unknown error"))
    end
  end

  type client = [`V1 of V1.Client.t | `V2 of V2.Client.t]

  let get_client ~pid input output x =
    match Version.of_string x with
    | Some V1 -> Ok (`V1 (V1.Client.mk ~pid input output))
    | Some V2 -> Ok (`V2 (V2.Client.mk ~pid input output))
    | None -> Error (`Msg "invalid client version")

  let pick_client ~pid ic oc versions =
    let open IO in
    let rec aux = function
      | [] -> return (Error (`Msg "Version negociation failed"))
      | latest :: others -> (
          Protocol.Init.output oc (`Version latest)
          >>= fun () ->
          Protocol.Init.read_input ic
          >>= function
          | `Version v when v = latest -> return (get_client ~pid ic oc v)
          | `Version v -> (
            match others with
            | h :: _ when v = h -> return (get_client ~pid ic oc v)
            | _ -> aux others )
          | `Unknown -> aux others
          | `Halt ->
              return
                (Error
                   (`Msg
                      "OCamlFormat-RPC did not respond. Check that a \
                       compatible version of the OCamlFormat RPC server \
                       (ocamlformat-rpc >= 0.18.0) is installed." ) ) )
    in
    aux versions

  let pid = function
    | `V1 cl -> V1.Client.pid cl
    | `V2 cl -> V2.Client.pid cl

  let halt = function
    | `V1 cl -> V1.Client.halt cl
    | `V2 cl -> V2.Client.halt cl

  let config c = function
    | `V1 cl -> V1.Client.config c cl
    | `V2 _ ->
        IO.return
          (Error
             (`Msg "'Config' command not implemented in ocamlformat-rpc V2")
          )

  let format ?(format_args = empty_args) x = function
    | `V1 cl -> V1.Client.format x cl
    | `V2 cl -> V2.Client.format ~format_args x cl
end
