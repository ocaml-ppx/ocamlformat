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

module Version = struct
  type t = V1

  let to_string = function V1 -> "v1"

  let of_string = function "v1" | "V1" -> Some V1 | _ -> None
end

module Make (IO : IO.S) = struct
  module type Command_S = sig
    type t

    val read_input : IO.ic -> t IO.t

    val to_sexp : t -> Csexp.t

    val output : IO.oc -> t -> unit IO.t
  end

  module type Client_S = sig
    type t

    type cmd

    val pid : t -> int

    val mk : pid:int -> IO.ic -> IO.oc -> t

    val query : cmd -> t -> cmd IO.t

    val halt : t -> (unit, [> `Msg of string]) result IO.t

    val config :
      (string * string) list -> t -> (unit, [> `Msg of string]) result IO.t
  end

  module Init :
    Command_S with type t = [`Halt | `Unknown | `Version of string] = struct
    type t = [`Halt | `Unknown | `Version of string]

    let read_input ic =
      let open IO in
      read ic
      >>= function
      | None -> return `Halt
      | Some (Atom "Halt") -> return `Halt
      | Some (List [Atom "Version"; Atom v]) -> return (`Version v)
      | Some _ -> return `Unknown

    let to_sexp =
      let open Csexp in
      function
      | `Version v -> List [Atom "Version"; Atom v] | _ -> assert false

    let output oc t = IO.write oc [to_sexp t]
  end

  module V1 : sig
    module Command :
      Command_S
        with type t =
          [ `Halt
          | `Unknown
          | `Error of string
          | `Config of (string * string) list
          | `Format of string ]

    module Client : sig
      include Client_S with type cmd = Command.t

      val format : string -> t -> (string, [> `Msg of string]) result IO.t
    end
  end = struct
    module Command = struct
      type t =
        [ `Halt
        | `Unknown
        | `Error of string
        | `Config of (string * string) list
        | `Format of string ]

      let read_input ic =
        let open Csexp in
        let open IO in
        read ic
        >>= function
        | None -> return `Halt
        | Some (List [Atom "Format"; Atom x]) -> return (`Format x)
        | Some (List [Atom "Config"; List l]) ->
            let c =
              List.fold_left
                (fun acc -> function
                  | List [Atom name; Atom value] -> (name, value) :: acc
                  | _ -> acc )
                [] l
              |> List.rev
            in
            return (`Config c)
        | Some (List [Atom "Error"; Atom x]) -> return (`Error x)
        | Some (Atom "Halt") -> return `Halt
        | Some _ -> return `Unknown

      let to_sexp =
        let open Csexp in
        function
        | `Format x -> List [Atom "Format"; Atom x]
        | `Config c ->
            let l =
              List.map (fun (name, value) -> List [Atom name; Atom value]) c
            in
            List [Atom "Config"; List l]
        | `Error x -> List [Atom "Error"; Atom x]
        | `Halt -> Atom "Halt"
        | _ -> assert false

      let output oc t = IO.write oc [to_sexp t]
    end

    module Client = struct
      type t = {pid: int; input: IO.ic; output: IO.oc}

      type cmd = Command.t

      let pid t = t.pid

      let mk ~pid input output = {pid; input; output}

      let query command t =
        let open IO in
        Command.output t.output command
        >>= fun () -> Command.read_input t.input

      let halt t =
        let open IO in
        match Command.output t.output `Halt with
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

  type client = [`V1 of V1.Client.t]

  let get_client ~pid input output x =
    match Version.of_string x with
    | Some V1 -> Ok (`V1 (V1.Client.mk ~pid input output))
    | None -> Error (`Msg "invalid client version")

  let pick_client ~pid ic oc versions =
    let open IO in
    let rec aux = function
      | [] -> return (Error (`Msg "Version negociation failed"))
      | latest :: others -> (
          IO.write oc [Init.to_sexp (`Version latest)]
          >>= fun () ->
          Init.read_input ic
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

  let pid = function `V1 cl -> V1.Client.pid cl

  let halt = function `V1 cl -> V1.Client.halt cl

  let config c = function `V1 cl -> V1.Client.config c cl

  let format x = function `V1 cl -> V1.Client.format x cl
end
