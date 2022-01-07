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

open Sexplib0

module type IO = sig
  type +'a t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  val return : 'a -> 'a t

  type ic

  type oc

  val read_line : ic -> string option t

  val read : ic -> int -> string t

  val write : oc -> string -> unit t

  val flush : oc -> unit t
end

module Make (IO : IO) = struct
  module type Command_S = sig
    type t

    val read_input : IO.ic -> t IO.t

    val to_sexp : t -> Sexp.t

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

    val format : string -> t -> (string, [> `Msg of string]) result IO.t
  end

  module type V = sig
    module Command : Command_S

    module Client : Client_S with type cmd = Command.t
  end

  module Csexp = Csexp.Make (Sexp)

  module Init :
    Command_S with type t = [`Halt | `Unknown | `Version of string] = struct
    type t = [`Halt | `Unknown | `Version of string]

    let read_input ic =
      let open Sexp in
      let open IO in
      read_line ic
      >>= function
      | None -> return `Unknown
      | Some x -> (
        match Csexp.parse_string x with
        | Ok (Atom "Halt") -> return `Halt
        | Ok (List [Atom "Version"; Atom v]) -> return (`Version v)
        | Ok _ -> return `Unknown
        | Error _msg -> return `Halt )

    let to_sexp =
      let open Sexp in
      function
      | `Version v -> List [Atom "Version"; Atom v] | _ -> assert false

    let output oc t =
      let open IO in
      to_sexp t |> Csexp.to_string |> IO.write oc >>= fun () -> IO.flush oc
  end

  module V1 :
    V
      with type Command.t =
        [ `Halt
        | `Unknown
        | `Error of string
        | `Config of (string * string) list
        | `Format of string ] = struct
    module Command = struct
      type t =
        [ `Halt
        | `Unknown
        | `Error of string
        | `Config of (string * string) list
        | `Format of string ]

      let read_input ic =
        let open Sexp in
        let open IO in
        read_line ic
        >>= function
        | None -> return `Unknown
        | Some x -> (
          match Csexp.parse_string x with
          | Ok (List [Atom "Format"; Atom x]) -> return (`Format x)
          | Ok (List [Atom "Config"; List l]) ->
              let c =
                List.fold_left
                  (fun acc -> function
                    | List [Atom name; Atom value] -> (name, value) :: acc
                    | _ -> acc )
                  [] l
                |> List.rev
              in
              return (`Config c)
          | Ok (List [Atom "Error"; Atom x]) -> return (`Error x)
          | Ok (Atom "Halt") -> return `Halt
          | Ok _ -> return `Unknown
          | Error _msg -> return `Halt )

      let to_sexp =
        let open Sexp in
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

      let output oc t =
        let open IO in
        to_sexp t |> Csexp.to_string |> IO.write oc >>= fun () -> IO.flush oc
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

  let get_client ~pid input output = function
    | "v1" | "V1" -> Some (`V1 (V1.Client.mk ~pid input output))
    | _ -> None

  let get_client_exn ~pid ic oc x =
    match get_client ~pid ic oc x with
    | Some x -> Ok x
    | None -> failwith "impossible"

  let pick_client ~pid ic oc versions =
    let open IO in
    let rec aux = function
      | [] -> return (Error (`Msg "Version negociation failed"))
      | latest :: others -> (
          let version = `Version latest in
          Init.to_sexp version |> Csexp.to_string |> IO.write oc
          >>= fun () ->
          IO.flush oc
          >>= fun () ->
          Init.read_input ic
          >>= function
          | `Version v when v = latest ->
              return (get_client_exn ~pid ic oc v)
          | `Version v -> (
            match others with
            | h :: _ when v = h -> return (get_client_exn ~pid ic oc v)
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
