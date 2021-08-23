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
module Csexp = Csexp.Make (Sexp)

module Command = struct
  type t =
    [ `Halt
    | `Unknown
    | `Error of string
    | `Config of (string * string) list
    | `Format of string ]

  let read_input in_channel =
    let open Sexp in
    match Csexp.input in_channel with
    | Ok (List [Atom "Format"; Atom x]) -> `Format x
    | Ok (List [Atom "Config"; List l]) ->
        let c =
          List.fold_left
            (fun acc -> function
              | List [Atom name; Atom value] -> (name, value) :: acc
              | _ -> acc )
            [] l
          |> List.rev
        in
        `Config c
    | Ok (List [Atom "Error"; Atom x]) -> `Error x
    | Ok (Atom "Halt") -> `Halt
    | Ok _ -> `Unknown
    | Error _msg -> `Halt

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

  let output channel t =
    to_sexp t |> Csexp.to_channel channel ;
    Stdlib.flush channel
end

module Client = struct
  type t = {pid: int; input: in_channel; output: out_channel}

  let pid t = t.pid

  let mk ~pid input output = {pid; input; output}

  let query command t =
    Command.output t.output command ;
    Command.read_input t.input

  let halt t =
    match
      Command.output t.output `Halt ;
      close_in t.input ;
      close_out t.output
    with
    | exception _ -> Error (`Msg "failing to close connection to server")
    | () -> Ok ()

  let config c t =
    match query (`Config c) t with
    | `Config _ -> Ok ()
    | `Error msg -> Error (`Msg msg)
    | _ -> Error (`Msg "failing to set configuration: unknown error")

  let format x t =
    match query (`Format x) t with
    | `Format x -> Ok x
    | `Error msg -> Error (`Msg msg)
    | _ -> Error (`Msg "failing to format input: unknown error")
end
