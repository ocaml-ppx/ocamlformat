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

module Magic = struct
  let int_of_ic : in_channel -> int = Obj.magic

  let ic_of_int : int -> in_channel = Obj.magic

  let int_of_oc : out_channel -> int = Obj.magic

  let oc_of_int : int -> out_channel = Obj.magic
end

module type Command_S = sig
  type t

  val read_input : Stdlib.in_channel -> t

  val to_sexp : t -> Sexp.t

  val output : Stdlib.out_channel -> t -> unit
end

module type Client_S = sig
  type t

  type cmd

  val to_sexp : t -> Sexp.t

  val of_sexp : Sexp.t -> (t, [> `Msg of string]) result

  val pid : t -> int

  val mk : pid:int -> in_channel -> out_channel -> t

  val query : cmd -> t -> cmd

  val halt : t -> (unit, [> `Msg of string]) result

  val config :
    (string * string) list -> t -> (unit, [> `Msg of string]) result

  val format : string -> t -> (string, [> `Msg of string]) result
end

module type V = sig
  module Command : Command_S

  module Client : Client_S with type cmd = Command.t
end

module Csexp = Csexp.Make (Sexp)

module Init :
  Command_S with type t = [`Halt | `Unknown | `Version of string] = struct
  type t = [`Halt | `Unknown | `Version of string]

  let read_input in_channel =
    let open Sexp in
    match Csexp.input in_channel with
    | Ok (Atom "Halt") -> `Halt
    | Ok (List [Atom "Version"; Atom v]) -> `Version v
    | Ok _ -> `Unknown
    | Error _msg -> `Halt

  let to_sexp =
    let open Sexp in
    function
    | `Version v -> List [Atom "Version"; Atom v] | _ -> assert false

  let output channel t =
    to_sexp t |> Csexp.to_channel channel ;
    Stdlib.flush channel
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

    type cmd = Command.t

    let to_sexp {pid; input; output} =
      let open Sexp in
      List
        [ Atom "Pid"
        ; Atom (string_of_int pid)
        ; Atom "Input"
        ; Atom (string_of_int (Magic.int_of_ic input))
        ; Atom "Output"
        ; Atom (string_of_int (Magic.int_of_oc output)) ]

    let of_sexp =
      let open Sexp in
      function
      | List
          [ Atom "Pid"
          ; Atom pid
          ; Atom "Input"
          ; Atom input
          ; Atom "Output"
          ; Atom output ] ->
          let pid = int_of_string pid in
          let input = Magic.ic_of_int (int_of_string input) in
          let output = Magic.oc_of_int (int_of_string output) in
          Ok {pid; input; output}
      | _ -> Error (`Msg "V1.Client.of_sexp: invalid sexp")

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
end

type client = [`V1 of V1.Client.t]

let sexp_of_client =
  let open Sexp in
  function `V1 c -> List [Atom "V1"; V1.Client.to_sexp c]

let client_of_sexp =
  let open Sexp in
  function
  | List [Atom "V1"; c] -> (
    match V1.Client.of_sexp c with Ok c -> Ok (`V1 c) | Error x -> Error x )
  | _ -> Error (`Msg "client_of_sexp: invalid sexp")

let get_client ~pid input output = function
  | "v1" | "V1" -> Some (`V1 (V1.Client.mk ~pid input output))
  | _ -> None

let get_client_exn ~pid input output x =
  match get_client ~pid input output x with
  | Some x -> Ok x
  | None -> failwith "impossible"

let pick_client ~pid input output versions =
  let rec aux = function
    | [] -> Error (`Msg "Version negociation failed")
    | latest :: others -> (
        let version = `Version latest in
        Csexp.to_channel output (Init.to_sexp version) ;
        flush output ;
        match Init.read_input input with
        | `Version v when v = latest -> get_client_exn ~pid input output v
        | `Version v -> (
          match others with
          | h :: _ when v = h -> get_client_exn ~pid input output v
          | _ -> aux others )
        | `Unknown -> aux others
        | `Halt ->
            Error
              (`Msg
                "OCamlFormat-RPC did not respond. Check that a compatible \
                 version of the OCamlFormat RPC server (ocamlformat-rpc >= \
                 0.18.0) is installed." ) )
  in
  aux versions

let pid = function `V1 cl -> V1.Client.pid cl

let halt = function `V1 cl -> V1.Client.halt cl

let config c = function `V1 cl -> V1.Client.config c cl

let format x = function `V1 cl -> V1.Client.format x cl

(** Session management *)

let sessions : (client, int) Hashtbl.t = Hashtbl.create 8

let new_session (t : client) =
  let count = Option.value (Hashtbl.find_opt sessions t) ~default:0 in
  let count = count + 1 in
  Hashtbl.replace sessions t count

let end_session (t : client) =
  let count = Option.value (Hashtbl.find_opt sessions t) ~default:0 in
  let count = count - 1 in
  if count <= 0 then ignore @@ halt t else Hashtbl.replace sessions t count
