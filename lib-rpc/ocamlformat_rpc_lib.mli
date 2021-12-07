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

module type Command_S = sig
  type t

  val read_input : Stdlib.in_channel -> (t, [`Msg of string]) result

  val to_sexp : t -> Sexplib0.Sexp.t

  val output : Stdlib.out_channel -> t -> unit
end

module type Client_S = sig
  type t

  type cmd

  val pid : t -> int

  val mk : pid:int -> in_channel -> out_channel -> t

  val query : cmd -> t -> (cmd, [`Msg of string]) result

  val halt : t -> (unit, [> `Msg of string]) result

  val config :
    (string * string) list -> t -> (unit, [> `Msg of string]) result

  val format : string -> t -> (string, [> `Msg of string]) result
end

module type V = sig
  module Command : Command_S

  module Client : Client_S with type cmd = Command.t
end

module V1 :
  V
    with type Command.t =
      [ `Halt
      | `Unknown
      | `Error of string
      | `Config of (string * string) list
      | `Format of string ]
