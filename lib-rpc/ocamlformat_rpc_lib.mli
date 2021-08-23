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

module Command : sig
  type t =
    [ `Halt
    | `Unknown
    | `Error of string
    | `Config of (string * string) list
    | `Format of string ]

  val read_input : Stdlib.in_channel -> t

  val to_sexp : t -> Sexplib0.Sexp.t

  val output : Stdlib.out_channel -> t -> unit
end

module Client : sig
  type t

  val pid : t -> int

  val mk : pid:int -> in_channel -> out_channel -> t

  val query : Command.t -> t -> Command.t

  val halt : t -> (unit, [> `Msg of string]) result

  val config :
    (string * string) list -> t -> (unit, [> `Msg of string]) result

  val format : string -> t -> (string, [> `Msg of string]) result
end
