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

module Make (IO : IO.S) : sig
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
    (** The caller must close the input and output channels after calling
        [halt]. *)

    val config :
      (string * string) list -> t -> (unit, [> `Msg of string]) result IO.t

    val format : string -> t -> (string, [> `Msg of string]) result IO.t
  end

  module type V = sig
    module Command : Command_S

    module Client : Client_S with type cmd = Command.t
  end

  (** Version used to set the protocol version *)
  module Init :
    Command_S with type t = [`Halt | `Unknown | `Version of string]

  module V1 :
    V
      with type Command.t =
        [ `Halt
        | `Unknown
        | `Error of string
        | `Config of (string * string) list
        | `Format of string ]

  type client = [`V1 of V1.Client.t]

  val pick_client :
       pid:int
    -> IO.ic
    -> IO.oc
    -> string list
    -> (client, [`Msg of string]) result IO.t
  (** [pick_client ~pid in out versions] returns the most-fitting client
      according to a list of [versions], that is a list ordered from the most
      to the least wished version. *)

  val pid : client -> int

  val halt : client -> (unit, [> `Msg of string]) result IO.t
  (** The caller must close the input and output channels after calling
      [halt]. *)

  val config :
       (string * string) list
    -> client
    -> (unit, [> `Msg of string]) result IO.t

  val format : string -> client -> (string, [> `Msg of string]) result IO.t
end
