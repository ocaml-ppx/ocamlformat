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

(** OCamlformat RPC API.

    This module defines the commands exchanged between the server and the
    client, and the way to build RPC clients. *)

type format_args =
  { path: string option
        (** Path used for the current formatting request, this allows
            ocamlformat to determine which .ocamlformat files must be
            applied. *)
  ; config: (string * string) list option
        (** Additional options used for the current formatting request. *) }

val empty_args : format_args

module Version : sig
  type t = V1 | V2

  val to_string : t -> string

  val of_string : string -> t option
end

(** The whole API is functorized over an [IO] module defining the blocking
    interface for reading and writing the data.

    After you decided on the [IO] implementation, the [Ocamlformat_rpc_lib]
    API can then be instantiated:

    {[
      module RPC = Ocamlformat_rpc_lib.Make (IO)
    ]} *)

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
  end

  (** Version used to set the protocol version *)
  module Init :
    Command_S with type t = [`Halt | `Unknown | `Version of string]

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

      val config :
        (string * string) list -> t -> (unit, [> `Msg of string]) result IO.t

      val format : string -> t -> (string, [> `Msg of string]) result IO.t
    end
  end

  module V2 : sig
    module Command :
      Command_S
        with type t =
          [ `Halt
          | `Unknown
          | `Error of string
          | `Format of string * format_args ]

    module Client : sig
      include Client_S with type cmd = Command.t

      val format :
           format_args:format_args
             (** [format_args] modifies the server's configuration
                 temporarily, for the current request. *)
        -> string
        -> t
        -> (string, [> `Msg of string]) result IO.t
    end
  end

  type client = [`V1 of V1.Client.t | `V2 of V2.Client.t]

  val pick_client :
       pid:int
    -> IO.ic
    -> IO.oc
    -> string list
    -> (client, [`Msg of string]) result IO.t
  (** The RPC offers multiple versions of the API.
      [pick_client ~pid in out versions] handles the interaction with the
      server to agree with a common version of the RPC to use. Trying
      successively each version of the provided list [versions] until the
      server agrees, for this reason you might want to list newer versions
      before older versions. The given [IO.ic] and [IO.oc] values are
      referenced by the [client] value and must be kept open until [halt] is
      called. *)

  val pid : client -> int

  val halt : client -> (unit, [> `Msg of string]) result IO.t
  (** Tell the server to close the connection. No more commands can be sent
      using the same [client] value. *)

  val config :
       (string * string) list
    -> client
    -> (unit, [> `Msg of string]) result IO.t
  (** @before v2 *)

  val format :
       ?format_args:format_args
         (** [format_args] modifies the server's configuration temporarily,
             for the current request.

             @since v2 *)
    -> string
    -> client
    -> (string, [> `Msg of string]) result IO.t
  (** When using v1, [format_args] will be ignored. *)

  (** A basic interaction could be:

      {[
        RPC.config [("profile", "ocamlformat")] client >>= fun () ->
        RPC.format "let x = 4 in x" client >>= fun formatted ->
        ...
        RPC.halt client >>= fun () ->
        ...
      ]} *)
end

(** For a basic working example, see: {{:../../../../test/rpc/rpc_test.ml}
    test/rpc/rpc_test.ml}. *)
