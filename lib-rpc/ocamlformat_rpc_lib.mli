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

(** The [IO] module defines the blocking interface for reading and writing to
    Cohttp streams *)
module type IO = sig
  (** ['a t] represents a blocking monad state *)
  type +'a t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  (** [a >>= b] will pass the result of [a] to the [b] function. This is a
      monadic [bind]. *)

  val return : 'a -> 'a t
  (** [return a] will construct a constant IO value. *)

  (** [ic] represents an input channel *)
  type ic

  (** [oc] represents an output channel *)
  type oc

  val read_line : ic -> string option t
  (** [read_line ic] will read a single line terminated by CR or CRLF from
      the input channel [ic]. It returns {!None} if EOF or other error
      condition is reached. *)

  val read : ic -> int -> string t
  (** [read ic len] will block until a maximum of [len] characters are read
      from the input channel [ic]. It returns an empty string if EOF or some
      other error condition occurs on the input channel, and can also return
      fewer than [len] characters if input buffering is not sufficient to
      satisfy the request. *)

  val write : oc -> string -> unit t
  (** [write oc s] will block until the complete [s] string is written to the
      output channel [oc]. *)

  val flush : oc -> unit t
  (** [flush oc] will return when all previously buffered content from
      calling {!write} have been written to the output channel [oc]. *)
end

module Make (IO : IO) : sig
  module type Command_S = sig
    type t

    val read_input : IO.ic -> t IO.t

    val to_sexp : t -> Sexplib0.Sexp.t

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

  val config :
       (string * string) list
    -> client
    -> (unit, [> `Msg of string]) result IO.t

  val format : string -> client -> (string, [> `Msg of string]) result IO.t
end
