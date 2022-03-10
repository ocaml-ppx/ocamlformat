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

module Make (IO : IO.S) : sig
  module type Command_S = sig
    type t

    val read_input : IO.ic -> t IO.t

    val output : IO.oc -> t -> unit IO.t
  end

  (** Version used to set the protocol version *)
  module Init :
    Command_S with type t = [`Halt | `Unknown | `Version of string]

  module V1 :
    Command_S
      with type t =
        [ `Halt
        | `Unknown
        | `Error of string
        | `Config of (string * string) list
        | `Format of string ]

  module V2 :
    Command_S
      with type t =
        [ `Halt
        | `Unknown
        | `Error of string
        | `Format of string * format_args ]
end
