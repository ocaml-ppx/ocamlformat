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

(** Precedence levels of Ast terms. *)
type t =
  | Low
  | Semi  (** [;] *)
  | LessMinus  (** [<-] *)
  | ColonEqual  (** [:=] *)
  | As  (** [as] *)
  | Comma  (** [,] *)
  | MinusGreater  (** [->] *)
  | BarBar  (** [or] and [||] *)
  | AmperAmper  (** [&] and [&&] *)
  | InfixOp0  (** [=…], [<…], [>…], [|…], [&…], [$…] and [!=] *)
  | InfixOp1  (** [@…] and [^…] *)
  | ColonColon  (** [::] *)
  | InfixOp2  (** [+…] and [-…] *)
  | InfixOp3  (** [*…], [/…], [%…], [mod], [land], [lor] and [lxor] *)
  | InfixOp4  (** [**…], [lsl], [lsr] and [asr] *)
  | UMinus  (** [-] and [-.] (prefix) *)
  | Apply
  | HashOp  (** [#…] *)
  | Dot  (** [x.y] and [x#y] *)
  | High
  | Atomic

val compare : t -> t -> int

val equal : t -> t -> bool

val to_string : t -> string
